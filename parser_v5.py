from __future__ import annotations

# --- stdlib ---
from dataclasses import dataclass, field
from pathlib import Path
import argparse, csv, re, html, sys, logging
from typing import List, Optional, Tuple

# --- third-party ---
# Install with: pip install beautifulsoup4 lxml
from bs4 import BeautifulSoup, Tag

# ---------- Logging ----------
logging.basicConfig(
    level=logging.INFO,
    format="%(levelname)s:%(name)s:%(message)s",
    stream=sys.stderr,
)
log = logging.getLogger("eps_parser")

# ---------- Regexes ----------
NUMBER_RE = re.compile(
    r'(?<![A-Za-z])(?:\$)?\(?\s*[+-]?(?:\d{1,3}(?:,\d{3})*|\d+)(?:\.\d+)?\s*\)?(?![A-Za-z])'
)
ADJUSTED_RE = re.compile(r'\b(adjust(ed)?|non[-\s]?gaap|pro\s*forma|core\s+earnings?)\b', re.I)
BASIC_RE    = re.compile(r'\bbasic\b', re.I)
DILUTED_RE  = re.compile(r'\bdilut(?:ed|ion)\b', re.I)
EPS_HEADER_RE  = re.compile(r'\b(earnings|income|loss)\s+per\s+share\b', re.I)

# Important: broaden EPS loss cues, but keep them EPS-specific
LOSS_PHRASE_RE = re.compile(r'\bloss\s+per\s+share\b|\bnet\s+loss\b', re.I)

PER_SHARE_RE   = re.compile(
    r'\b((?:earnings|income|loss)\s+per\s+share|per\s+(?:basic|diluted)\s+share)\b', re.I
)
CURRENCY_RE    = re.compile(r'\$\s*')
STRICT_EPS_PHRASE_RE = re.compile(r'(earnings|income|loss)\s+per\s+share\b', re.I)
EPS_TOKEN_RE   = re.compile(r'\bE\.?P\.?S\.?\b|\bEPS\b', re.I)

# Context guardians
ANNUAL_CONTEXT_RE = re.compile(r'\b(full\s+year|fiscal\s+year|annual|twelve\s+months|12[-\s]*months)\b', re.I)
GUIDANCE_RE       = re.compile(r'\b(guidance|outlook|range|target(?:ed)?|expects?)\b', re.I)
QUARTER_RE        = re.compile(r'\b(three\s+months|quarter(?:ly)?|q[1-4])\b', re.I)
REPORTED_RE       = re.compile(r'\b(reported|gaap)\b', re.I)

# Bank phrasing that must NOT flip sign:
NON_EPS_LOSS_RE   = re.compile(
    r'\b(loan\s+loss(?:es)?|loss\s+provision|provision\s+for\s+loan\s+loss(?:es)?|'
    r'allowance\s+for\s+loan\s+loss(?:es)?)\b',
    re.I
)

# Block rows that are almost certainly not EPS
BLOCK_ROW_RE = re.compile(
    r"(weighted[-\s]?average\s+shares|average\s+shares|shares\s+outstanding|common\s+shares|"
    r"basic\s+and\s+diluted\s+shares|revenue|net\s+sales|sales|ebitda|adjusted\s+ebitda|cash|cash\s+flow|"
    r"segment|operating\s+margin|gross\s+margin|percent|%|dividends?|cash\s+dividends?|"
    r"book\s+value|net\s+asset\s+value|nav\b)",
    re.I
)

# Hard magnitude cap for EPS; anything above this is discarded
HARD_ABS_CAP = 20.0

# ---------- Numeric normalization ----------
def normalize_num_token(tok: str, loss_ctx: bool) -> Optional[float]:
    neg = tok.strip().startswith('(') and tok.strip().endswith(')')
    tok = tok.strip().strip('()').replace(',', '')
    tok = CURRENCY_RE.sub('', tok)
    try:
        v = float(tok)
    except Exception:
        return None
    if neg:
        v = -abs(v)
    if loss_ctx and v > 0:
        v = -v
    return v

def plausibility_penalty(v: float) -> float:
    av = abs(v)
    if av <= 20: 
        return 0.0
    if av <= 50:
        return -3.0
    if av <= 100: 
        return -6.0
    return -10.0

def decimal_bonus(tok: str) -> float:
    return 0.5 if '.' in tok else 0.0

def integer_penalty(tok: str, val: float) -> float:
    if '.' in tok: return 0.0
    return -2.0 if abs(val) >= 10 else -0.5

# ---------- Soup/table helpers ----------
def _is_hidden(el: Tag) -> bool:
    if el.has_attr("hidden"):
        return True
    style = (el.get("style") or "").lower()
    if "display:none" in style or "visibility:hidden" in style or "mso-hide:all" in style:
        return True
    if re.search(r'width\s*:\s*0(px|)\b', style):
        return True
    return False

def _cell_text(td: Tag) -> str:
    text = td.get_text(separator=' ', strip=True)
    text = html.unescape(re.sub(r'\s+', ' ', text))
    return text

@dataclass
class Table:
    rows: List[List[str]] = field(default_factory=list)

def _expand_cells_with_spans(table: Tag) -> Table:
    out = Table()
    spans: dict[int, Tuple[str, int]] = {}

    for tr in table.find_all('tr'):
        row: List[Optional[str]] = []
        col = 0

        def next_free_col(c: int) -> int:
            while c < len(row) and row[c] is not None:
                c += 1
            return c

        # carry rowspans down
        if spans:
            max_col = max(spans.keys())
            if len(row) <= max_col:
                row.extend([None] * (max_col + 1 - len(row)))
            for c, (txt, remaining) in list(spans.items()):
                row[c] = txt
                if remaining <= 1:
                    spans.pop(c, None)
                else:
                    spans[c] = (txt, remaining - 1)

        tds = [td for td in tr.find_all(['td', 'th'], recursive=False) if not _is_hidden(td)]
        for td in tds:
            txt = _cell_text(td)
            colspan = int(td.get('colspan') or 1)
            rowspan = int(td.get('rowspan') or 1)

            col = next_free_col(col)
            if col >= len(row):
                row.extend([None] * (col + 1 - len(row)))

            for k in range(colspan):
                idx = col + k
                if idx >= len(row):
                    row.extend([None] * (idx + 1 - len(row)))
                row[idx] = txt
                if rowspan > 1:
                    spans[idx] = (txt, rowspan - 1)
            col += colspan

        while row and row[-1] is None:
            row.pop()

        row_texts = [c if c is not None else "" for c in row]
        if any(cell.strip() for cell in row_texts):
            out.rows.append(row_texts)

    return out

def _soup_tables(html_bytes: bytes) -> List[Table]:
    s = html_bytes.decode('utf-8', errors='ignore')
    s = re.sub(r'(?is)<!--.*?-->', ' ', s)
    soup = None
    for parser in ('lxml', 'html.parser'):
        try:
            soup = BeautifulSoup(s, parser)
            break
        except Exception as e:
            log.warning("Parser %s failed: %s", parser, e)
    if soup is None:
        return []

    tables = []
    for t in soup.find_all('table'):
        try:
            tables.append(_expand_cells_with_spans(t))
        except Exception as e:
            log.debug("Failed to normalize one table: %s", e)
            simple = Table()
            for tr in t.find_all('tr'):
                row = []
                for td in tr.find_all(['td', 'th']):
                    if _is_hidden(td):
                        continue
                    row.append(_cell_text(td))
                if any(c.strip() for c in row):
                    simple.rows.append(row)
            tables.append(simple)
    return tables

# ---------- Header & scoring helpers ----------
def row_is_header_like(r: List[str]) -> bool:
    non_num = sum(1 for c in r if not NUMBER_RE.search(c))
    return non_num >= max(1, int(0.6*len(r)))

def header_context_score(col_index: int, header_rows: List[List[str]]) -> float:
    if not header_rows:
        return 0.0
    score = 0.0
    cells = []
    for r in header_rows[:3]:
        if 0 <= col_index < len(r):
            cells.append(r[col_index].lower())
    cell_text = ' '.join(cells)
    if ('three months ended' in cell_text or 'quarter ended' in cell_text
        or 'three-month' in cell_text or '3-month' in cell_text):
        score += 8.0
    if ('twelve months ended' in cell_text or 'year ended' in cell_text
        or '12 months' in cell_text):
        score -= 3.0
    year_re = re.compile(r'\b(19\d{2}|20\d{2})\b')
    years = [int(m.group(0)) for m in year_re.finditer(cell_text)]
    if years:
        score += (max(years) - 1990) / 5.0
    score -= 0.2 * col_index
    return score

def pick_best_quarter_column(header_rows: List[List[str]]) -> Optional[int]:
    if not header_rows:
        return None
    max_cols = max(len(r) for r in header_rows)
    best = (float('-inf'), None)
    year_re = re.compile(r'\b(19\d{2}|20\d{2})\b')
    for j in range(max_cols):
        cells = []
        for r in header_rows[:4]:
            if 0 <= j < len(r):
                cells.append(r[j].lower())
        t = ' '.join(cells)
        if not t.strip():
            continue
        score = 0.0
        if ('three months ended' in t or 'quarter ended' in t or 'three-month' in t or '3-month' in t):
            score += 12.0
        if ('twelve months ended' in t or 'year ended' in t or '12 months' in t):
            score -= 8.0
        years = [int(m.group(0)) for m in year_re.finditer(t)]
        if years:
            score += (max(years) - 1990) / 2.0
        score -= 0.15 * j
        if score > best[0]:
            best = (score, j)
    return best[1]

def maybe_merge_paren_token(tok: str, row: List[str], ci: int) -> str:
    tok_s = tok.strip()
    if tok_s.startswith('(') and not tok_s.endswith(')'):
        if ci+1 < len(row) and row[ci+1].strip().startswith(')'):
            return tok_s + ')'
    prev = row[ci-1].strip() if ci-1 >= 0 else ''
    nxt  = row[ci+1].strip() if ci+1 < len(row) else ''
    if not tok_s.startswith('('):
        prev_has_lp = prev in ('(', '$(', '($') or prev.endswith('(')
        nxt_has_rp  = nxt.startswith(')')
        if prev_has_lp and (tok_s.endswith(')') or nxt_has_rp):
            core = tok_s.rstrip(')')
            return f'({core})'
    return tok

def row_blocklisted(rr: List[str]) -> bool:
    return bool(BLOCK_ROW_RE.search(' '.join(rr)))

# ---------- Extraction from tables ----------
def extract_from_table(rows: List[List[str]]) -> Tuple[Optional[float], float]:
    if not rows: return (None, float('-inf'))
    header_rows = [r for r in rows[:5] if row_is_header_like(r)]
    best_col = pick_best_quarter_column(header_rows)
    eps_rows = [i for i,r in enumerate(rows) if EPS_HEADER_RE.search(' '.join(r))]
    candidates: List[Tuple[float,float]] = []

    # EPS block and nearby Basic/Diluted rows
    if eps_rows:
        for idx in eps_rows:
            band = rows[idx: min(len(rows), idx+4)]
            band_loss = bool(LOSS_PHRASE_RE.search(' '.join(rows[idx]).lower()))
            for rr in band:
                if row_blocklisted(rr):
                    continue
                label = ' '.join(rr).lower()
                is_loss = band_loss or bool(LOSS_PHRASE_RE.search(label))
                base = 0.0
                if ADJUSTED_RE.search(label): 
                    base -= 8.0  # stronger anti-adjusted
                if BASIC_RE.search(label):    
                    base += 4.0
                if DILUTED_RE.search(label):  
                    base += 2.0
                if not (BASIC_RE.search(label) or DILUTED_RE.search(label) or EPS_HEADER_RE.search(label)):
                    base -= 0.5
                for ci, cell in enumerate(rr):
                    if '%' in cell:
                        continue
                    m = NUMBER_RE.search(cell)
                    if not m:
                        continue
                    if best_col is None and header_rows:
                        hdr_bits = []
                        for hr in header_rows[:3]:
                            if ci < len(hr):
                                hdr_bits.append(hr[ci].lower())
                        col_hdr_text = ' '.join(hdr_bits)
                        if ('twelve months ended' in col_hdr_text or
                            'year ended' in col_hdr_text or
                            '12 months' in col_hdr_text):
                            continue
                    if best_col is not None and (ci not in (best_col, best_col + 1)):
                        continue
                    tok = maybe_merge_paren_token(m.group(0), rr, ci)
                    val = normalize_num_token(tok, is_loss)
                    if val is None:
                        continue
                    if abs(val) > HARD_ABS_CAP:
                        continue
                    if abs(val) >= 5.0 and not (BASIC_RE.search(label) or DILUTED_RE.search(label)):
                        continue
                    hscore = header_context_score(ci, header_rows)
                    score = base + hscore + decimal_bonus(tok) + plausibility_penalty(val) + integer_penalty(tok, val)
                    candidates.append((score, val))

    # Fallback: explicit Basic/Diluted rows anywhere
    if not candidates:
        for rr in rows:
            if row_blocklisted(rr):
                continue
            label = ' '.join(rr).lower()
            if BASIC_RE.search(label) or DILUTED_RE.search(label):
                is_loss = bool(LOSS_PHRASE_RE.search(label))
                base = 0.0
                if ADJUSTED_RE.search(label): 
                    base -= 8.0
                if BASIC_RE.search(label):    
                    base += 2.0
                if DILUTED_RE.search(label):  
                    base += 3.0
                for ci, cell in enumerate(rr):
                    if '%' in cell:
                        continue
                    m = NUMBER_RE.search(cell)
                    if not m:
                        continue
                    if best_col is None and header_rows:
                        hdr_bits = []
                        for hr in header_rows[:3]:
                            if ci < len(hr):
                                hdr_bits.append(hr[ci].lower())
                        col_hdr_text = ' '.join(hdr_bits)
                        if ('twelve months ended' in col_hdr_text or
                            'year ended' in col_hdr_text or
                            '12 months' in col_hdr_text):
                            continue
                    if best_col is not None and (ci not in (best_col, best_col + 1)):
                        continue
                    tok = maybe_merge_paren_token(m.group(0), rr, ci)
                    val = normalize_num_token(tok, is_loss)
                    if val is None:
                        continue
                    if abs(val) > HARD_ABS_CAP:
                        continue
                    hscore = header_context_score(ci, header_rows)
                    score = base + hscore + decimal_bonus(tok) + plausibility_penalty(val) + integer_penalty(tok, val)
                    candidates.append((score, val))

    if candidates:
        best = sorted(candidates, key=lambda x: x[0])[-1]
        return (best[1], best[0])
    return (None, float('-inf'))

# ---------- Narrative helpers ----------
def strip_text_bs4(html_bytes: bytes) -> str:
    s = html_bytes.decode('utf-8', errors='ignore')
    s = re.sub(r'(?is)<!--.*?-->', ' ', s)
    try:
        soup = BeautifulSoup(s, 'lxml')
    except Exception:
        soup = BeautifulSoup(s, 'html.parser')
    for bad in soup(['script','style']):
        bad.decompose()
    t = soup.get_text(separator=' ', strip=True)
    t = html.unescape(re.sub(r'\s+', ' ', t))
    return t

def _compute_ctx_flags(txt: str) -> Tuple[bool, bool, bool, bool, bool]:
    """Return flags: (has_quarter, has_annual, has_adjusted, has_reported, has_guidance)"""
    low = txt.lower()
    return (
        bool(QUARTER_RE.search(low)),
        bool(ANNUAL_CONTEXT_RE.search(low)),
        bool(ADJUSTED_RE.search(low)),
        bool(REPORTED_RE.search(low)),
        bool(GUIDANCE_RE.search(low)),
    )

def _is_loss_ctx(txt: str) -> bool:
    low = txt.lower()
    # EPS-specific loss phrases
    if LOSS_PHRASE_RE.search(low):
        return True
    # Avoid false flips from bank "loan loss" phrasing etc.
    if NON_EPS_LOSS_RE.search(low):
        return False
    # Plain word "loss" alone is too noisy; don't flip on that
    return False

def extract_from_narrative_strict(text: str) -> Optional[float]:
    best = None
    for m in STRICT_EPS_PHRASE_RE.finditer(text):
        start = max(0, m.start()-160); end = min(len(text), m.end()+180)
        window = text[start:end]
        after = text[m.end(): min(len(text), m.end()+160)]
        before = text[max(0, m.start()-160): m.start()]
        mnum = NUMBER_RE.search(after) or NUMBER_RE.search(window)
        if not mnum:
            continue
        tok = mnum.group(0)
        if '.' not in tok:
            continue

        has_qtr, has_ann, has_adj, has_rep, has_guid = _compute_ctx_flags(before + " " + after)
        # If clearly annual or guidance near the number, skip entirely
        if has_ann:
            continue
        if has_guid:
            continue

        is_loss = _is_loss_ctx(before + " " + after)
        val = normalize_num_token(tok, is_loss)
        if val is None or abs(val) > 20:
            continue

        base = 0.0
        if has_adj: 
            base -= 6.0
        if BASIC_RE.search((before+after).lower()): 
            base += 0.5
        if DILUTED_RE.search((before+after).lower()): 
            base += 1.0
        if has_rep: 
            base += 0.75
        if has_qtr: 
            base += 1.0

        score = base + decimal_bonus(tok) + plausibility_penalty(val) + integer_penalty(tok, val)
        if (best is None) or (score > best[0]):
            best = (score, val)
    return None if best is None else best[1]

def extract_from_text_eps_token(text: str) -> Optional[float]:
    cands: List[Tuple[float, float]] = []
    for m in EPS_TOKEN_RE.finditer(text):
        start = max(0, m.start()-160); end = min(len(text), m.end()+180)
        window = text[start:end]
        low = window.lower()
        after = text[m.end(): min(len(text), m.end()+160)]
        before = text[max(0, m.start()-160): m.start()]

        has_qtr, has_ann, has_adj, has_rep, has_guid = _compute_ctx_flags(window)
        # Skip annual/guidance windows — typical press release headers like “Full Year Adjusted EPS”
        if has_ann:
            continue
        if has_guid:
            continue

        base = 0.0
        if has_qtr: 
            base += 2.0
        if has_adj: 
            base -= 8.0
        if has_rep: 
            base += 0.75
        if BASIC_RE.search(low): 
            base += 0.5
        if DILUTED_RE.search(low): 
            base += 0.75

        is_loss = _is_loss_ctx(window)

        mnum = NUMBER_RE.search(after) or NUMBER_RE.search(window)
        if not mnum:
            continue
        tok = mnum.group(0)
        if '.' not in tok:
            continue
        val = normalize_num_token(tok, is_loss)
        if val is None or abs(val) > 20:
            continue

        score = base + decimal_bonus(tok) + plausibility_penalty(val) + integer_penalty(tok, val)
        cands.append((score, val))
    if not cands:
        return None
    cands.sort(key=lambda x: x[0])
    return cands[-1][1]

def extract_from_text(text: str) -> Optional[float]:
    cands: List[Tuple[float, float]] = []
    for m in PER_SHARE_RE.finditer(text):
        start = max(0, m.start()-160); end = min(len(text), m.end()+180)
        window = text[start:end]
        after  = text[m.end(): min(len(text), m.end()+160)]
        before = text[max(0, m.start()-160): m.start()]

        # prefer number after; else nearest before; else any in window
        mnum = NUMBER_RE.search(after)
        if not mnum:
            prev = list(NUMBER_RE.finditer(before))
            if prev:
                mnum = prev[-1]
        if not mnum:
            mnum = NUMBER_RE.search(window)
        if not mnum:
            continue

        tok = mnum.group(0)
        ctx = (before + " " + after)

        has_qtr, has_ann, has_adj, has_rep, has_guid = _compute_ctx_flags(ctx)
        # Skip annual/guidance
        if has_ann:
            continue
        if has_guid:
            continue

        base = 0.0
        if has_adj: 
            base -= 8.0
        if has_rep: 
            base += 0.75
        if BASIC_RE.search(ctx.lower()):   
            base += 2.0
        if DILUTED_RE.search(ctx.lower()): 
            base += 2.0
        if has_qtr: 
            base += 1.25

        is_loss = _is_loss_ctx(ctx)

        val = normalize_num_token(tok, is_loss)
        if val is None or '.' not in tok or abs(val) > 20:
            continue
        score = base + decimal_bonus(tok) + plausibility_penalty(val) + integer_penalty(tok, val)
        cands.append((score, val))
    if not cands:
        return None
    cands.sort(key=lambda x: x[0])
    return cands[-1][1]

# ---------- Orchestrator ----------
def extract_eps_from_html(html_bytes: bytes) -> Optional[float]:
    # 1) Try tables first
    try:
        tables = _soup_tables(html_bytes)
    except Exception as e:
        log.warning("BeautifulSoup table parse failed (%s). Falling back to narrative only.", e)
        tables = []

    best_val=None; best_score=float('-inf')
    for t in tables:
        try:
            v, sc = extract_from_table(t.rows)
            if v is not None and sc > best_score:
                best_val, best_score = v, sc
        except Exception as te:
            log.debug("Table extraction error ignored: %s", te)

    if best_val is not None:
        return best_val

    # 2) Narrative passes (strict → EPS token → general)
    try:
        text = strip_text_bs4(html_bytes)
    except Exception as e:
        log.warning("Narrative text flattening failed: %s", e)
        text = ""

    v = extract_from_narrative_strict(text)
    if v is not None:
        return v
    v = extract_from_text_eps_token(text)
    if v is not None:
        return v
    return extract_from_text(text)

# ---------- CLI ----------
def main(argv=None):
    ap = argparse.ArgumentParser(description="EDGAR EPS parser (BeautifulSoup + robust parsing).")
    ap.add_argument('input_dir', help='Directory containing HTML filings')
    ap.add_argument('output_csv', help='Output CSV path')
    args = ap.parse_args(argv)

    in_dir = Path(args.input_dir)
    out_csv = Path(args.output_csv)
    out_csv.parent.mkdir(parents=True, exist_ok=True)

    files = sorted([p for p in in_dir.iterdir() if p.suffix.lower() in ('.html', '.htm')])

    with out_csv.open('w', newline='', encoding='utf-8') as f:
        w = csv.writer(f)
        w.writerow(['filename', 'EPS'])
        for fp in files:
            try:
                eps = extract_eps_from_html(fp.read_bytes())
                w.writerow([fp.name, '' if eps is None else f'{eps:.2f}'])
            except Exception as e:
                log.error("Failed on %s: %s", fp.name, e)
                w.writerow([fp.name, ''])

if __name__ == '__main__':
    main()