from html.parser import HTMLParser
from dataclasses import dataclass, field
from pathlib import Path
import argparse, csv, re, html
from typing import List, Optional, Tuple

# Regexes
NUMBER_RE = re.compile(r'(?<![A-Za-z])(?:\$)?\(?\s*[+-]?(?:\d{1,3}(?:,\d{3})*|\d+)(?:\.\d+)?\s*\)?(?![A-Za-z])')
ADJUSTED_RE = re.compile(r'\b(adjust(ed)?|non[-\s]?gaap|pro\s*forma|core\s+earnings?)\b', re.I)
BASIC_RE    = re.compile(r'\bbasic\b', re.I)
DILUTED_RE  = re.compile(r'\bdilut(?:ed|ion)\b', re.I)
EPS_HEADER_RE  = re.compile(r'\b(earnings|income|loss)\s+per\s+share\b', re.I)
LOSS_PHRASE_RE = re.compile(r'\bloss\s+per\s+share\b|\bnet\s+loss\b', re.I)
PER_SHARE_RE   = re.compile(r'\b((?:earnings|income|loss)\s+per\s+share|per\s+(?:basic|diluted)\s+share)\b', re.I)
CURRENCY_RE    = re.compile(r'\$\s*')
STRICT_EPS_PHRASE_RE = re.compile(r'(earnings|income|loss)\s+per\s+share\b', re.I)
EPS_TOKEN_RE = re.compile(r'\bEPS\b', re.I)
CONT_OPS_RE  = re.compile(r'continuing\s+operations', re.I)

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

# Numeric normalization
def normalize_num_token(tok: str, loss_ctx: bool) -> Optional[float]:
    neg = tok.strip().startswith('(') and tok.strip().endswith(')')
    tok = tok.strip().strip('()').replace(',', '')
    tok = CURRENCY_RE.sub('', tok)
    try:
        v = float(tok)
    except:
        return None
    if neg:
        v = -abs(v)
    if loss_ctx and v > 0:
        v = -v
    return v

def plausibility_penalty(v: float) -> float:
    av = abs(v)
    if av <= 20: return 0.0
    if av <= 50: return -3.0
    if av <= 100: return -6.0
    return -10.0

def decimal_bonus(tok: str) -> float:
    return 0.5 if '.' in tok else 0.0

def integer_penalty(tok: str, val: float) -> float:
    if '.' in tok: return 0.0
    return -2.0 if abs(val) >= 10 else -0.5

# HTML helpers (no external dependencies)
class Stripper(HTMLParser):
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.out: List[str] = []
        self.skip = False
    def handle_starttag(self, tag, attrs):
        if tag in ('script','style'): self.skip = True
    def handle_endtag(self, tag):
        if tag in ('script','style'): self.skip = False
    def handle_data(self, data):
        if not self.skip:
            s = ' '.join(data.split())
            if s: self.out.append(s)
    def text(self) -> str:
        return ' '.join(self.out)

@dataclass
class Table:
    rows: List[List[str]] = field(default_factory=list)

class TableParser(HTMLParser):
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.tables: List[Table] = []
        self.in_table=False; self.in_row=False; self.in_cell=False; self.skip=False
        self.cur_table: Optional[Table] = None
        self.cur_row: List[str] = []
        self.cur_cell: List[str] = []
    def handle_starttag(self, tag, attrs):
        if tag in ('script','style'): self.skip=True; return
        if tag=='table':
            self.in_table=True; self.cur_table=Table()
        elif tag=='tr' and self.in_table:
            self.in_row=True; self.cur_row=[]
        elif tag in ('td','th') and self.in_row:
            self.in_cell=True; self.cur_cell=[]
    def handle_endtag(self, tag):
        if tag in ('script','style'): self.skip=False
        if tag in ('td','th') and self.in_cell:
            txt = ' '.join(' '.join(self.cur_cell).split())
            self.cur_row.append(txt); self.in_cell=False; self.cur_cell=[]
        elif tag=='tr' and self.in_row:
            if any(c.strip() for c in self.cur_row):
                assert self.cur_table is not None
                self.cur_table.rows.append(self.cur_row)
            self.in_row=False; self.cur_row=[]
        elif tag=='table' and self.in_table:
            if self.cur_table and self.cur_table.rows:
                self.tables.append(self.cur_table)
            self.in_table=False; self.cur_table=None
    def handle_data(self, data):
        if not self.skip and self.in_cell:
            self.cur_cell.append(data)

# Header & scoring helpers
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
        score -= 6.0
    year_re = re.compile(r'\b(19\d{2}|20\d{2})\b')
    years = [int(m.group(0)) for m in year_re.finditer(cell_text)]
    if years:
        score += (max(years) - 1990) / 5.0
    score -= 0.25 * col_index  # bias left
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
            score -= 10.0
        years = [int(m.group(0)) for m in year_re.finditer(t)]
        if years:
            score += (max(years) - 1990) / 2.0
        score -= 0.2 * j
        if score > best[0]:
            best = (score, j)
    return best[1]

def maybe_merge_paren_token(tok: str, row: List[str], ci: int) -> str:
    if tok.strip().startswith('(') and not tok.strip().endswith(')'):
        if ci+1 < len(row) and row[ci+1].strip().startswith(')'):
            return tok.strip() + ')'
    return tok

def row_blocklisted(rr: List[str]) -> bool:
    return bool(BLOCK_ROW_RE.search(' '.join(rr)))

# Extraction
def extract_from_table(rows: List[List[str]]) -> Tuple[Optional[float], float]:
    if not rows: return (None, float('-inf'))
    header_rows = [r for r in rows[:5] if row_is_header_like(r)]
    best_col = pick_best_quarter_column(header_rows)
    eps_rows = [i for i,r in enumerate(rows) if EPS_HEADER_RE.search(' '.join(r))]
    candidates: List[Tuple[float,float]] = []

    def row_base_score(label: str) -> float:
        lab = label.lower()
        base = 0.0
        if ADJUSTED_RE.search(lab): base -= 8.0  # stronger non-GAAP penalty
        if 'net' in lab or 'total' in lab or 'consolidated' in lab: base += 1.5
        if CONT_OPS_RE.search(lab): base -= 1.5
        # Always prefer BASIC > DILUTED
        if BASIC_RE.search(lab): base += 5.0
        if DILUTED_RE.search(lab): base += 1.0
        # If generic EPS header line, small boost
        if EPS_HEADER_RE.search(lab): base += 0.5
        return base

    # EPS block (row with '... per share') and next few rows (Basic/Diluted)
    if eps_rows:
        for idx in eps_rows:
            band = rows[idx: min(len(rows), idx+4)]
            band_loss = bool(LOSS_PHRASE_RE.search(' '.join(rows[idx]).lower()))
            for rr in band:
                if row_blocklisted(rr):
                    continue
                label = ' '.join(rr).lower()
                is_loss = band_loss or bool(LOSS_PHRASE_RE.search(label))
                base = row_base_score(label)

                for ci, cell in enumerate(rr):
                    if '%' in cell:
                        continue
                    m = NUMBER_RE.search(cell)
                    if not m: 
                        continue
                    # Respect strict single best column if available
                    if best_col is not None and ci != best_col:
                        continue
                    if best_col is None and header_rows:
                        # Check this column isn't annual
                        hdr_bits = []
                        for hr in header_rows[:3]:
                            if ci < len(hr):
                                hdr_bits.append(hr[ci].lower())
                        col_hdr_text = ' '.join(hdr_bits)
                        if ('twelve months ended' in col_hdr_text or
                            'year ended' in col_hdr_text or
                            '12 months' in col_hdr_text):
                            continue

                    tok = maybe_merge_paren_token(m.group(0), rr, ci)
                    val = normalize_num_token(tok, is_loss)
                    if val is None: 
                        continue
                    # magnitude & label guards
                    if abs(val) > HARD_ABS_CAP:
                        continue
                    # If label isn't explicitly basic/diluted and the value is large, skip
                    if abs(val) >= 5.0 and not (BASIC_RE.search(label) or DILUTED_RE.search(label)):
                        continue
                    hscore = header_context_score(ci, header_rows)
                    score = base + hscore + decimal_bonus(tok) + plausibility_penalty(val) + integer_penalty(tok, val)
                    candidates.append((score, val))

    # Fallback: rows with Basic/Diluted (outside explicit EPS header)
    if not candidates:
        for rr in rows:
            if row_blocklisted(rr):
                continue
            label = ' '.join(rr).lower()
            if BASIC_RE.search(label) or DILUTED_RE.search(label):
                is_loss = bool(LOSS_PHRASE_RE.search(label))
                base = row_base_score(label)
                for ci, cell in enumerate(rr):
                    if '%' in cell:
                        continue
                    m = NUMBER_RE.search(cell)
                    if not m: 
                        continue
                    if best_col is not None and ci != best_col:
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

def strip_text(html_bytes: bytes) -> str:
    s = html_bytes.decode('utf-8', errors='ignore')
    s = re.sub(r'(?is)<!--.*?-->', ' ', s)
    st = Stripper(); st.feed(s)
    t = st.text()
    t = html.unescape(t)
    t = re.sub(r'\s+', ' ', t).strip()
    return t

def extract_from_narrative_strict(text: str) -> Optional[float]:
    best = None
    for m in STRICT_EPS_PHRASE_RE.finditer(text):
        win = text[m.end(): m.end() + 140]
        low = win.lower()
        mnum = NUMBER_RE.search(win)
        if not mnum:
            continue
        tok = mnum.group(0)
        if '.' not in tok:
            continue
        is_loss = ('loss' in m.group(0).lower()) or ('loss' in low)
        val = normalize_num_token(tok, is_loss)
        if val is None:
            continue
        if abs(val) > 20:
            continue
        base = 0.0
        # BASIC > DILUTED in narrative too
        if 'basic' in low:   base += 0.75
        if 'dilut' in low:   base += 0.25
        score = base + 0.5
        if (best is None) or (score > best[0]):
            best = (score, val)
    return None if best is None else best[1]

def extract_from_text_eps_token(text: str) -> Optional[float]:
    cands: List[Tuple[float, float]] = []
    for m in EPS_TOKEN_RE.finditer(text):
        start = max(0, m.start()-140); end = min(len(text), m.end()+160)
        window = text[start:end]
        low = window.lower()

        base = 0.0
        if re.search(r'\b(three\s+months|quarter(?:ly)?|q[1-4])\b', low): 
            base += 2.0
        if re.search(r'\b(twelve\s+months|year(?:\s+ended)?|six\s+months)\b', low): 
            base -= 2.0
        if ADJUSTED_RE.search(low): 
            base -= 8.0
        if BASIC_RE.search(low): 
            base += 0.5
        if DILUTED_RE.search(low): 
            base += 0.25
        is_loss = bool(LOSS_PHRASE_RE.search(low))

        after = text[m.end(): min(len(text), m.end()+100)]
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
        start = max(0, m.start()-120); end = min(len(text), m.end()+160)
        window = text[start:end]
        low = window.lower()
        base = 0.0
        if ADJUSTED_RE.search(low): base -= 8.0
        if BASIC_RE.search(low):    base += 2.0
        if DILUTED_RE.search(low):  base += 1.0
        if CONT_OPS_RE.search(low): base -= 1.0
        is_loss = bool(LOSS_PHRASE_RE.search(low))

        after  = text[m.end(): min(len(text), m.end()+120)]
        before = text[max(0, m.start()-120): m.start()]

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
        ctx = (before + " " + after).lower()
        is_loss = bool(LOSS_PHRASE_RE.search(ctx))
        base = 0.0
        if ADJUSTED_RE.search(ctx): base -= 8.0
        if BASIC_RE.search(ctx):    base += 2.0
        if DILUTED_RE.search(ctx):  base += 1.0
        if CONT_OPS_RE.search(ctx): base -= 1.0

        val = normalize_num_token(tok, is_loss)
        if val is None:
            continue
        if '.' not in tok:
            continue
        if abs(val) > 20:
            continue
        score = base + decimal_bonus(tok) + plausibility_penalty(val) + integer_penalty(tok, val)
        cands.append((score, val))
    if not cands:
        return None
    cands.sort(key=lambda x: x[0])
    return cands[-1][1]

def extract_eps_from_html(html_bytes: bytes) -> Optional[float]:
    s = html_bytes.decode('utf-8', errors='ignore')
    s = re.sub(r'(?is)<!--.*?-->', ' ', s)
    tp = TableParser(); tp.feed(s)
    best_val=None; best_score=float('-inf')
    # Table-level GAAP hint: if a table block clearly says adjusted/non-GAAP, downweight
    for t in tp.tables:
        table_text = ' '.join(' '.join(r) for r in t.rows).lower()
        table_penalty = -4.0 if ADJUSTED_RE.search(table_text) else 0.0
        v, sc = extract_from_table(t.rows)
        if v is not None:
            sc += table_penalty
            if sc > best_score:
                best_val, best_score = v, sc
    if best_val is not None:
        return best_val
    text = strip_text(html_bytes)
    v = extract_from_narrative_strict(text)
    if v is not None:
        return v
    v = extract_from_text_eps_token(text)
    if v is not None:
        return v
    return extract_from_text(text)

def main(argv=None):
    ap = argparse.ArgumentParser(description="EDGAR EPS parser (stdlib-only, BASIC> DILUTED, GAAP>non-GAAP).")
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
            except Exception:
                w.writerow([fp.name, ''])

if __name__ == '__main__':
    main()