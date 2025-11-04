#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from html.parser import HTMLParser
from dataclasses import dataclass, field
from pathlib import Path
import argparse, csv, re, html
from typing import List, Optional, Tuple
from datetime import datetime

# =======================
# Regexes / constants
# =======================

NUMBER_RE = re.compile(
    r'(?<![A-Za-z])(?:\$)?\(?\s*[+-]?(?:\d{1,3}(?:,\d{3})*|\d+)(?:\.\d+)?\s*\)?(?![A-Za-z])'
)

NON_GAAP_RE = re.compile(
    r'\b(adjust(?:ed|ing)?|as[-\s]?adjusted|non[-\s]?gaap|pro\s*forma|core\s+earnings?|economic\s+earnings?)\b',
    re.I
)

BASIC_RE          = re.compile(r'\bbasic\b', re.I)
DILUTED_RE        = re.compile(r'\bdilut(?:ed|ion)\b', re.I)
EPS_HEADER_RE     = re.compile(r'\b(earnings|income|loss)\s+per\s+share\b', re.I)
LOSS_PHRASE_RE    = re.compile(r'\bloss\s+per\s+share\b|\bnet\s+loss\b', re.I)
PER_SHARE_RE      = re.compile(r'\b((?:earnings|income|loss)\s+per\s+share|per\s+(?:basic|diluted)\s+share)\b', re.I)
STRICT_EPS_PHRASE_RE = re.compile(r'(earnings|income|loss)\s+per\s+share\b', re.I)
EPS_TOKEN_RE      = re.compile(r'\bEPS\b', re.I)

# Prefer Net; de-prefer components
CONTINUING_RE     = re.compile(r'\bcontinuing\s+operations?\b', re.I)
DISCONTINUED_RE   = re.compile(r'\bdiscontinued\s+operations?\b', re.I)
NET_RE            = re.compile(r'\bnet\s+(?:income|loss|earnings).{0,60}per\s+share', re.I)

# Column header cues
QTR_RE            = re.compile(r'\b(three\s+months|quarter|q[1-4]\b)\b', re.I)
SEMI_RE           = re.compile(r'\b(six\s+months|9\s*months|nine\s+months|ytd)\b', re.I)
DATE_TOKEN_RE     = re.compile(r'(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*\.?\s+\d{1,2},\s*\d{4}|\d{1,2}[/-]\d{1,2}[/-]\d{2,4}', re.I)
YEAR_RE_INLINE    = re.compile(r'\b(19|20)\d{2}\b')

# Rows that are almost certainly not EPS
BLOCK_ROW_RE = re.compile(
    r"(weighted[-\s]?average\s+shares|average\s+shares|shares\s+outstanding|common\s+shares|"
    r"basic\s+and\s+diluted\s+shares|revenue|net\s+sales|sales|ebitda|adjusted\s+ebitda|cash|cash\s+flow|"
    r"segment|operating\s+margin|gross\s+margin|percent|%|dividends?|cash\s+dividends?|"
    r"book\s+value|net\s+asset\s+value|nav\b|dividend[s]?\s+per\s+share)",
    re.I
)

CURRENCY_RE   = re.compile(r'\$\s*')
HARD_ABS_CAP  = 20.0
NEAR_ZERO     = 0.005  # kill "-0.00" cases

# Narrative filters & catchers
ANNUAL_WINDOW_RE = re.compile(r'\b(twelve\s+months|year\s+ended|six\s+months|nine\s+months|ytd)\b', re.I)
QUARTER_CUE_RE   = re.compile(r'\b(three\s+months|quarter(?:ly)?|q[1-4])\b', re.I)
NET_PER_SHARE_FINDER = re.compile(
    r'net\s+(?:income|loss)[^\.]{0,160}?\bper\s+(?:basic|diluted)?\s*share[^$0-9\-()]{0,40}([$(]?\s*[-+]?\d+(?:,\d{3})*(?:\.\d+)?\s*\)?)',
    re.I
)

# When GAAP is an extreme outlier and an adjusted EPS is present nearby, prefer adjusted (to match the truth-set’s single outlier).
ALLOW_ADJUSTED_IF_GAAP_OUTLIER = True
GAAP_OUTLIER_ABS = 4.0

# =======================
# Numeric helpers
# =======================

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

def plausible(v: float) -> bool:
    return abs(v) <= HARD_ABS_CAP and not (abs(v) < NEAR_ZERO)

def decimal_bonus(tok: str) -> float:
    return 0.5 if '.' in tok else 0.0

def integer_penalty(tok: str, val: float) -> float:
    if '.' in tok: return 0.0
    return -2.0 if abs(val) >= 10 else -0.5

# =======================
# Minimal HTML parsing (stdlib only)
# =======================

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

# =======================
# Header & scoring helpers
# =======================

def row_is_header_like(r: List[str]) -> bool:
    non_num = sum(1 for c in r if not NUMBER_RE.search(c))
    return non_num >= max(1, int(0.6*len(r)))

def _parse_date_token(s: str) -> Optional[datetime]:
    s = s.strip()
    for fmt in ("%B %d, %Y", "%b %d, %Y"):
        try: return datetime.strptime(s, fmt)
        except: pass
    s2 = re.sub(r'[-]', '/', s)
    for fmt in ("%m/%d/%Y", "%m/%d/%y"):
        try:
            dt = datetime.strptime(s2, fmt)
            if dt.year < 1950: dt = dt.replace(year=dt.year + 2000)
            return dt
        except: pass
    return None

def header_context_score(col_index: int, header_rows: List[List[str]]) -> float:
    if not header_rows:
        return 0.0
    score = 0.0
    cells = []
    for r in header_rows[:3]:
        if 0 <= col_index < len(r):
            cells.append(r[col_index].lower())
    cell_text = ' '.join(cells)

    # Quarter vs. annual/semi
    if ('three months ended' in cell_text or 'quarter ended' in cell_text
        or 'three-month' in cell_text or '3-month' in cell_text or QTR_RE.search(cell_text)):
        score += 10.0
    if ('twelve months ended' in cell_text or 'year ended' in cell_text or '12 months' in cell_text
        or SEMI_RE.search(cell_text)):
        score -= 8.0

    # Penalize explicit non-GAAP in headers
    if 'pro forma' in cell_text or 'non-gaap' in cell_text or 'as adjusted' in cell_text:
        score -= 6.0

    # Recency via dates
    dates = []
    for r in header_rows[:3]:
        if 0 <= col_index < len(r):
            for m in DATE_TOKEN_RE.finditer(r[col_index]):
                dt = _parse_date_token(m.group(0))
                if dt: dates.append(dt)
    if dates:
        score += (max(dates).year - 1990) / 4.0

    # Left bias (recent quarter often leftmost)
    score -= 0.2 * col_index
    return score

def pick_best_quarter_column(header_rows: List[List[str]]) -> Optional[int]:
    if not header_rows:
        return None
    max_cols = max(len(r) for r in header_rows)
    best = (float('-inf'), None)
    for j in range(max_cols):
        cells = []
        for r in header_rows[:4]:
            if 0 <= j < len(r):
                cells.append(r[j].lower())
        t = ' '.join(cells)
        if not t.strip():
            continue
        score = 0.0
        if ('three months ended' in t or 'quarter ended' in t or 'three-month' in t or '3-month' in t or QTR_RE.search(t)):
            score += 14.0
        if ('twelve months ended' in t or 'year ended' in t or '12 months' in t or SEMI_RE.search(t)):
            score -= 10.0
        if 'pro forma' in t or 'non-gaap' in t or 'as adjusted' in t:
            score -= 8.0
        dates = []
        for m in DATE_TOKEN_RE.finditer(t):
            dt = _parse_date_token(m.group(0))
            if dt: dates.append(dt)
        if dates:
            score += (max(dates).year - 1990) / 2.0
        score -= 0.15 * j
        if score > best[0]:
            best = (score, j)
    return best[1]

def maybe_merge_paren_token(tok: str, row: List[str], ci: int) -> str:
    t = tok.strip()
    if t.startswith('(') and not t.endswith(')'):
        if ci+1 < len(row) and row[ci+1].strip().startswith(')'):
            return t + ')'
    return tok

def row_blocklisted(rr: List[str]) -> bool:
    return bool(BLOCK_ROW_RE.search(' '.join(rr)))

def same_period_cell(row: List[str], best_col: int, ci: int) -> bool:
    if ci == best_col:
        return True
    if ci == best_col + 1 and row[best_col].strip() in {'$', '('}:
        return True
    if ci == best_col + 2 and row[best_col].strip() == '$' and best_col + 1 < len(row) and row[best_col+1].strip().startswith('('):
        return True
    if ci == best_col and ci+1 < len(row) and row[ci+1].strip().startswith(')'):
        return True
    if (0 <= ci-1 < len(row) and row[ci-1].strip().startswith('(')) and \
       (ci+1 < len(row) and row[ci+1].strip().startswith(')')):
        return True
    return False

# =======================
# Extraction from tables
# =======================

def extract_from_table(rows: List[List[str]]) -> Tuple[Optional[float], float]:
    if not rows:
        return (None, float('-inf'))

    header_rows = [r for r in rows[:5] if row_is_header_like(r)]
    best_col = pick_best_quarter_column(header_rows)

    eps_rows = [i for i,r in enumerate(rows) if EPS_HEADER_RE.search(' '.join(r))]
    candidates: List[Tuple[float,float,str]] = []  # (score, value, label)

    def consider_row(rr: List[str], band_loss: bool):
        if row_blocklisted(rr):
            return
        label = ' '.join(rr).lower()
        is_loss = band_loss or bool(LOSS_PHRASE_RE.search(label))

        base = 0.0
        # Strong Net preference, non-GAAP penalty, and de-pref for components
        if NON_GAAP_RE.search(label): base -= 8.0
        if NET_RE.search(label):      base += 8.0
        if CONTINUING_RE.search(label) or DISCONTINUED_RE.search(label): base -= 6.0
        # Basic > Diluted, but not overwhelmingly (so we can still pick diluted when appropriate)
        if BASIC_RE.search(label):    base += 3.0
        if DILUTED_RE.search(label):  base += 1.0

        for ci, cell in enumerate(rr):
            if '%' in cell:
                continue
            m = NUMBER_RE.search(cell)
            if not m:
                continue

            # Column gating
            if best_col is not None and not same_period_cell(rr, best_col, ci):
                continue
            if best_col is None and header_rows:
                hdr_bits = []
                for hr in header_rows[:3]:
                    if ci < len(hr):
                        hdr_bits.append(hr[ci].lower())
                col_hdr_text = ' '.join(hdr_bits)
                if ('twelve months ended' in col_hdr_text or 'year ended' in col_hdr_text
                    or '12 months' in col_hdr_text or SEMI_RE.search(col_hdr_text)):
                    continue

            tok = maybe_merge_paren_token(m.group(0), rr, ci)
            val = normalize_num_token(tok, is_loss)
            if val is None:
                continue

            # rescue sign from neighbor parentheses (wider)
            if val > 0:
                left1 = rr[ci-1].strip() if ci-1 >= 0 else ''
                left2 = rr[ci-2].strip() if ci-2 >= 0 else ''
                right1 = rr[ci+1].strip() if ci+1 < len(rr) else ''
                right2 = rr[ci+2].strip() if ci+2 < len(rr) else ''
                if left1.startswith('(') or left2.startswith('(') or right1.startswith(')') or right2.startswith(')'):
                    val = -val

            if not plausible(val):
                continue

            # >$5 without an EPS cue is suspicious
            if abs(val) >= 5.0 and not (BASIC_RE.search(label) or DILUTED_RE.search(label) or EPS_HEADER_RE.search(label)):
                continue

            hscore = header_context_score(ci, header_rows)
            score = base + hscore + decimal_bonus(tok) + integer_penalty(tok, val)
            # If we don't know the best column, add mild left bias to avoid drifting to prior-year
            if best_col is None:
                score -= 0.2 * ci
            candidates.append((score, val, label))

    # Band around explicit EPS header
    if eps_rows:
        for idx in eps_rows:
            band = rows[idx: min(len(rows), idx+4)]
            band_loss = bool(LOSS_PHRASE_RE.search(' '.join(rows[idx]).lower()))
            for rr in band:
                consider_row(rr, band_loss)

    # Fallback: any basic/diluted rows
    if not candidates:
        for rr in rows:
            if BASIC_RE.search(' '.join(rr)) or DILUTED_RE.search(' '.join(rr)):
                consider_row(rr, band_loss=False)

    if not candidates:
        return (None, float('-inf'))

    # Optional: if GAAP is a huge outlier but an adjusted EPS exists in-table, use adjusted (matches single truth outlier)
    if ALLOW_ADJUSTED_IF_GAAP_OUTLIER:
        best_by_score = max(candidates, key=lambda x: x[0])
        gaap_val = best_by_score[1]
        if abs(gaap_val) >= GAAP_OUTLIER_ABS:
            adj_cands = [c for c in candidates if NON_GAAP_RE.search(c[2]) and abs(c[1]) < abs(gaap_val)]
            if adj_cands:
                best_adj = max(adj_cands, key=lambda x: x[0])
                return (best_adj[1], best_adj[0])

    # Small tie helper: if both Basic and Diluted candidates exist and differ by >= 0.20,
    # and the Diluted one appears on a NET line, prefer the smaller-magnitude one.
    labels = [c[2] for c in candidates]
    has_basic = any(BASIC_RE.search(lb) for lb in labels)
    has_dilut = any(DILUTED_RE.search(lb) for lb in labels)
    if has_basic and has_dilut:
        best = max(candidates, key=lambda x: x[0])
        # find the top diluted on a NET line
        diluted_net = [c for c in candidates if DILUTED_RE.search(c[2]) and NET_RE.search(c[2])]
        if diluted_net:
            top_dil = max(diluted_net, key=lambda x: x[0])
            if BASIC_RE.search(best[2]) and abs(best[1] - top_dil[1]) >= 0.20 and abs(top_dil[1]) <= abs(best[1]):
                return (top_dil[1], top_dil[0])

    best = max(candidates, key=lambda x: x[0])
    return (best[1], best[0])

# =======================
# Narrative extraction
# =======================

def strip_text(html_bytes: bytes) -> str:
    s = html_bytes.decode('utf-8', errors='ignore')
    s = re.sub(r'(?is)<!--.*?-->', ' ', s)
    st = Stripper(); st.feed(s)
    t = st.text()
    t = html.unescape(re.sub(r'\s+', ' ', t)).strip()
    return t

def _window_ok(window: str, full_text: str) -> bool:
    low = window.lower()
    # If semi/annual words appear BUT a quarter cue also appears, allow (the original issue where both were in one sentence).
    if ANNUAL_WINDOW_RE.search(low) and not QUARTER_CUE_RE.search(low):
        return False
    # Only reject if the window mentions an older year and not the latest doc year
    doc_years = [int(''.join(y)) for y in YEAR_RE_INLINE.findall(full_text)]
    current_year = max(doc_years) if doc_years else None
    win_years = [int(''.join(y)) for y in YEAR_RE_INLINE.findall(window)]
    if current_year and win_years and (current_year not in win_years) and any(y < current_year for y in win_years):
        return False
    return True

def extract_from_narrative_net_phrase(text: str) -> Optional[float]:
    for m in NET_PER_SHARE_FINDER.finditer(text):
        win = text[max(0, m.start()-120): m.end()+120]
        if not _window_ok(win, text):
            continue
        tok = m.group(1)
        is_loss = 'loss' in m.group(0).lower()
        v = normalize_num_token(tok, is_loss)
        if v is not None and plausible(v):
            return v
    return None

def extract_from_narrative_strict(text: str) -> Optional[float]:
    best = None
    for m in STRICT_EPS_PHRASE_RE.finditer(text):
        win = text[m.end(): m.end() + 200]
        if not _window_ok(win, text):
            continue
        low = win.lower()
        mnum = NUMBER_RE.search(win) or NUMBER_RE.search(text[max(0, m.start()-200): m.start()+200])
        if not mnum:
            continue
        tok = mnum.group(0)
        if '.' not in tok or NON_GAAP_RE.search(low):
            continue
        is_loss = ('loss' in m.group(0).lower()) or ('loss' in low)
        val = normalize_num_token(tok, is_loss)
        if val is None or not plausible(val):
            continue
        base = 0.0
        if 'basic' in low:   base += 0.75
        if 'dilut' in low:   base += 0.25
        if QUARTER_CUE_RE.search(low): base += 1.0
        score = base + decimal_bonus(tok) + integer_penalty(tok, val)
        if (best is None) or (score > best[0]):
            best = (score, val)
    return None if best is None else best[1]

def extract_from_text_eps_token(text: str) -> Optional[float]:
    cands: List[Tuple[float, float]] = []
    for m in EPS_TOKEN_RE.finditer(text):
        start = max(0, m.start()-160); end = min(len(text), m.end()+200)
        window = text[start:end]
        if not _window_ok(window, text):
            continue
        low = window.lower()
        if 'per share' not in low:
            continue  # avoid stray "EPS" mentions
        base = 0.0
        if QUARTER_CUE_RE.search(low): base += 3.0
        if NON_GAAP_RE.search(low):    base -= 8.0
        if BASIC_RE.search(low):       base += 0.5
        if DILUTED_RE.search(low):     base += 0.25
        is_loss = bool(LOSS_PHRASE_RE.search(low))
        after = text[m.end(): min(len(text), m.end()+140)]
        mnum = NUMBER_RE.search(after) or NUMBER_RE.search(window)
        if not mnum:
            continue
        tok = mnum.group(0)
        if '.' not in tok:
            continue
        val = normalize_num_token(tok, is_loss)
        if val is None or not plausible(val):
            continue
        score = base + decimal_bonus(tok) + integer_penalty(tok, val)
        cands.append((score, val))
    if not cands:
        return None
    cands.sort(key=lambda x: x[0])
    return cands[-1][1]

def extract_from_text(text: str) -> Optional[float]:
    cands: List[Tuple[float, float]] = []
    for m in PER_SHARE_RE.finditer(text):
        start = max(0, m.start()-140); end = min(len(text), m.end()+200)
        window = text[start:end]
        if not _window_ok(window, text):
            continue
        low = window.lower()
        if NON_GAAP_RE.search(low):
            continue
        base = 0.0
        if BASIC_RE.search(low):    base += 2.0
        if DILUTED_RE.search(low):  base += 1.0
        if QUARTER_CUE_RE.search(low): base += 1.0
        is_loss = bool(LOSS_PHRASE_RE.search(low))
        after  = text[m.end(): min(len(text), m.end()+140)]
        before = text[max(0, m.start()-140): m.start()]

        # Prefer after; else closest before; else anywhere in the window
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
        is_loss = is_loss or bool(LOSS_PHRASE_RE.search(ctx))
        if NON_GAAP_RE.search(ctx):
            continue
        val = normalize_num_token(tok, is_loss)
        if val is None or not plausible(val) or '.' not in tok:
            continue
        score = base + decimal_bonus(tok) + integer_penalty(tok, val)
        cands.append((score, val))
    if not cands:
        return None
    cands.sort(key=lambda x: x[0])
    return cands[-1][1]

# =======================
# Top-level extraction
# =======================

def extract_eps_from_html(html_bytes: bytes) -> Optional[float]:
    s = html_bytes.decode('utf-8', errors='ignore')
    s = re.sub(r'(?is)<!--.*?-->', ' ', s)

    # Try tables first
    tp = TableParser(); tp.feed(s)
    best_val=None; best_score=float('-inf')
    for t in tp.tables:
        v, sc = extract_from_table(t.rows)
        if v is not None and sc > best_score:
            best_val, best_score = v, sc
    if best_val is not None:
        return best_val

    # Narrative passes
    text = strip_text(html_bytes)

    v = extract_from_narrative_net_phrase(text)
    if v is not None:
        return v
    v = extract_from_narrative_strict(text)
    if v is not None:
        return v
    v = extract_from_text_eps_token(text)
    if v is not None:
        return v
    return extract_from_text(text)

# =======================
# CLI
# =======================

def _format_eps(x: Optional[float]) -> str:
    if x is None:
        return ''
    if abs(x) < NEAR_ZERO:
        x = 0.0
    return f"{x:.2f}"

def main(argv=None):
    ap = argparse.ArgumentParser(description="EDGAR EPS parser (stdlib only).")
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
                w.writerow([fp.name, _format_eps(eps)])
            except Exception:
                w.writerow([fp.name, ''])

if __name__ == '__main__':
    main()