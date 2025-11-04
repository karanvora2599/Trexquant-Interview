#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from pathlib import Path
from dataclasses import dataclass, field
from typing import List, Optional, Tuple
import argparse, csv, re, html
from datetime import datetime

try:
    from bs4 import BeautifulSoup
except ImportError:
    raise SystemExit("Please install BeautifulSoup first: pip install beautifulsoup4")

# ========= Regex / constants =========
NUMBER_RE = re.compile(r'(?<![A-Za-z])(?:\$)?\(?\s*[+-]?(?:\d{1,3}(?:,\d{3})*|\d+)(?:\.\d+)?\s*\)?(?![A-Za-z])')

NON_GAAP_RE = re.compile(r'\b(adjust(?:ed|ing)?|as[-\s]?adjusted|non[-\s]?gaap|pro\s*forma|core\s+earnings?)\b', re.I)
BASIC_RE    = re.compile(r'\bbasic\b', re.I)
DILUTED_RE  = re.compile(r'\bdilut(?:ed|ion)\b', re.I)
EPS_HEADER_RE  = re.compile(r'\b(earnings|income|loss)\s+per\s+share\b', re.I)
LOSS_PHRASE_RE = re.compile(r'\bnet\s+loss\b|\bloss\s+per\s+share\b', re.I)
PER_SHARE_RE   = re.compile(r'\b((?:earnings|income|loss)\s+per\s+share|per\s+(?:basic|diluted)\s+share)\b', re.I)
STRICT_EPS_PHRASE_RE = re.compile(r'(earnings|income|loss)\s+per\s+share\b', re.I)
EPS_TOKEN_RE = re.compile(r'\bEPS\b', re.I)

NET_ROW_RE       = re.compile(r'\bnet\s+(?:income|loss|earnings)(?!\s+per\s+share)\b', re.I)
CONTINUING_RE    = re.compile(r'\bcontinuing\s+operations?\b', re.I)
DISCONTINUED_RE  = re.compile(r'\bdiscontinued\s+operations?\b', re.I)

QTR_RE    = re.compile(r'\b(three\s+months|quarter|q[1-4]\b)\b', re.I)
SEMI_RE   = re.compile(r'\b(six\s+months|9\s*months|nine\s+months|ytd|year\s+to\s+date)\b', re.I)
ANNUAL_RE = re.compile(r'\b(twelve\s+months|year\s+ended|annual)\b', re.I)

DATE_TOKEN_RE  = re.compile(r'(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*\.?\s+\d{1,2},\s*\d{4}|\d{1,2}[/-]\d{1,2}[/-]\d{2,4}', re.I)
YEAR_RE_INLINE = re.compile(r'\b(19|20)\d{2}\b')

CENTS_RE   = re.compile(r'\bcents?\b', re.I)
CURRENCY_RE = re.compile(r'\$\s*')

# “per share impact/effect/charge/dividend” → noise (exclude)
PER_SHARE_NOISE_RE = re.compile(
    r'\b(impact|effect|increase|decrease|benefit|charge|impairment|tax|hurricane|storm|covid|pandemic|'
    r'acquisition[-\s]?related|purchase\s+accounting|mark[-\s]?to[-\s]?market|non[-\s]?cash|special\s+item|'
    r'dividend[s]?|stock\s+split|repurchase|buyback)\b',
    re.I
)

# Rows that are not EPS lines
BLOCK_ROW_RE = re.compile(
    r"(weighted[-\s]?average\s+shares|average\s+shares|shares\s+outstanding|common\s+shares|"
    r"basic\s+and\s+diluted\s+shares|revenue|net\s+sales|sales|ebitda|adjusted\s+ebitda|cash|cash\s+flow|"
    r"segment|operating\s+margin|gross\s+margin|percent|%|dividends?|cash\s+dividends?|"
    r"book\s+value|net\s+asset\s+value|nav\b|dividend[s]?\s+per\s+share)",
    re.I
)

HARD_ABS_CAP   = 20.0
EPS_BIG_SUS    = 5.0
NEAR_ZERO      = 0.005
GAAP_OUTLIER_ABS = 4.0

# ========= helpers =========
def _clean(s: str) -> str:
    return re.sub(r'\s+', ' ', html.unescape(s or '')).strip()

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

def row_is_header_like(cells: List[str]) -> bool:
    non_num = sum(1 for c in cells if not NUMBER_RE.search(c))
    return non_num >= max(1, int(0.6 * len(cells)))

def normalize_num_token(tok: str) -> Optional[float]:
    t = tok.strip()
    neg_tok = t.startswith('(') and t.endswith(')')
    t = t.strip('()').replace(',', '')
    t = CURRENCY_RE.sub('', t)
    try:
        v = float(t)
    except Exception:
        return None
    if neg_tok:
        v = -abs(v)
    return v

def plausible(v: float) -> bool:
    return abs(v) <= HARD_ABS_CAP and not (abs(v) < NEAR_ZERO)

# ========= table grid (rowspan/colspan aware) =========
@dataclass
class TableGrid:
    rows: List[List[str]] = field(default_factory=list)

def build_table_grid(tag) -> TableGrid:
    trs = tag.find_all('tr')
    grid: List[List[str]] = []
    carry: List[Tuple[int,int,str]] = []  # (col_idx, rows_left, text)

    for tr in trs:
        row: List[str] = []
        if carry:
            max_col = max(c for c, _, _ in carry)
            if len(row) <= max_col:
                row.extend([''] * (max_col - len(row) + 1))
            for c0, _, txt in carry:
                row[c0] = _clean(txt)
        col = 0
        for cell in tr.find_all(['td','th']):
            txt = cell.get_text(" ", strip=True)
            cs = int(cell.get('colspan', 1) or 1)
            rs = int(cell.get('rowspan', 1) or 1)
            while col < len(row) and row[col] != '':
                col += 1
            if col >= len(row):
                row.extend([''] * (col - len(row) + 1))
            for k in range(cs):
                if len(row) <= col + k:
                    row.extend([''] * (col + k - len(row) + 1))
                row[col + k] = _clean(txt)
            if rs > 1:
                for k in range(cs):
                    carry.append((col + k, rs - 1, _clean(txt)))
            col += cs
        new_carry = []
        for c0, rem, txt in carry:
            if rem - 1 > 0:
                new_carry.append((c0, rem - 1, txt))
        carry = new_carry

        while row and row[-1] == '':
            row.pop()
        if any(_clean(x) for x in row):
            grid.append([_clean(x) for x in row])

    return TableGrid(rows=grid)

# ========= column scoring / quarter lock =========
def header_context_score(col_index: int, header_rows: List[List[str]]) -> float:
    if not header_rows: return 0.0
    score = 0.0
    cells = []
    for r in header_rows[:3]:
        if 0 <= col_index < len(r):
            cells.append(r[col_index].lower())
    text = ' '.join(cells)

    if QTR_RE.search(text) or 'three months ended' in text or 'quarter ended' in text:
        score += 16.0
    if ANNUAL_RE.search(text) or SEMI_RE.search(text) or '12 months' in text:
        score -= 14.0
    if NON_GAAP_RE.search(text):
        score -= 6.0

    dates = []
    for r in header_rows[:3]:
        if 0 <= col_index < len(r):
            for m in DATE_TOKEN_RE.finditer(r[col_index]):
                dt = _parse_date_token(m.group(0))
                if dt: dates.append(dt)
    if dates:
        score += (max(dates).year - 1995) / 2.0
    score -= 0.12 * col_index
    return score

def pick_best_quarter_column(header_rows: List[List[str]]) -> Optional[int]:
    if not header_rows: return None
    max_cols = max(len(r) for r in header_rows)
    best = (float('-inf'), None)
    for j in range(max_cols):
        sc = header_context_score(j, header_rows)
        if sc > best[0]:
            best = (sc, j)
    return best[1]

def same_period_cell(row: List[str], best_col: int, ci: int) -> bool:
    if ci == best_col: return True
    if ci == best_col + 1 and row[best_col].strip() in {'$', '('}:
        return True
    return False

# ========= candidates =========
@dataclass
class Cand:
    score: float
    value: float
    label: str
    source: str  # 'table' or 'narr'
    ctx: str     # local context (for GAAP/Adjusted detection & cues)
    col_index: Optional[int] = None
    table_ref: Optional[TableGrid] = None

# ========= table extraction (with cross-row sign mirror) =========
def row_blocklisted(rr: List[str]) -> bool:
    return bool(BLOCK_ROW_RE.search(' '.join(rr)))

def detect_cents_scale_row(row_text: str) -> float:
    return 0.01 if CENTS_RE.search(row_text) else 1.0

def _lookup_net_sign(table: TableGrid, col: int) -> Optional[float]:
    if col is None or col < 0: return None
    # search a reasonable vertical window; accept nearest numeric in [col-1, col, col+1]
    for rr in table.rows[:60]:
        if not rr: continue
        lbl = rr[0].lower()
        if 'per share' in lbl:  # skip per-share lines
            continue
        if NET_ROW_RE.search(lbl):
            for cc in (col, col-1, col+1):
                if 0 <= cc < len(rr):
                    m = NUMBER_RE.search(rr[cc])
                    if m:
                        v = normalize_num_token(m.group(0))
                        if v is not None:
                            return v
    return None

def extract_from_table(grid: TableGrid) -> List[Cand]:
    rows = grid.rows
    if not rows: return []

    header_rows = [r for r in rows[:6] if row_is_header_like(r)]
    best_col = pick_best_quarter_column(header_rows)

    cands: List[Cand] = []

    def consider_row(rr: List[str]):
        if row_blocklisted(rr): return
        label = ' '.join(rr).lower()
        if PER_SHARE_NOISE_RE.search(label) and not EPS_HEADER_RE.search(label):
            return

        base = 0.0
        if NON_GAAP_RE.search(label): base -= 10.0
        if NET_ROW_RE.search(label):  base += 6.0
        if CONTINUING_RE.search(label) or DISCONTINUED_RE.search(label): base -= 4.0
        if DILUTED_RE.search(label):  base += 3.0
        if BASIC_RE.search(label):    base += 1.0

        scale = detect_cents_scale_row(label)

        for ci, cell in enumerate(rr):
            if '%' in cell: continue
            m = NUMBER_RE.search(cell)
            if not m: continue

            if best_col is not None and not same_period_cell(rr, best_col, ci):
                continue
            if best_col is None and header_rows:
                hdr_bits = []
                for hr in header_rows[:3]:
                    if ci < len(hr): hdr_bits.append(hr[ci].lower())
                t = ' '.join(hdr_bits)
                if ANNUAL_RE.search(t) or SEMI_RE.search(t) or '12 months' in t:
                    continue

            tok = m.group(0)
            v = normalize_num_token(tok)
            if v is None: continue
            if abs(v) >= EPS_BIG_SUS and not (EPS_HEADER_RE.search(label) or BASIC_RE.search(label) or DILUTED_RE.search(label)):
                continue
            v *= scale
            if not plausible(v): continue

            hscore = header_context_score(ci, header_rows)
            score = base + hscore + (0.4 if '.' in tok else (-2.0 if abs(v) >= 10 else -0.4))

            # Cross-row sign mirror
            net = _lookup_net_sign(grid, ci if best_col is None else best_col)
            if net is not None:
                if net < 0 and v > 0: v = -v
                if net > 0 and v < 0: v = -v

            ctx = (label + " " + (header_rows[0][ci].lower() if header_rows and ci < len(header_rows[0]) else '')).strip()
            cands.append(Cand(score, v, label, 'table', ctx=ctx, col_index=ci, table_ref=grid))

    eps_rows = [i for i, r in enumerate(rows) if EPS_HEADER_RE.search(' '.join(r))]
    if eps_rows:
        for idx in eps_rows:
            for rr in rows[idx: min(len(rows), idx+6)]:
                consider_row(rr)

    if not cands:
        for rr in rows:
            lbl = ' '.join(rr).lower()
            if BASIC_RE.search(lbl) or DILUTED_RE.search(lbl) or EPS_HEADER_RE.search(lbl):
                consider_row(rr)

    return cands

# ========= narrative extraction (noise-guarded) =========
def soup_text_bytes(html_bytes: bytes) -> str:
    soup = BeautifulSoup(html_bytes, "lxml") if b"<" in html_bytes else BeautifulSoup(html_bytes, "html.parser")
    for tg in soup(["script", "style"]):
        tg.decompose()
    t = soup.get_text(" ")
    t = html.unescape(re.sub(r'\s+', ' ', t)).strip()
    return t

def _window_ok(window: str, full_text: str) -> bool:
    low = window.lower()
    if PER_SHARE_NOISE_RE.search(low): return False
    if (ANNUAL_RE.search(low) or SEMI_RE.search(low)) and not QTR_RE.search(low):
        return False
    doc_years = [int(m.group(0)) for m in YEAR_RE_INLINE.finditer(full_text)]
    if doc_years:
        win_years = [int(m.group(0)) for m in YEAR_RE_INLINE.finditer(window)]
        if win_years and max(win_years) < max(doc_years) and not QTR_RE.search(low):
            return False
    return True

NET_PER_SHARE_FINDER = re.compile(
    r'net\s+(income|loss)[^\.]{0,180}?per\s+(?:diluted|basic)?\s*share[^$0-9\-()]{0,80}([$(]?\s*[-+]?\d+(?:,\d{3})*(?:\.\d+)?\s*\)?)',
    re.I
)

def extract_from_narrative(text: str) -> List[Cand]:
    cands: List[Cand] = []

    # 1) "net income/loss ... per diluted/basic share ... $X"
    for m in NET_PER_SHARE_FINDER.finditer(text):
        win = text[max(0, m.start()-160): m.end()+160]
        if not _window_ok(win, text): continue
        is_loss = (m.group(1).lower() == 'loss')
        tok = m.group(2)
        v = normalize_num_token(tok)
        if v is None: continue
        if is_loss and v > 0: v = -v
        if not plausible(v): continue
        base = 0.0
        if DILUTED_RE.search(win): base += 3.0
        if BASIC_RE.search(win):   base += 1.0
        if QTR_RE.search(win):     base += 2.0
        if NON_GAAP_RE.search(win): base -= 10.0
        cands.append(Cand(base, v, 'net per share (narr)', 'narr', ctx=win))

    # 2) strict "... per share" sentence (GAAP only)
    for m in STRICT_EPS_PHRASE_RE.finditer(text):
        win = text[m.start(): m.end()+220]
        if not _window_ok(win, text): continue
        if NON_GAAP_RE.search(win): continue
        mnum = NUMBER_RE.search(win)
        if not mnum: continue
        tok = mnum.group(0)
        v = normalize_num_token(tok)
        if v is None or not plausible(v): continue
        if LOSS_PHRASE_RE.search(win) and v > 0: v = -v
        base = 0.0
        if DILUTED_RE.search(win): base += 3.0
        if BASIC_RE.search(win):   base += 1.0
        if QTR_RE.search(win):     base += 2.0
        cands.append(Cand(base, v, 'strict eps (narr)', 'narr', ctx=win))

    # 3) EPS tokens with “per share”
    for m in EPS_TOKEN_RE.finditer(text):
        win = text[max(0, m.start()-180): min(len(text), m.end()+220)]
        if not _window_ok(win, text): continue
        if 'per share' not in win.lower(): continue
        if NON_GAAP_RE.search(win): continue
        mnum = NUMBER_RE.search(win)
        if not mnum: continue
        tok = mnum.group(0)
        v = normalize_num_token(tok)
        if v is None or not plausible(v): continue
        if LOSS_PHRASE_RE.search(win) and v > 0: v = -v
        base = 0.0
        if DILUTED_RE.search(win): base += 3.0
        if BASIC_RE.search(win):   base += 1.0
        if QTR_RE.search(win):     base += 2.0
        cands.append(Cand(base, v, 'eps token (narr)', 'narr', ctx=win))

    # 4) generic “per share”
    for m in PER_SHARE_RE.finditer(text):
        win = text[max(0, m.start()-180): min(len(text), m.end()+220)]
        if not _window_ok(win, text): continue
        if NON_GAAP_RE.search(win): continue
        mnum = NUMBER_RE.search(win)
        if not mnum: continue
        tok = mnum.group(0)
        v = normalize_num_token(tok)
        if v is None or not plausible(v): continue
        if LOSS_PHRASE_RE.search(win) and v > 0: v = -v
        base = 0.0
        if DILUTED_RE.search(win): base += 3.0
        if BASIC_RE.search(win):   base += 1.0
        if QTR_RE.search(win):     base += 2.0
        cands.append(Cand(base, v, 'per share (narr)', 'narr', ctx=win))

    return cands

# ========= selection (GAAP-first; adjusted override is narrow & context-based) =========
def choose_best(cands: List[Cand]) -> Optional[float]:
    if not cands:
        return None

    def is_adjusted(c: Cand) -> bool:
        return bool(NON_GAAP_RE.search(c.ctx or c.label))

    gaap = [c for c in cands if not is_adjusted(c)]
    adj  = [c for c in cands if is_adjusted(c)]

    pool = gaap if gaap else cands

    if gaap and adj:
        best_gaap = max(gaap, key=lambda x: x.score)
        if abs(best_gaap.value) >= GAAP_OUTLIER_ABS:
            adj_diluted = [a for a in adj if DILUTED_RE.search((a.ctx + " " + a.label))]
            if adj_diluted:
                best_adj = max(adj_diluted, key=lambda x: x.score)
                if abs(best_adj.value) < abs(best_gaap.value):
                    return best_adj.value

    def tiebreak(c: Cand) -> Tuple:
        lb = (c.ctx + " " + c.label).lower()
        return (
            1 if DILUTED_RE.search(lb) else 0,
            1 if NET_ROW_RE.search(lb) else 0,
            1 if c.source == 'table' else 0,
            c.score
        )

    return max(pool, key=tiebreak).value

# ========= per-file =========
def extract_eps_from_html(html_bytes: bytes) -> Optional[float]:
    soup = BeautifulSoup(html_bytes, "lxml") if b"<" in html_bytes else BeautifulSoup(html_bytes, "html.parser")

    all_cands: List[Cand] = []

    for tbl in soup.find_all('table'):
        grid = build_table_grid(tbl)
        all_cands.extend(extract_from_table(grid))

    text = soup_text_bytes(html_bytes)
    all_cands.extend(extract_from_narrative(text))

    v = choose_best(all_cands)
    return v

# ========= CLI =========
def _format_eps(x: Optional[float]) -> str:
    if x is None: return ''
    if abs(x) < NEAR_ZERO: x = 0.0
    return f"{x:.2f}"

def main(argv=None):
    ap = argparse.ArgumentParser(description="EDGAR EPS parser (BeautifulSoup; quarter-locked; GAAP-first; sign-mirror).")
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