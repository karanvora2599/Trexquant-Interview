#!/usr/bin/env python3
"""
EDGAR EPS parser (GAAP, latest quarterly; prefer Basic over Diluted)

Usage:
  python parser_v7.py /path/to/Training_Filings /path/to/output.csv
"""

from __future__ import annotations

from html.parser import HTMLParser
from dataclasses import dataclass, field
from pathlib import Path
import argparse
import csv
import html
import re
from typing import List, Optional, Tuple

# ---------------- Regexes (NOTE: single backslashes inside raw strings) ----------------
NUMBER_RE      = re.compile(r'(?<![A-Za-z])\$?\(?\s*[+-]?(?:\d{1,3}(?:,\d{3})*|\d+)(?:\.\d+)?\s*\)?(?![A-Za-z%])')
ADJ_RE         = re.compile(r'\b(adjust(?:ed|ment)?|non[-\s]?gaap|pro\s*forma|core\s+earnings?)\b', re.I)
BASIC_RE       = re.compile(r'\bbasic\b', re.I)
DIL_RE         = re.compile(r'\bdilut(?:ed|ion)\b', re.I)
EPS_HDR_RE     = re.compile(r'\b(earnings|income|loss)\s+per\s+share\b', re.I)
LOSS_RE        = re.compile(r'\b(loss(?:es)?\s+per\s+share|net\s+loss(?:\s+per\s+share)?)\b', re.I)
PER_SHARE_RE   = re.compile(r'\b((?:earnings|income|loss)\s+per\s+share|per\s+(?:basic|diluted)\s+share)\b', re.I)
EPS_TOKEN_RE   = re.compile(r'\beps\b', re.I)
CURR_RE        = re.compile(r'\$\s*')
ANNUAL_RE      = re.compile(r'(twelve\s+months|12\s+months|year\s+ended|annual|ytd)', re.I)
QTR_RE         = re.compile(r'(three\s+months|quarter\s+ended|q[1-4]\b|three-month|3-month|first\s+quarter|second\s+quarter|third\s+quarter|fourth\s+quarter)', re.I)
MONTH_RE       = re.compile(r'(jan|feb|mar|apr|may|jun|jul|aug|sep|sept|oct|nov|dec)[a-z]*', re.I)
YEAR_RE        = re.compile(r'\b(19\d{2}|20\d{2})\b')

BLOCK_ROW_RE = re.compile(
    r'(weighted[-\s]?average\s+shares|average\s+shares|shares\s+outstanding|common\s+shares|'
    r'basic\s+and\s+diluted\s+shares|revenue|net\s+sales|sales|ebitda|adjusted\s+ebitda|cash|cash\s+flow|'
    r'segment|operating\s+margin|gross\s+margin|percent|dividends?|cash\s+dividends?|book\s+value|'
    r'net\s+asset\s+value|nav\b)',
    re.I
)

EPS_ABS_CAP = 30.0  # drop absurd values

MONTH2NUM = {
    'jan':1,'feb':2,'mar':3,'apr':4,'may':5,'jun':6,
    'jul':7,'aug':8,'sep':9,'sept':9,'oct':10,'nov':11,'dec':12
}

# ---------------- HTML helpers ----------------
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
        self.in_table = self.in_row = self.in_cell = self.skip = False
        self.cur_table: Optional[Table] = None
        self.cur_row: List[str] = []
        self.cur_cell: List[str] = []
    def handle_starttag(self, tag, attrs):
        if tag in ('script','style'):
            self.skip = True; return
        if tag == 'table':
            self.in_table = True; self.cur_table = Table()
        elif tag == 'tr' and self.in_table:
            self.in_row = True; self.cur_row = []
        elif tag in ('td','th') and self.in_row:
            self.in_cell = True; self.cur_cell = []
    def handle_endtag(self, tag):
        if tag in ('script','style'):
            self.skip = False
        if tag in ('td','th') and self.in_cell:
            txt = ' '.join(' '.join(self.cur_cell).split())
            self.cur_row.append(txt)
            self.in_cell = False; self.cur_cell = []
        elif tag == 'tr' and self.in_row:
            if any(c.strip() for c in self.cur_row):
                assert self.cur_table is not None
                self.cur_table.rows.append(self.cur_row)
            self.in_row = False; self.cur_row = []
        elif tag == 'table' and self.in_table:
            if self.cur_table and self.cur_table.rows:
                self.tables.append(self.cur_table)
            self.in_table = False; self.cur_table = None
    def handle_data(self, data):
        if not self.skip and self.in_cell:
            self.cur_cell.append(data)

# ---------------- Table utilities ----------------
def row_is_header_like(r: List[str]) -> bool:
    non_num = sum(1 for c in r if not NUMBER_RE.search(c))
    return non_num >= max(1, int(0.6*len(r)))

def header_rows_of(table_rows: List[List[str]]) -> List[List[str]]:
    return [r for r in table_rows[:8] if row_is_header_like(r)]

def stacked_header_text(header_rows: List[List[str]], j: int) -> str:
    return ' '.join(hr[j].lower() for hr in header_rows if j < len(hr))

@dataclass
class ColInfo:
    idx: int
    text: str
    is_basic: bool
    is_diluted: bool
    is_quarter: bool
    is_annual: bool
    years: List[int]
    month: Optional[int]
    recency_score: float

def parse_month(text: str) -> Optional[int]:
    m = MONTH_RE.search(text)
    if not m: return None
    return MONTH2NUM.get(m.group(1).lower())

def build_colinfo(header_rows: List[List[str]], max_cols: int) -> List[ColInfo]:
    cols: List[ColInfo] = []
    for j in range(max_cols):
        t = stacked_header_text(header_rows, j)
        if not t.strip():
            cols.append(ColInfo(j, t, False, False, False, False, [], None, -1e9))
            continue
        years = [int(m.group(1)) for m in YEAR_RE.finditer(t)]
        mon = parse_month(t)
        is_q = bool(QTR_RE.search(t))
        is_a = bool(ANNUAL_RE.search(t))
        is_b = bool(BASIC_RE.search(t))
        is_d = bool(DIL_RE.search(t))
        # recency: year dominates, then month; prefer quarter; mild left penalty
        sc = 0.0
        if years:
            sc += (max(years) - 1990) * 2.0
        if mon:
            sc += mon / 12.0
        if is_q: sc += 3.0
        if is_a: sc -= 5.0
        sc -= 0.15 * j
        cols.append(ColInfo(j, t, is_b, is_d, is_q, is_a, years, mon, sc))
    return cols

def choose_quarter_cols(colinfo: List[ColInfo]) -> List[int]:
    # prefer explicit quarter columns; else any dated (month/year) non-annual; else any non-annual (except col 0)
    q = [c for c in colinfo if c.is_quarter and not c.is_annual and c.idx != 0]
    if q:
        q.sort(key=lambda c: c.recency_score, reverse=True)
        return [c.idx for c in q]
    d = [c for c in colinfo if not c.is_annual and (c.years or c.month) and c.idx != 0]
    if d:
        d.sort(key=lambda c: c.recency_score, reverse=True)
        return [c.idx for c in d]
    a = [c for c in colinfo if not c.is_annual and c.idx != 0]
    a.sort(key=lambda c: c.recency_score, reverse=True)
    return [c.idx for c in a]

def first_decimal(cell: str) -> Optional[str]:
    m = NUMBER_RE.search(cell)
    if not m:
        return None
    tok = m.group(0)
    return tok if '.' in tok else None

def normalize_token(tok: str, loss_ctx: bool) -> Optional[float]:
    t = tok.strip()
    neg = t.startswith('(') and t.endswith(')')
    t = t.strip('()').replace(',', '')
    t = CURR_RE.sub('', t)
    try:
        v = float(t)
    except Exception:
        return None
    if neg:
        v = -abs(v)
    if loss_ctx and v > 0:
        v = -v
    if abs(v) > EPS_ABS_CAP:
        return None
    return v

def eps_band_loss(rows: List[List[str]], start: int, end: int) -> bool:
    lo = max(0, start-2); hi = min(len(rows), end+1)
    window = ' '.join(' '.join(rows[k]) for k in range(lo, hi)).lower()
    return bool(LOSS_RE.search(window)) or (' earnings (loss) ' in f' {window} ')

# ---------------- Extraction ----------------
def extract_from_table(rows: List[List[str]]) -> Optional[float]:
    if not rows:
        return None
    hdrs = header_rows_of(rows)
    max_cols = max(len(r) for r in rows)
    colinfo = build_colinfo(hdrs, max_cols)
    quarter_cols = choose_quarter_cols(colinfo)

    basic_cols   = [c.idx for c in colinfo if c.is_basic]
    diluted_cols = [c.idx for c in colinfo if c.is_diluted]

    # EPS header rows, then scan a tight band around them
    eps_rows = [i for i, r in enumerate(rows) if EPS_HDR_RE.search(' '.join(r))]
    for idx in eps_rows:
        start = max(0, idx-2); end = min(len(rows), idx+7)
        loss = eps_band_loss(rows, idx, end-1)

        basic_val = diluted_val = None
        for i in range(start, end):
            r = rows[i]
            row_txt = ' '.join(r).lower()
            if BLOCK_ROW_RE.search(row_txt) or ADJ_RE.search(row_txt):
                continue

            # CASE A: Basic/Diluted as row labels
            if BASIC_RE.search(row_txt) or DIL_RE.search(row_txt):
                for j in quarter_cols:
                    if j >= len(r): continue
                    tok = first_decimal(r[j])
                    if not tok: continue
                    v = normalize_token(tok, loss)
                    if v is None: continue
                    if BASIC_RE.search(row_txt): basic_val = v
                    if DIL_RE.search(row_txt):   diluted_val = v

            # CASE B: Basic/Diluted as column headers; current row is EPS label
            has_basic_or_dil_cols = bool(basic_cols or diluted_cols)
            if basic_val is None and diluted_val is None and has_basic_or_dil_cols:
                if EPS_HDR_RE.search(row_txt) or ' per share' in row_txt or 'net income per' in row_txt or 'earnings (loss) per' in row_txt:
                    # sort columns by recency preference
                    col_rank = sorted(colinfo, key=lambda c: c.recency_score, reverse=True)
                    # prefer Basic column value first
                    pref_basic_cols = [j for j in basic_cols if j in quarter_cols] or basic_cols
                    for c in col_rank:
                        if c.idx in pref_basic_cols and c.idx < len(r):
                            tok = first_decimal(r[c.idx])
                            if tok:
                                v = normalize_token(tok, loss)
                                if v is not None:
                                    basic_val = v
                                    break
                    if basic_val is None:
                        pref_dil_cols = [j for j in diluted_cols if j in quarter_cols] or diluted_cols
                        for c in col_rank:
                            if c.idx in pref_dil_cols and c.idx < len(r):
                                tok = first_decimal(r[c.idx])
                                if tok:
                                    v = normalize_token(tok, loss)
                                    if v is not None:
                                        diluted_val = v
                                        break

        if basic_val is not None:
            return basic_val
        if diluted_val is not None:
            return diluted_val

    # Fallback: Basic/Diluted rows without an explicit EPS header row
    for i, r in enumerate(rows):
        row_txt = ' '.join(r).lower()
        if (BASIC_RE.search(row_txt) or DIL_RE.search(row_txt)) and not ADJ_RE.search(row_txt):
            loss = eps_band_loss(rows, i, min(i+2, len(rows)-1))
            for j in quarter_cols:
                if j >= len(r): continue
                tok = first_decimal(r[j])
                if not tok: continue
                v = normalize_token(tok, loss)
                if v is None: continue
                return v

    return None

# ---------------- Narrative (guarded) ----------------
def strip_text(html_bytes: bytes) -> str:
    s = html_bytes.decode('utf-8', errors='ignore')
    s = re.sub(r'(?is)<!--.*?-->', ' ', s)
    st = Stripper(); st.feed(s)
    t = st.text()
    t = html.unescape(t)
    t = re.sub(r'\s+', ' ', t).strip()
    return t

def extract_from_narrative(text: str) -> Optional[float]:
    best: Optional[Tuple[float, float]] = None
    for m in EPS_HDR_RE.finditer(text):
        ctx = text[max(0, m.start()-140): m.end()+160].lower()
        if ANNUAL_RE.search(ctx) or not QTR_RE.search(ctx):
            continue
        mnum = NUMBER_RE.search(text[m.end(): m.end()+160])
        if not mnum:
            continue
        tok = mnum.group(0)
        if '.' not in tok:
            continue
        loss = bool(LOSS_RE.search(ctx)) or (' loss ' in f' {ctx} ')
        v = normalize_token(tok, loss)
        if v is None:
            continue
        score = 1.0 + (2.0 if BASIC_RE.search(ctx) else 0.0) + (1.0 if DIL_RE.search(ctx) else 0.0)
        if (best is None) or (score > best[0]):
            best = (score, v)
    return None if best is None else best[1]

# ---------------- Driver ----------------
def extract_eps_from_html(html_bytes: bytes) -> Optional[float]:
    s = html_bytes.decode('utf-8', errors='ignore')
    s = re.sub(r'(?is)<!--.*?-->', ' ', s)
    tp = TableParser(); tp.feed(s)

    # Tables first
    for t in tp.tables:
        v = extract_from_table(t.rows)
        if v is not None:
            return v

    # Narrative (guarded)
    text = strip_text(html_bytes)
    v = extract_from_narrative(text)
    if v is not None:
        return v

    # Weak fallback around "per share"/"EPS" requiring quarterly cues and rejecting annual
    weak = None
    for regex in (PER_SHARE_RE, EPS_TOKEN_RE):
        for m in regex.finditer(text):
            win = text[max(0, m.start()-120): m.end()+160]
            low = win.lower()
            if ADJ_RE.search(low) or ANNUAL_RE.search(low) or not QTR_RE.search(low):
                continue
            loss = bool(LOSS_RE.search(low)) or (' loss ' in f' {low} ')
            mnum = NUMBER_RE.search(win)
            if not mnum: continue
            tok = mnum.group(0)
            if '.' not in tok: continue
            v = normalize_token(tok, loss)
            if v is None: continue
            weak = v
    return weak

def run_dir(input_dir: Path, output_csv: Path) -> None:
    files = sorted(p for p in input_dir.iterdir() if p.suffix.lower() in ('.html', '.htm'))
    with output_csv.open('w', newline='', encoding='utf-8') as f:
        w = csv.writer(f)
        w.writerow(['filename', 'EPS'])
        for fp in files:
            try:
                eps = extract_eps_from_html(fp.read_bytes())
                w.writerow([fp.name, '' if eps is None else f'{eps:.2f}'.rstrip('0').rstrip('.')])
            except Exception:
                w.writerow([fp.name, ''])

def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description='EDGAR EPS parser (quarterly GAAP; Basic over Diluted; table-first).')
    ap.add_argument('input_dir', help='Directory with .html/.htm filings')
    ap.add_argument('output_csv', help='Output CSV path')
    args = ap.parse_args(argv)
    run_dir(Path(args.input_dir), Path(args.output_csv))
    return 0

if __name__ == '__main__':
    raise SystemExit(main())