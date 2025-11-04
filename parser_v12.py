from html.parser import HTMLParser
from dataclasses import dataclass, field
from pathlib import Path
import argparse, csv, re, html
from typing import List, Optional, Tuple
from decimal import Decimal, ROUND_HALF_UP
from datetime import datetime

# Regexes
NUMBER_RE = re.compile(r'(?<![A-Za-z])(?:\$)?\(?\s*[+-]?(?:\d{1,3}(?:,\d{3})*|\d+)(?:\.\d+)?\s*\)?(?!\s*%)(?![A-Za-z])')
ADJUSTED_RE = re.compile(r'\b(adjust(ed)?|non[-\s]?gaap|pro\s*forma|core\s+earnings?)\b', re.I)
BASIC_RE    = re.compile(r'\bbasic\b', re.I)
DILUTED_RE  = re.compile(r'\bdilut(?:ed|ion)\b', re.I)
EPS_HEADER_RE  = re.compile(r'\b(earnings|income|loss)\s+per\s+share\b', re.I)
LOSS_PHRASE_RE = re.compile(r'\bloss\s+per\s+share\b|\bnet\s+loss\b', re.I)

# EXPANDED: include more per-share phrasings
PER_SHARE_RE   = re.compile(
    r'\b((?:earnings|income|loss)\s+per\s+share'
    r'|per\s+(?:basic|diluted)\s+share'
    r'|per\s+share\s+on\s+a\s+(?:diluted|basic)\s+basis'
    r'|per\s+common\s+share'
    r'|per\s+share\s+of\s+common\s+stock)\b',
    re.I
)

CURRENCY_RE    = re.compile(r'\$\s*')
STRICT_EPS_PHRASE_RE = re.compile(r'(earnings|income|loss)\s+per\s+share\b', re.I)
EPS_TOKEN_RE = re.compile(r'\bEPS\b', re.I)

# Simple narrative catcher (kept)
SIMPLE_PER_SHARE_RE = re.compile(
    r'\$?\(?\s*([+-]?\d+(?:\.\d+)?)\s*\)?\s+per\s+(?:diluted|basic)\s+share',
    re.I
)

EPS_BASIC_DILUTED_SENT_RE = re.compile(
    r'\b(basic|diluted)\s+EPS\b[^$]{0,120}?\$?\(?\s*([+-]?\d+(?:\.\d+)?)\s*\)?',
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

CONTINUING_RE = re.compile(r'\bcontinuing\s+operations\b', re.I)
DISCONT_RE    = re.compile(r'\bdiscontinued\s+operations?\b', re.I)
GAAP_RE       = re.compile(r'\bGAAP\b', re.I)
ANNUAL_RE     = re.compile(r'\b(twelve\s+months\s+ended|year\s+ended|12\s+months|fiscal\s+year)\b', re.I)
QUARTER_RE    = re.compile(r'\b(three\s+months\s+ended|quarter\s+ended|three[-\s]?month|3[-\s]?month|q[1-4])\b', re.I)

# NEW: narrative noise guards
CHANGE_RE = re.compile(r'\b(increase|decrease|changed?|up|down|improv(ed|ement)|declin(ed|e))\b', re.I)
PRIOR_RE  = re.compile(r'\b(prior[-\s]?year|year[-\s]?ago|same\s+period\s+last\s+year|previous\s+year|prior\s+period)\b', re.I)

# Hard magnitude cap for EPS
HARD_ABS_CAP = 20.0
NEAR_ZERO = 0.005

# Date parsing (for column/narrative recency)
MONTHS = r"(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*"
DATE_PATTS = [
    re.compile(rf'\b{MONTHS}\s+\d{{1,2}},\s*\d{{4}}\b', re.I),
    re.compile(rf'\b{MONTHS}\s+\d{{4}}\b', re.I),
    re.compile(r'\b\d{1,2}/\d{1,2}/\d{2,4}\b')
]

BASIC_DILUTED_PAIR_RE = re.compile(
    r'(?:income|earnings|loss)\s+per\s+share[^$]{0,120}?'
    r'(?:basic[^$]{0,60}?\$?\(?\s*([+-]?\d+(?:\.\d+)?)\s*\)?[^b]{0,120}?diluted[^$]{0,60}?\$?\(?\s*([+-]?\d+(?:\.\d+)?)\s*\)?'
    r'|diluted[^$]{0,60}?\$?\(?\s*([+-]?\d+(?:\.\d+)?)\s*\)?[^d]{0,120}?basic[^$]{0,60}?\$?\(?\s*([+-]?\d+(?:\.\d+)?)\s*\)?)',
    re.I | re.S
)

SINGLE_BASIC_OR_DILUTED_RE = re.compile(
    r'(?:income|earnings|loss)\s+per\s+share[^$]{0,140}?'
    r'(?:(basic|diluted)[^$]{0,60}?\$?\(?\s*([+-]?\d+(?:\.\d+)?)\s*\)?)',
    re.I | re.S
)

def _parse_any_date(s: str) -> Optional[datetime]:
    s = s.strip()
    for patt in DATE_PATTS:
        m = patt.search(s)
        if not m:
            continue
        frag = m.group(0)
        for fmt in ("%b %d, %Y", "%B %d, %Y", "%b %Y", "%B %Y", "%m/%d/%Y", "%m/%d/%y"):
            try:
                return datetime.strptime(frag, fmt)
            except:
                pass
    return None

def _recency_score(text_window: str) -> float:
    d = _parse_any_date(text_window)
    if not d: 
        return 0.0
    return (d.year - 1990) * 0.6 + d.timetuple().tm_yday * 0.02

# Small helpers
def _near_adjusted(text: str, start: int, end: int, radius: int = 100) -> bool:
    lo = max(0, start - radius); hi = min(len(text), end + radius)
    return ADJUSTED_RE.search(text[lo:hi]) is not None

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

# HTML helpers
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
        if tag in ('script','style'): 
            self.skip=True; return
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

# Header & scoring helpers (unchanged)
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
    if QUARTER_RE.search(cell_text): 
        score += 8.0
    if ANNUAL_RE.search(cell_text):  
        score -= 10.0
    if CONTINUING_RE.search(cell_text): 
        score -= 5.0
    if GAAP_RE.search(cell_text): 
        score += 2.0
    d = _parse_any_date(cell_text)
    if d:
        score += (d.year - 1990) * 0.6 + d.timetuple().tm_yday * 0.02
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
        if QUARTER_RE.search(t): 
            score += 12.0
        if ANNUAL_RE.search(t):  
            score -= 14.0
        if CONTINUING_RE.search(t): 
            score -= 6.0
        if GAAP_RE.search(t): score += 2.0
        d = _parse_any_date(t)
        if d:
            score += (d.year - 1990) * 0.6 + d.timetuple().tm_yday * 0.02
        score -= 0.15 * j
        if score > best[0]:
            best = (score, j)
    return best[1]

def _col_text(ci: int, header_rows: List[List[str]]) -> str:
    bits = []
    for hr in header_rows[:4]:
        if ci < len(hr):
            bits.append(hr[ci].lower())
    return ' '.join(bits)

def maybe_merge_paren_token(tok: str, row: List[str], ci: int) -> str:
    if tok.strip().startswith('(') and not tok.strip().endswith(')'):
        if ci+1 < len(row) and row[ci+1].strip().startswith(')'):
            return tok.strip() + ')'
    return tok

def row_blocklisted(rr: List[str]) -> bool:
    return bool(BLOCK_ROW_RE.search(' '.join(rr)))

# Extraction from tables
def extract_from_table(rows: List[List[str]]) -> Tuple[Optional[float], float]:
    if not rows: return (None, float('-inf'))
    header_rows = [r for r in rows[:8] if row_is_header_like(r)]
    best_col = pick_best_quarter_column(header_rows)
    candidates: List[Tuple[float,float]] = []

    eps_rows = [i for i,r in enumerate(rows) if EPS_HEADER_RE.search(' '.join(r))]
    if eps_rows:
        for idx in eps_rows:
            section_text = ' '.join(' '.join(r) for r in rows[max(0, idx-3): idx]).lower()
            section_penalty = -12.0 if ADJUSTED_RE.search(section_text) else 0.0

            band = rows[idx: min(len(rows), idx+6)]
            band_has_basic = any(BASIC_RE.search(' '.join(r).lower()) for r in band)
            band_text = ' '.join(' '.join(r).lower() for r in band)
            band_loss = bool(LOSS_PHRASE_RE.search(band_text) or re.search(r'\bnet\s+loss\b', band_text))

            row_cands = []
            for rr in band:
                if row_blocklisted(rr):
                    continue
                label = ' '.join(rr).lower()

                if CONTINUING_RE.search(label) or DISCONT_RE.search(label):
                    continue

                base = section_penalty
                if ADJUSTED_RE.search(label): 
                    base -= 12.0
                if BASIC_RE.search(label):    
                    base += 6.0
                if DILUTED_RE.search(label):  
                    base += 1.0
                if band_has_basic and not BASIC_RE.search(label) and not EPS_HEADER_RE.search(label):
                    base -= 4.0

                for ci, cell in enumerate(rr):
                    m = NUMBER_RE.search(cell)
                    if not m: 
                        continue
                    if best_col is None and header_rows:
                        hdr_bits = []
                        for hr in header_rows[:3]:
                            if ci < len(hr):
                                hdr_bits.append(hr[ci].lower())
                        col_hdr_text_tmp = ' '.join(hdr_bits)
                        if ANNUAL_RE.search(col_hdr_text_tmp):
                            continue
                    if best_col is not None and (ci not in (best_col, best_col + 1)):
                        continue
                    if ADJUSTED_RE.search(_col_text(ci, header_rows)):
                        continue

                    tok = maybe_merge_paren_token(m.group(0), rr, ci)
                    loss_ctx = band_loss or bool(LOSS_PHRASE_RE.search(label) or re.search(r'\bnet\s+loss\b', label))
                    val = normalize_num_token(tok, loss_ctx)
                    if val is None: 
                        continue
                    if abs(val) > HARD_ABS_CAP or abs(val) < NEAR_ZERO:
                        continue
                    if abs(val) >= 5.0 and not BASIC_RE.search(label):
                        continue

                    hscore = header_context_score(ci, header_rows)
                    score = base + hscore + decimal_bonus(tok) + plausibility_penalty(val) + integer_penalty(tok, val)
                    row_cands.append((score, val, hscore))

            if row_cands:
                row_cands.sort(key=lambda x: (x[2], abs(x[1]), x[0]))
                best = row_cands[-1]
                close = [c for c in row_cands if abs(c[2] - best[2]) <= 1.0]
                if close:
                    close.sort(key=lambda x: (abs(x[1]), x[2], x[0]))
                    best = close[-1]
                candidates.append((best[0], best[1]))

    if not candidates:
        for rr in rows:
            if row_blocklisted(rr):
                continue
            label = ' '.join(rr).lower()
            if not BASIC_RE.search(label):
                continue
            base = 4.0
            if ADJUSTED_RE.search(label): 
                base -= 12.0
            if CONTINUING_RE.search(label) or DISCONT_RE.search(label):
                continue
            for ci, cell in enumerate(rr):
                m = NUMBER_RE.search(cell)
                if not m: 
                    continue
                if best_col is None and header_rows:
                    hdr_bits = []
                    for hr in header_rows[:3]:
                        if ci < len(hr):
                            hdr_bits.append(hr[ci].lower())
                    col_hdr_text_tmp = ' '.join(hdr_bits)
                    if ANNUAL_RE.search(col_hdr_text_tmp):
                        continue
                if best_col is not None and (ci not in (best_col, best_col + 1)):
                    continue
                if ADJUSTED_RE.search(_col_text(ci, header_rows)):
                    continue
                tok = maybe_merge_paren_token(m.group(0), rr, ci)
                loss_ctx = bool(LOSS_PHRASE_RE.search(label))
                val = normalize_num_token(tok, loss_ctx)
                if val is None: 
                    continue
                if abs(val) > HARD_ABS_CAP or abs(val) < NEAR_ZERO:
                    continue
                hscore = header_context_score(ci, header_rows)
                score = base + hscore + decimal_bonus(tok) + plausibility_penalty(val) + integer_penalty(tok, val)
                candidates.append((score, val))

    if candidates:
        best = sorted(candidates, key=lambda x: x[0])[-1]
        return (best[1], best[0])
    return (None, float('-inf'))

# Narrative extractors (tightened)
def strip_text(html_bytes: bytes) -> str:
    s = html_bytes.decode('utf-8', errors='ignore')
    s = re.sub(r'(?is)<!--.*?-->', ' ', s)
    st = Stripper(); st.feed(s)
    t = st.text()
    t = html.unescape(t)
    t = re.sub(r'\s+', ' ', t).strip()
    return t

NET_SENT_RE = re.compile(
    r'\bnet\s+(income|earnings|loss)\b.{0,160}?\$?\(?\s*([+-]?\d+(?:\.\d+)?)\s*\)?\s+per\s+(?:diluted|basic)\s+share',
    re.I|re.S
)

BASIS_PER_SHARE_RE = re.compile(
    r'\$?\(?\s*([+-]?\d+(?:\.\d+)?)\s*\)?\s+per\s+share\s+on\s+a\s+(diluted|basic)\s+basis',
    re.I
)

def _skip_narrative_noise(ctx: str) -> bool:
    # exclude annual-only, change phrases, prior-year comparisons, discontinued ops
    if ANNUAL_RE.search(ctx) and not QUARTER_RE.search(ctx): 
        return True
    if CHANGE_RE.search(ctx): 
        return True
    if PRIOR_RE.search(ctx):  
        return True
    if DISCONT_RE.search(ctx): 
        return True
    return False

def extract_per_share_on_basis(text: str) -> Optional[float]:
    best = None
    L = len(text)
    for m in BASIS_PER_SHARE_RE.finditer(text):
        tok = m.group(1)
        s, e = m.span(1)
        if _near_adjusted(text, s, e, radius=120):  # stricter
            continue
        ctx = text[max(0, s-260): min(L, e+240)]
        if _skip_narrative_noise(ctx.lower()):
            continue
        if '.' not in tok:
            continue
        is_loss = bool(LOSS_PHRASE_RE.search(ctx.lower()))
        val = normalize_num_token(tok, is_loss)
        if val is None or abs(val) > HARD_ABS_CAP or abs(val) < NEAR_ZERO:
            continue
        score = 1.0 + (1.0 if QUARTER_RE.search(ctx.lower()) else 0.0) + _recency_score(ctx)
        score += 1e-5 * (L - s)
        if abs(val) >= 3.0 and not re.search(r'\bnet\s+(income|earnings|loss)\b', ctx, re.I):
            continue
        best = (score, val) if best is None or score > best[0] else best
    return None if best is None else best[1]

def extract_narrative_net_sentence(text: str) -> Optional[float]:
    best = None
    for m in NET_SENT_RE.finditer(text):
        kind = m.group(1).lower()
        tok  = m.group(2)
        start, end = m.span(2)
        if _near_adjusted(text, start, end, radius=120):
            continue
        win = text[max(0,start-180): min(len(text), end+180)]
        if _skip_narrative_noise(win.lower()):
            continue
        if '.' not in tok: 
            continue
        val = normalize_num_token(tok, kind=='loss')
        if val is None or abs(val) > HARD_ABS_CAP or abs(val) < NEAR_ZERO:
            continue
        base = 1.5
        if QUARTER_RE.search(win): 
            base += 1.0
        if GAAP_RE.search(win):    
            base += 0.6
        base += _recency_score(win)
        if abs(val) >= 3.0 and kind not in ('income','loss','earnings'):
            continue
        sc = base + decimal_bonus(tok) + plausibility_penalty(val) + integer_penalty(tok, val)
        best = (sc, val) if best is None or sc > best[0] else best
    return None if best is None else best[1]

def extract_eps_basic_diluted_sentence(text: str) -> Optional[float]:
    best = None
    for m in EPS_BASIC_DILUTED_SENT_RE.finditer(text):
        cls = (m.group(1) or '').lower()
        tok = m.group(2)
        s, e = m.span(2)
        if _near_adjusted(text, s, e, radius=120):
            continue
        ctx = text[max(0, s-200): min(len(text), e+200)]
        low = ctx.lower()
        if _skip_narrative_noise(low):
            continue
        if '.' not in tok:
            continue
        is_loss = bool(re.search(r'\b(net\s+)?loss\b', low))
        val = normalize_num_token(tok, is_loss)
        if val is None or abs(val) > HARD_ABS_CAP or abs(val) < NEAR_ZERO:
            continue
        # slight preference for diluted if many matches
        base = (0.2 if cls == 'diluted' else 0.0) + _recency_score(ctx)
        sc = base + decimal_bonus(tok) + plausibility_penalty(val) + integer_penalty(tok, val)
        best = (sc, val) if best is None or sc > best[0] else best
    return None if best is None else best[1]

def extract_from_narrative_strict(text: str) -> Optional[float]:
    best = None
    for m in STRICT_EPS_PHRASE_RE.finditer(text):
        win = text[m.end(): m.end() + 220]
        low = win.lower()
        mnum = NUMBER_RE.search(win)
        if not mnum:
            continue
        s_num, e_num = mnum.span()
        abs_s = m.end() + s_num
        abs_e = m.end() + e_num
        if _near_adjusted(text, abs_s, abs_e, radius=120):
            continue
        if _skip_narrative_noise(win.lower()):
            continue
        tok = mnum.group(0)
        if '.' not in tok:
            continue
        is_loss = ('loss' in m.group(0).lower()) or ('loss' in low)
        val = normalize_num_token(tok, is_loss)
        if val is None or abs(val) > 20 or abs(val) < NEAR_ZERO:
            continue
        if abs(val) >= 3.0 and not re.search(r'\bnet\s+(income|earnings|loss)\b', win, re.I):
            continue
        base = 0.5
        if 'dilut' in low: 
            base += 0.75
        if 'basic' in low: 
            base += 0.25
        base += _recency_score(win)
        score = base + 0.5 + plausibility_penalty(val) + integer_penalty(tok, val)
        best = (score, val) if best is None or score > best[0] else best
    return None if best is None else best[1]

def extract_from_text_eps_token(text: str) -> Optional[float]:
    cands: List[Tuple[float, float]] = []
    for m in EPS_TOKEN_RE.finditer(text):
        start = max(0, m.start()-180); end = min(len(text), m.end()+220)
        window = text[start:end]
        low = window.lower()
        if _skip_narrative_noise(low):
            continue
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
        after = text[m.end(): min(len(text), m.end()+200)]
        mnum = NUMBER_RE.search(after) or NUMBER_RE.search(window)
        if not mnum: 
            continue
        abs_s = (m.end() + mnum.start()) if mnum.re.pattern in after else (start + mnum.start())
        abs_e = abs_s + len(mnum.group(0))
        if _near_adjusted(text, abs_s, abs_e, radius=120):
            continue
        tok = mnum.group(0)
        if '.' not in tok:
            continue
        val = normalize_num_token(tok, is_loss)
        if val is None or abs(val) > 20 or abs(val) < NEAR_ZERO:
            continue
        if abs(val) >= 3.0 and not re.search(r'\bnet\s+(income|earnings|loss)\b', window, re.I):
            continue
        score = base + _recency_score(window) + decimal_bonus(tok) + plausibility_penalty(val) + integer_penalty(tok, val)
        cands.append((score, val))

    if not cands:
        return None
    cands.sort(key=lambda x: x[0])
    return cands[-1][1]

def extract_from_text(text: str) -> Optional[float]:
    cands: List[Tuple[float, float]] = []
    for m in PER_SHARE_RE.finditer(text):
        s = m.start(); e = m.end()
        start = max(0, s-200); end = min(len(text), e+220)
        before = text[start:s]
        after  = text[e:end]

        mnum = NUMBER_RE.search(after)
        abs_s = abs_e = None
        if mnum:
            abs_s = e + mnum.start(); abs_e = e + mnum.end()
        else:
            prev = list(NUMBER_RE.finditer(before))
            if prev:
                mnum = prev[-1]
                abs_s = start + mnum.start(); abs_e = start + mnum.end()
        if not mnum:
            mnum = NUMBER_RE.search(text[start:end])
            if mnum:
                abs_s = start + mnum.start(); abs_e = start + mnum.end()
        if not mnum:
            continue

        if _near_adjusted(text, abs_s, abs_e, radius=120):
            continue

        tok = mnum.group(0)
        if '.' not in tok:
            continue

        ctx = (before + " " + after)
        low = ctx.lower()
        if _skip_narrative_noise(low):
            continue

        is_loss = bool(LOSS_PHRASE_RE.search(low))
        val = normalize_num_token(tok, is_loss)
        if val is None or abs(val) > 20 or abs(val) < NEAR_ZERO:
            continue

        if abs(val) >= 3.0 and not re.search(r'\bnet\s+(income|earnings|loss)\b', low, re.I):
            continue

        base = 0.0
        if BASIC_RE.search(low):    
            base += 2.0
        if DILUTED_RE.search(low):  
            base += 1.0
        if QUARTER_RE.search(low):  
            base += 1.0
        if ADJUSTED_RE.search(low): 
            base -= 2.0
        base += _recency_score(ctx)

        score = base + decimal_bonus(tok) + plausibility_penalty(val) + integer_penalty(tok, val)
        cands.append((score, val))

    if not cands:
        return None
    cands.sort(key=lambda x: x[0])
    return cands[-1][1]

def extract_simple_per_share(text: str) -> Optional[float]:
    best = None

    # "$X per (basic|diluted) share"
    for m in SIMPLE_PER_SHARE_RE.finditer(text):
        tok = m.group(1)
        s, e = m.span(1)
        if _near_adjusted(text, s, e, radius=120):
            continue
        ctx_lo = max(0, s - 260); ctx_hi = min(len(text), e + 120)
        ctx = text[ctx_lo:ctx_hi]
        low = ctx.lower()
        if _skip_narrative_noise(low):
            continue
        is_loss = bool(re.search(r'\b(net\s+)?loss\b', low))
        if '.' not in tok:
            continue
        val = normalize_num_token(tok, is_loss)
        if val is None or abs(val) > HARD_ABS_CAP or abs(val) < NEAR_ZERO:
            continue
        if abs(val) >= 3.0 and not re.search(r'\bnet\s+(income|earnings|loss)\b', low, re.I):
            continue
        score = 1.0 + (1.0 if QUARTER_RE.search(low) else 0.0) + _recency_score(ctx)
        best = (score, val) if best is None or score > best[0] else best

    for m in re.finditer(r'\$?\(?\s*([+-]?\d+(?:\.\d+)?)\s*\)?\s+per\s+common\s+share', text, re.I):
        tok = m.group(1)
        s, e = m.span(1)
        if _near_adjusted(text, s, e, radius=120):
            continue
        ctx_lo = max(0, s - 260); ctx_hi = min(len(text), e + 120)
        ctx = text[ctx_lo:ctx_hi]
        low = ctx.lower()
        if _skip_narrative_noise(low):
            continue
        is_loss = bool(re.search(r'\b(net\s+)?loss\b', low))
        if '.' not in tok:
            continue
        val = normalize_num_token(tok, is_loss)
        if val is None or abs(val) > HARD_ABS_CAP or abs(val) < NEAR_ZERO:
            continue
        if abs(val) >= 3.0 and not re.search(r'\bnet\s+(income|earnings|loss)\b', low, re.I):
            continue
        score = 1.0 + (1.0 if QUARTER_RE.search(low) else 0.0) + _recency_score(ctx)
        best = (score, val) if best is None or score > best[0] else best

    return None if best is None else best[1]

def extract_basic_diluted_inline(text: str) -> Optional[float]:
    """
    Catch lines like:
      '... income per share - Basic $0.26 - Diluted $0.25 ...'
    Prefer Diluted if both present; infer loss if 'loss' nearby.
    """
    # Paired case (both Basic and Diluted on same line)
    for m in BASIC_DILUTED_PAIR_RE.finditer(text):
        s, e = m.span()
        if _near_adjusted(text, s, e, radius=100):
            continue
        win = text[max(0, s-160): min(len(text), e+160)]
        if ANNUAL_RE.search(win) and not QUARTER_RE.search(win):
            continue
        low = win.lower()
        is_loss = bool(re.search(r'\b(net\s+)?loss\b', low))
        bs = m.group(1)
        dl = m.group(2)
        dl2 = m.group(3)
        bs2 = m.group(4)
        basic_tok = bs or bs2
        diluted_tok = dl or dl2
        tok = diluted_tok or basic_tok
        if not tok or '.' not in tok:
            continue
        val = normalize_num_token(tok, is_loss)
        if val is None or abs(val) < NEAR_ZERO or abs(val) > HARD_ABS_CAP:
            continue
        return val

    # Single case (only Basic or only Diluted appears under EPS header)
    for m in SINGLE_BASIC_OR_DILUTED_RE.finditer(text):
        s, e = m.span()
        if _near_adjusted(text, s, e, radius=100):
            continue
        win = text[max(0, s-160): min(len(text), e+160)]
        if ANNUAL_RE.search(win) and not QUARTER_RE.search(win):
            continue
        cls = (m.group(1) or '').lower()
        tok = m.group(2)
        if not tok or '.' not in tok:
            continue
        low = win.lower()
        is_loss = bool(re.search(r'\b(net\s+)?loss\b', low))
        val = normalize_num_token(tok, is_loss)
        if val is None or abs(val) < NEAR_ZERO or abs(val) > HARD_ABS_CAP:
            continue
        return val
    return None

# Orchestrator (order preserved, with new EPS Basic/Diluted catcher)
def extract_eps_from_html(html_bytes: bytes) -> Optional[float]:
    s = html_bytes.decode('utf-8', errors='ignore')
    s = re.sub(r'(?is)<!--.*?-->', ' ', s)
    tp = TableParser(); tp.feed(s)
    best_val=None; best_score=float('-inf')
    for t in tp.tables:
        v, sc = extract_from_table(t.rows)
        if v is not None and sc > best_score:
            best_val, best_score = v, sc
    if best_val is not None:
        return best_val

    text = strip_text(html_bytes)

    # 1) Net sentence
    v = extract_narrative_net_sentence(text)
    if v is not None:
        return v

    # 2) NEW: lines like "Diluted EPS ... $X"
    v = extract_eps_basic_diluted_sentence(text)
    if v is not None:
        return v

    # 3) inline "Basic ... Diluted ..." catcher (under EPS header wording)
    v = extract_basic_diluted_inline(text)
    if v is not None:
        return v

    # 4) simple per-share
    v = extract_simple_per_share(text)
    if v is not None:
        return v

    # 5) strict EPS phrase
    v = extract_from_narrative_strict(text)
    if v is not None:
        return v

    # 6) EPS token
    v = extract_from_text_eps_token(text)
    if v is not None:
        if abs(v) > 3 and not re.search(r'(earnings|income|loss)\s+per\s+share|per\s+(?:basic|diluted)\s+share', text, re.I):
            v = None
        if v is not None:
            return v

    # 7) generic per-share
    v = extract_from_text(text)
    if v is not None:
        if abs(v) > 3 and not re.search(r'(earnings|income|loss)\s+per\s+share|per\s+(?:basic|diluted)\s+share', text, re.I):
            v = None
        if v is not None:
            return v

    return None

# ======================
# Output formatting
# ======================
def _fmt_two_decimals_rounded(x: float) -> str:
    q = Decimal(str(x)).quantize(Decimal('0.01'), rounding=ROUND_HALF_UP)
    if abs(q) < Decimal('0.005'):
        return '0.00'
    s = f"{q:.2f}"
    return '0.00' if s in ('-0.00', '0.00') and abs(float(q)) < 0.005 else s

# ======================
# CLI
# ======================
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
                w.writerow([fp.name, '' if eps is None else _fmt_two_decimals_rounded(eps)])
            except Exception:
                w.writerow([fp.name, ''])

if __name__ == '__main__':
    main()