// eps_parser.cpp
#include <gumbo.h>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <regex>
#include <string>
#include <vector>
#include <optional>
#include <tuple>
#include <chrono>
#include <cctype>
#include <algorithm>

using std::string;
using std::vector;
using std::optional;
using std::tuple;
using std::size_t;
namespace fs = std::filesystem;

// ----------------- Regexes (mirroring Python) -----------------
static const std::regex NUMBER_RE(R"((?<![A-Za-z])(?:\$)?\(?\s*[+-]?(?:\d{1,3}(?:,\d{3})*|\d+)(?:\.\d+)?\s*\)?(?![A-Za-z]))");
static const std::regex ADJUSTED_RE(R"(\b(adjust(ed)?|non[-\s]?gaap|pro\s*forma|core\s+earnings?)\b)", std::regex::icase);
static const std::regex BASIC_RE(R"(\bbasic\b)", std::regex::icase);
static const std::regex DILUTED_RE(R"(\bdilut(?:ed|ion)\b)", std::regex::icase);
static const std::regex EPS_HEADER_RE(R"(\b(earnings|income|loss)\s+per\s+share\b)", std::regex::icase);
static const std::regex LOSS_PHRASE_RE(R"(\bloss\s+per\s+share\b|\bnet\s+loss\b)", std::regex::icase);
static const std::regex PER_SHARE_RE(R"(\b((?:earnings|income|loss)\s+per\s+share|per\s+(?:basic|diluted)\s+share)\b)", std::regex::icase);
static const std::regex CURRENCY_RE(R"(\$\s*)");
static const std::regex STRICT_EPS_PHRASE_RE(R"((earnings|income|loss)\s+per\s+share\b)", std::regex::icase);
static const std::regex EPS_TOKEN_RE(R"(\bEPS\b)", std::regex::icase);
static const std::regex BLOCK_ROW_RE(
    R"((weighted[-\s]?average\s+shares|average\s+shares|shares\s+outstanding|common\s+shares|)"
    R"(basic\s+and\s+diluted\s+shares|revenue|net\s+sales|sales|ebitda|adjusted\s+ebitda|cash|cash\s+flow|)"
    R"(segment|operating\s+margin|gross\s+margin|percent|%|dividends?|cash\s+dividends?|)"
    R"(book\s+value|net\s+asset\s+value|nav\b))",
    std::regex::icase
);
static constexpr double HARD_ABS_CAP = 20.0;

// ----------------- String utils -----------------
static inline string to_lower(string s) {
    std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c){ return std::tolower(c); });
    return s;
}

static inline bool regex_search_icase(const string& s, const std::regex& re) {
    return std::regex_search(s, re);
}

static inline string regex_strip_currency(const string& s) {
    return std::regex_replace(s, CURRENCY_RE, "");
}

static inline string trim(const string& in) {
    size_t i = 0, j = in.size();
    while (i < j && std::isspace((unsigned char)in[i])) ++i;
    while (j > i && std::isspace((unsigned char)in[j-1])) --j;
    return in.substr(i, j - i);
}

static inline string collapse_ws(const string& s) {
    string out; out.reserve(s.size());
    bool in_space = false;
    for (char c : s) {
        if (std::isspace((unsigned char)c)) {
            if (!in_space) { out.push_back(' '); in_space = true; }
        } else { out.push_back(c); in_space = false; }
    }
    return trim(out);
}

// ----------------- Numeric normalization & scoring -----------------
static optional<double> normalize_num_token(string tok, bool loss_ctx) {
    string st = trim(tok);
    bool neg_paren = (!st.empty() && st.front() == '(' && st.back() == ')');
    // allow merging cases handled elsewhere
    if (!st.empty() && st.front() == '(' && (st.size() < 2 || st.back() != ')')) {
        // leave as-is; parse will likely fail; caller merges if needed
    }
    // strip parens/commas/$
    if (!st.empty() && st.front() == '(' && st.back() == ')') {
        st = st.substr(1, st.size()-2);
        neg_paren = true;
    }
    st.erase(std::remove(st.begin(), st.end(), ','), st.end());
    st = regex_strip_currency(st);
    try {
        double v = std::stod(st);
        if (neg_paren) v = -std::abs(v);
        if (loss_ctx && v > 0) v = -v;
        return v;
    } catch (...) {
        return std::nullopt;
    }
}

static double plausibility_penalty(double v) {
    double av = std::abs(v);
    if (av <= 20) return 0.0;
    if (av <= 50) return -3.0;
    if (av <= 100) return -6.0;
    return -10.0;
}

static double decimal_bonus(const string& tok) { return (tok.find('.') != string::npos) ? 0.5 : 0.0; }

static double integer_penalty(const string& tok, double val) {
    if (tok.find('.') != string::npos) return 0.0;
    return (std::abs(val) >= 10) ? -2.0 : -0.5;
}

// ----------------- Gumbo helpers -----------------
static bool is_tag(const GumboNode* node, GumboTag tag) {
    return node && node->type == GUMBO_NODE_ELEMENT && node->v.element.tag == tag;
}
static bool is_script_or_style(const GumboNode* node) {
    if (!node || node->type != GUMBO_NODE_ELEMENT) return false;
    GumboTag t = node->v.element.tag;
    return t == GUMBO_TAG_SCRIPT || t == GUMBO_TAG_STYLE;
}

static void text_from_node(const GumboNode* node, string& out) {
    if (!node) return;
    if (node->type == GUMBO_NODE_TEXT || node->type == GUMBO_NODE_CDATA) {
        out += node->v.text.text;
        out.push_back(' ');
        return;
    }
    if (node->type != GUMBO_NODE_ELEMENT) return;
    if (is_script_or_style(node)) return;

    const GumboVector* children = &node->v.element.children;
    for (unsigned i = 0; i < children->length; ++i) {
        text_from_node(static_cast<GumboNode*>(children->data[i]), out);
    }
}

static string extract_visible_text(GumboOutput* out) {
    string raw; text_from_node(out->root, raw);
    return collapse_ws(raw);
}

// Extract all <table> → rows → cells (visible text only)
struct Table { vector<vector<string>> rows; };

static void collect_tables(const GumboNode* node, vector<Table>& out_tables);

static vector<string> collect_row_cells(const GumboNode* tr) {
    vector<string> cells;
    const GumboVector* children = &tr->v.element.children;
    for (unsigned i = 0; i < children->length; ++i) {
        const GumboNode* n = static_cast<GumboNode*>(children->data[i]);
        if (n->type == GUMBO_NODE_ELEMENT &&
            (n->v.element.tag == GUMBO_TAG_TD || n->v.element.tag == GUMBO_TAG_TH)) {
            string txt; text_from_node(n, txt);
            cells.push_back(collapse_ws(txt));
        }
    }
    return cells;
}

static vector<vector<string>> collect_table_rows(const GumboNode* table) {
    vector<vector<string>> rows;
    const GumboVector* children = &table->v.element.children;
    for (unsigned i = 0; i < children->length; ++i) {
        const GumboNode* n = static_cast<GumboNode*>(children->data[i]);
        if (n->type == GUMBO_NODE_ELEMENT &&
            (n->v.element.tag == GUMBO_TAG_TR || n->v.element.tag == GUMBO_TAG_TBODY ||
             n->v.element.tag == GUMBO_TAG_THEAD || n->v.element.tag == GUMBO_TAG_TFOOT)) {
            if (n->v.element.tag == GUMBO_TAG_TR) {
                auto cells = collect_row_cells(n);
                bool any = false;
                for (auto& c : cells) if (!trim(c).empty()) { any = true; break; }
                if (any) rows.emplace_back(std::move(cells));
            } else {
                // section: recurse into children
                const GumboVector* kids = &n->v.element.children;
                for (unsigned j = 0; j < kids->length; ++j) {
                    const GumboNode* tr = static_cast<GumboNode*>(kids->data[j]);
                    if (tr->type == GUMBO_NODE_ELEMENT && tr->v.element.tag == GUMBO_TAG_TR) {
                        auto cells = collect_row_cells(tr);
                        bool any = false;
                        for (auto& c : cells) if (!trim(c).empty()) { any = true; break; }
                        if (any) rows.emplace_back(std::move(cells));
                    }
                }
            }
        }
    }
    return rows;
}

static void collect_tables(const GumboNode* node, vector<Table>& out_tables) {
    if (!node) return;
    if (is_tag(node, GUMBO_TAG_TABLE)) {
        Table t;
        t.rows = collect_table_rows(node);
        if (!t.rows.empty()) out_tables.emplace_back(std::move(t));
    }
    if (node->type == GUMBO_NODE_ELEMENT) {
        const GumboVector* children = &node->v.element.children;
        for (unsigned i = 0; i < children->length; ++i) {
            collect_tables(static_cast<GumboNode*>(children->data[i]), out_tables);
        }
    }
}

// ----------------- Header & scoring helpers (ports) -----------------
static bool row_is_header_like(const vector<string>& r) {
    int non_num = 0;
    for (auto& c : r) {
        if (!std::regex_search(c, NUMBER_RE)) non_num++;
    }
    int thresh = std::max(1, (int)std::floor(0.6 * r.size()));
    return non_num >= thresh;
}

static double header_context_score(int col_index, const vector<vector<string>>& header_rows) {
    if (header_rows.empty()) return 0.0;
    double score = 0.0;
    vector<string> cells;
    for (size_t i = 0; i < std::min<size_t>(3, header_rows.size()); ++i) {
        if (col_index >= 0 && col_index < (int)header_rows[i].size())
            cells.push_back(to_lower(header_rows[i][col_index]));
    }
    string cell_text;
    for (auto& s : cells) { cell_text += s; cell_text.push_back(' '); }

    if (cell_text.find("three months ended") != string::npos ||
        cell_text.find("quarter ended") != string::npos ||
        cell_text.find("three-month") != string::npos ||
        cell_text.find("3-month") != string::npos) score += 8.0;

    if (cell_text.find("twelve months ended") != string::npos ||
        cell_text.find("year ended") != string::npos ||
        cell_text.find("12 months") != string::npos) score -= 3.0;

    static const std::regex year_re(R"(\b(19\d{2}|20\d{2})\b)");
    std::sregex_iterator it(cell_text.begin(), cell_text.end(), year_re), end;
    int best_year = -1;
    for (; it != end; ++it) {
        best_year = std::max(best_year, std::stoi((*it)[1]));
    }
    if (best_year != -1) score += (best_year - 1990) / 5.0;

    score -= 0.2 * col_index;
    return score;
}

static optional<int> pick_best_quarter_column(const vector<vector<string>>& header_rows) {
    if (header_rows.empty()) return std::nullopt;
    size_t max_cols = 0;
    for (auto& r : header_rows) max_cols = std::max(max_cols, r.size());
    double best_score = -1e18;
    optional<int> best_col = std::nullopt;

    static const std::regex year_re(R"(\b(19\d{2}|20\d{2})\b)");
    for (int j = 0; j < (int)max_cols; ++j) {
        string t;
        for (size_t i = 0; i < std::min<size_t>(4, header_rows.size()); ++i) {
            if (j < (int)header_rows[i].size())
                t += to_lower(header_rows[i][j]) + " ";
        }
        if (trim(t).empty()) continue;

        double score = 0.0;
        if (t.find("three months ended") != string::npos ||
            t.find("quarter ended") != string::npos ||
            t.find("three-month") != string::npos ||
            t.find("3-month") != string::npos) score += 12.0;

        if (t.find("twelve months ended") != string::npos ||
            t.find("year ended") != string::npos ||
            t.find("12 months") != string::npos) score -= 8.0;

        std::sregex_iterator it(t.begin(), t.end(), year_re), end;
        int best_year = -1;
        for (; it != end; ++it) best_year = std::max(best_year, std::stoi((*it)[1]));
        if (best_year != -1) score += (best_year - 1990) / 2.0;

        score -= 0.15 * j;
        if (score > best_score) { best_score = score; best_col = j; }
    }
    return best_col;
}

static string maybe_merge_paren_token(const string& tok, const vector<string>& row, int ci) {
    string s = trim(tok);
    if (!s.empty() && s.front() == '(' && (s.size() < 2 || s.back() != ')')) {
        if (ci + 1 < (int)row.size()) {
            string nxt = trim(row[ci+1]);
            if (!nxt.empty() && nxt.front() == ')') return s + ')';
        }
    }
    return tok;
}

static bool row_blocklisted(const vector<string>& rr) {
    string joined;
    for (auto& c : rr) { joined += c; joined.push_back(' '); }
    return std::regex_search(joined, BLOCK_ROW_RE);
}

// ----------------- Extraction from a single table -----------------
static std::pair<optional<double>, double>
extract_from_table(const vector<vector<string>>& rows) {
    if (rows.empty()) return {std::nullopt, -1e18};
    vector<vector<string>> header_rows;
    for (size_t i = 0; i < std::min<size_t>(5, rows.size()); ++i) {
        if (row_is_header_like(rows[i])) header_rows.push_back(rows[i]);
    }
    optional<int> best_col = pick_best_quarter_column(header_rows);

    vector<int> eps_rows;
    for (int i = 0; i < (int)rows.size(); ++i) {
        string joined;
        for (auto& c : rows[i]) { joined += c; joined.push_back(' '); }
        if (std::regex_search(joined, EPS_HEADER_RE)) eps_rows.push_back(i);
    }

    vector<std::pair<double,double>> candidates; // (score, val)

    auto header_text_for_col = [&](int ci) -> string {
        string hdr;
        for (size_t i = 0; i < std::min<size_t>(3, header_rows.size()); ++i) {
            if (ci < (int)header_rows[i].size())
                hdr += to_lower(header_rows[i][ci]) + " ";
        }
        return hdr;
    };

    auto score_band_rows = [&](const vector<vector<string>>& band, bool band_loss) {
        for (const auto& rr : band) {
            if (row_blocklisted(rr)) continue;
            string label;
            for (auto& c : rr) { label += c; label.push_back(' '); }
            string low = to_lower(label);
            bool is_loss = band_loss || std::regex_search(low, LOSS_PHRASE_RE);

            double base = 0.0;
            if (std::regex_search(low, ADJUSTED_RE)) base -= 5.0;
            if (std::regex_search(low, BASIC_RE))    base += 4.0;   // prefer Basic slightly
            if (std::regex_search(low, DILUTED_RE))  base += 1.0;

            bool label_has_basic_or_diluted_or_eps = std::regex_search(low, BASIC_RE) ||
                                                     std::regex_search(low, DILUTED_RE) ||
                                                     std::regex_search(low, EPS_HEADER_RE);
            if (!label_has_basic_or_diluted_or_eps) base -= 0.5;

            for (int ci = 0; ci < (int)rr.size(); ++ci) {
                const string& cell = rr[ci];
                if (cell.find('%') != string::npos) continue;
                std::smatch m;
                if (!std::regex_search(cell, m, NUMBER_RE)) continue;

                if (!best_col && !header_rows.empty()) {
                    string col_hdr = header_text_for_col(ci);
                    if (col_hdr.find("twelve months ended") != string::npos ||
                        col_hdr.find("year ended") != string::npos ||
                        col_hdr.find("12 months") != string::npos) {
                        continue; // skip annual-looking column
                    }
                }
                if (best_col && !(ci == *best_col || ci == *best_col + 1)) continue;

                string tok = maybe_merge_paren_token(m.str(), rr, ci);
                auto valopt = normalize_num_token(tok, is_loss);
                if (!valopt.has_value()) continue;
                double val = *valopt;
                if (std::abs(val) > HARD_ABS_CAP) continue;
                if (std::abs(val) >= 5.0 &&
                    !(std::regex_search(low, BASIC_RE) || std::regex_search(low, DILUTED_RE))) {
                    continue; // guard against unrelated big numbers
                }
                double hscore = header_context_score(ci, header_rows);
                double score = base + hscore + decimal_bonus(tok) + plausibility_penalty(val) + integer_penalty(tok, val);
                candidates.emplace_back(score, val);
            }
        }
    };

    // 1) EPS block row and next few rows
    if (!eps_rows.empty()) {
        for (int idx : eps_rows) {
            int end = std::min<int>((int)rows.size(), idx + 4);
            vector<vector<string>> band(rows.begin()+idx, rows.begin()+end);
            string eps_label_joined;
            for (auto& c : rows[idx]) { eps_label_joined += to_lower(c); eps_label_joined.push_back(' '); }
            bool band_loss = std::regex_search(eps_label_joined, LOSS_PHRASE_RE);
            score_band_rows(band, band_loss);
        }
    }

    // 2) Fallback: rows with Basic/Diluted
    if (candidates.empty()) {
        for (const auto& rr : rows) {
            if (row_blocklisted(rr)) continue;
            string low;
            for (auto& c : rr) { low += to_lower(c); low.push_back(' '); }
            if (std::regex_search(low, BASIC_RE) || std::regex_search(low, DILUTED_RE)) {
                bool is_loss = std::regex_search(low, LOSS_PHRASE_RE);
                double base = 0.0;
                if (std::regex_search(low, ADJUSTED_RE)) base -= 5.0;
                if (std::regex_search(low, BASIC_RE))    base += 2.0;
                if (std::regex_search(low, DILUTED_RE))  base += 3.0;

                for (int ci = 0; ci < (int)rr.size(); ++ci) {
                    const string& cell = rr[ci];
                    if (cell.find('%') != string::npos) continue;
                    std::smatch m;
                    if (!std::regex_search(cell, m, NUMBER_RE)) continue;

                    if (!best_col && !header_rows.empty()) {
                        string col_hdr = header_text_for_col(ci);
                        if (col_hdr.find("twelve months ended") != string::npos ||
                            col_hdr.find("year ended") != string::npos ||
                            col_hdr.find("12 months") != string::npos) {
                            continue;
                        }
                    }
                    if (best_col && !(ci == *best_col || ci == *best_col + 1)) continue;

                    string tok = maybe_merge_paren_token(m.str(), rr, ci);
                    auto valopt = normalize_num_token(tok, is_loss);
                    if (!valopt.has_value()) continue;
                    double val = *valopt;
                    if (std::abs(val) > HARD_ABS_CAP) continue;

                    double hscore = header_context_score(ci, header_rows);
                    double score = base + hscore + decimal_bonus(tok) + plausibility_penalty(val) + integer_penalty(tok, val);
                    candidates.emplace_back(score, val);
                }
            }
        }
    }

    if (!candidates.empty()) {
        auto best = *std::max_element(candidates.begin(), candidates.end(),
                                      [](auto& a, auto& b){ return a.first < b.first; });
        return {best.second, best.first};
    }
    return {std::nullopt, -1e18};
}

// ----------------- Narrative extraction -----------------
static optional<double> extract_from_narrative_strict(const string& text) {
    optional<std::pair<double,double>> best; // (score,val)
    auto it = std::sregex_iterator(text.begin(), text.end(), STRICT_EPS_PHRASE_RE);
    auto end = std::sregex_iterator();
    for (; it != end; ++it) {
        int start = (int)it->position();
        int win_start = it->position() + it->length();
        int win_end = std::min<int>((int)text.size(), win_start + 120);
        string win = text.substr(win_start, win_end - win_start);
        string low = to_lower(win);

        std::smatch mnum;
        if (!std::regex_search(win, mnum, NUMBER_RE)) continue;
        string tok = mnum.str();
        if (tok.find('.') == string::npos) continue;

        bool is_loss = (to_lower(it->str()).find("loss") != string::npos) ||
                       (low.find("loss") != string::npos);

        auto valopt = normalize_num_token(tok, is_loss);
        if (!valopt.has_value()) continue;
        double val = *valopt;
        if (std::abs(val) > 20) continue;

        double base = 0.0;
        if (low.find("dilut") != string::npos) base += 0.75;
        if (low.find("basic") != string::npos) base += 0.25;
        double score = base + 0.5; // mirrors python
        if (!best || score > best->first) best = std::make_pair(score, val);
    }
    if (!best) return std::nullopt;
    return best->second;
}

static optional<double> extract_from_text_eps_token(const string& text) {
    vector<std::pair<double,double>> cands;
    auto it = std::sregex_iterator(text.begin(), text.end(), EPS_TOKEN_RE);
    auto end = std::sregex_iterator();
    for (; it != end; ++it) {
        int s = (int)it->position();
        int e = s + (int)it->length();
        int start = std::max(0, s - 140);
        int stop  = std::min<int>((int)text.size(), e + 160);
        string window = text.substr(start, stop - start);
        string low = to_lower(window);

        double base = 0.0;
        if (std::regex_search(low, std::regex(R"(\b(three\s+months|quarter(?:ly)?|q[1-4])\b)", std::regex::icase))) base += 2.0;
        if (std::regex_search(low, std::regex(R"(\b(twelve\s+months|year(?:\s+ended)?|six\s+months)\b)", std::regex::icase))) base -= 2.0;
        if (std::regex_search(low, ADJUSTED_RE)) base -= 5.0;
        if (std::regex_search(low, BASIC_RE))    base += 0.5;
        if (std::regex_search(low, DILUTED_RE))  base += 0.25;
        bool is_loss = std::regex_search(low, LOSS_PHRASE_RE);

        string after = text.substr(e, std::min<int>(100, (int)text.size() - e));
        std::smatch mnum;
        if (!std::regex_search(after, mnum, NUMBER_RE)) {
            if (!std::regex_search(window, mnum, NUMBER_RE)) continue;
        }
        string tok = mnum.str();
        if (tok.find('.') == string::npos) continue;

        auto valopt = normalize_num_token(tok, is_loss);
        if (!valopt.has_value()) continue;
        double val = *valopt;
        if (std::abs(val) > 20) continue;

        double score = base + decimal_bonus(tok) + plausibility_penalty(val) + integer_penalty(tok, val);
        cands.emplace_back(score, val);
    }
    if (cands.empty()) return std::nullopt;
    auto best = *std::max_element(cands.begin(), cands.end(),
                                  [](auto&a, auto&b){ return a.first < b.first; });
    return best.second;
}

static optional<double> extract_from_text_general(const string& text) {
    vector<std::pair<double,double>> cands;
    auto it = std::sregex_iterator(text.begin(), text.end(), PER_SHARE_RE);
    auto end = std::sregex_iterator();
    for (; it != end; ++it) {
        int s = (int)it->position();
        int e = s + (int)it->length();
        int start = std::max(0, s - 120);
        int stop  = std::min<int>((int)text.size(), e + 160);
        string window = text.substr(start, stop - start);
        string low = to_lower(window);

        string after  = text.substr(e, std::min<int>(120, (int)text.size() - e));
        string before = text.substr(std::max(0, s - 120), std::min(120, s));

        std::smatch mnum;
        if (!std::regex_search(after, mnum, NUMBER_RE)) {
            // last number before
            std::sregex_iterator pit(before.begin(), before.end(), NUMBER_RE), pend;
            if (pit != pend) {
                std::sregex_iterator last = pit;
                for (; pit != pend; ++pit) last = pit;
                mnum = *last;
            } else if (!std::regex_search(window, mnum, NUMBER_RE)) {
                continue;
            }
        }
        string tok = mnum.str();

        string ctx = to_lower(before + " " + after);
        bool is_loss = std::regex_search(ctx, LOSS_PHRASE_RE);
        double base = 0.0;
        if (std::regex_search(ctx, ADJUSTED_RE)) base -= 5.0;
        if (std::regex_search(ctx, BASIC_RE))    base += 2.0;
        if (std::regex_search(ctx, DILUTED_RE))  base += 1.0;

        auto valopt = normalize_num_token(tok, is_loss);
        if (!valopt.has_value()) continue;
        double val = *valopt;
        if (tok.find('.') == string::npos) continue;
        if (std::abs(val) > 20) continue;

        double score = base + decimal_bonus(tok) + plausibility_penalty(val) + integer_penalty(tok, val);
        cands.emplace_back(score, val);
    }
    if (cands.empty()) return std::nullopt;
    auto best = *std::max_element(cands.begin(), cands.end(),
                                  [](auto&a, auto&b){ return a.first < b.first; });
    return best.second;
}

// ----------------- Per-file extraction -----------------
static optional<double> extract_eps_from_html_bytes(const string& html) {
    // Parse with Gumbo
    GumboOutput* out = gumbo_parse_with_options(&kGumboDefaultOptions, html.c_str(), html.size());
    if (!out) return std::nullopt;

    // Collect tables
    vector<Table> tables;
    collect_tables(out->root, tables);

    optional<double> best_val;
    double best_score = -1e18;
    for (auto& t : tables) {
        auto [val, sc] = extract_from_table(t.rows);
        if (val.has_value() && sc > best_score) {
            best_val = val; best_score = sc;
        }
    }
    if (best_val.has_value()) {
        gumbo_destroy_output(&kGumboDefaultOptions, out);
        return best_val;
    }

    // Narrative passes
    string text = extract_visible_text(out);
    gumbo_destroy_output(&kGumboDefaultOptions, out);

    if (auto v = extract_from_narrative_strict(text)) return v;
    if (auto v = extract_from_text_eps_token(text))   return v;
    return extract_from_text_general(text);
}

// ----------------- Main -----------------
int main(int argc, char** argv) {
    if (argc < 3) {
        std::cerr << "Usage: " << argv[0] << " <input_dir> <output_csv>\n";
        return 1;
    }
    auto start = std::chrono::steady_clock::now();

    fs::path in_dir(argv[1]);
    fs::path out_csv(argv[2]);

    try { fs::create_directories(out_csv.parent_path()); } catch (...) {}

    std::ofstream out(out_csv);
    if (!out) { std::cerr << "Failed to open output CSV\n"; return 1; }
    out << "filename,EPS\n";

    // Collect .html / .htm files sorted by name
    vector<fs::path> files;
    for (auto& entry : fs::directory_iterator(in_dir)) {
        if (!entry.is_regular_file()) continue;
        auto ext = to_lower(entry.path().extension().string());
        if (ext == ".html" || ext == ".htm") files.push_back(entry.path());
    }
    std::sort(files.begin(), files.end());

    for (auto& fp : files) {
        try {
            std::ifstream f(fp, std::ios::binary);
            string html((std::istreambuf_iterator<char>(f)), std::istreambuf_iterator<char>());
            // strip HTML comments
            html = std::regex_replace(html, std::regex(R"((?is)<!--.*?-->)", std::regex::icase), " ");
            auto eps = extract_eps_from_html_bytes(html);
            if (eps.has_value()) {
                out << fp.filename().string() << "," << std::fixed << std::setprecision(2) << *eps << "\n";
            } else {
                out << fp.filename().string() << ",\n";
            }
        } catch (...) {
            out << fp.filename().string() << ",\n";
        }
    }

    auto end = std::chrono::steady_clock::now();
    double secs = std::chrono::duration<double>(end - start).count();
    std::cout << "Done in " << secs << " seconds.\n";
    return 0;
}