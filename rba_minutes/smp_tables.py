"""Every table in every Statement on Monetary Policy, as clean data.

Where the tables live:

- 1997 to February 2006: inline in the issue's Bulletin page
- May 2006 to 2023: on the issue's "List of tables" page (tables.html)
- 2024 onwards: inline in the chapter pages

All eras share the same markup: <caption> with the title and a "sub" units
line, <thead> column headers (sometimes several rows with colspans), <tbody>
rows with a <th scope="row"> label, group rows (class "tr-head", or a label
with no values) and indented rows (class "sub"), and a <tfoot> with notes and
sources. Each table is stored once (smp_tables) and cell by cell
(smp_table_cells), keeping the published text alongside the parsed number.

The Financial Stability Review uses the same markup and the same eras, so its
tables go through the same code into fsr_tables and fsr_table_cells (pub="fsr").
"""

import csv
import json
import logging
import re
import sqlite3
import time
from pathlib import Path

from bs4 import BeautifulSoup, Tag

from . import config, db
from .clean import _find_content, normalise_text
from .scrape import make_session
from .smp import RAW_DIR

log = logging.getLogger(__name__)

EXPORT_DIR = config.EXPORT_DIR / "smp_tables"

# --- Downloading the "List of tables" pages ---------------------------------------

def fetch_table_pages(raw_dir: Path = RAW_DIR, refresh: bool = False) -> int:
    """Download each issue's 'List of tables' page (SMP 2006-2023, FSR) and record it in the manifest."""
    session = make_session()
    fetched = 0
    for manifest_path in sorted(raw_dir.glob("*/manifest.json")):
        manifest = json.loads(manifest_path.read_text())
        if manifest["era"] not in ("smp", "fsr") or ("table_pages" in manifest and not refresh):
            continue
        manifest["table_pages"] = []
        if not any(p["file"].endswith("in-brief.html") for p in manifest["pages"]):
            url = manifest["url"] + "tables.html"
            response = session.get(url, timeout=config.REQUEST_TIMEOUT_SECONDS)
            if response.status_code == 200:
                (manifest_path.parent / "tables.html").write_bytes(response.content)
                manifest["table_pages"].append({"url": url, "file": "tables.html"})
                fetched += 1
            elif response.status_code != 404:
                response.raise_for_status()
            time.sleep(config.REQUEST_DELAY_SECONDS)
        manifest_path.write_text(json.dumps(manifest, indent=2))
    return fetched


# --- Cell values ---------------------------------------------------------------------

FRACTIONS = {"¼": 0.25, "½": 0.5, "¾": 0.75, "⅓": 1 / 3, "⅔": 2 / 3, "⅛": 0.125, "⅜": 0.375,
             "⅝": 0.625, "⅞": 0.875}
# A leading en dash is a minus sign in some tables ('–0.2'); on its own it means nil.
_NUM = r"[−+–-]?(?:\d[\d,]*(?:\.\d+)?[¼½¾⅓⅔⅛⅜⅝⅞]?|[¼½¾⅓⅔⅛⅜⅝⅞])"
UNIT_SUFFIX = re.compile(r"\s*(?:bps?|basis points|ppts?|pp|percentage points)$", re.IGNORECASE)
UP, DOWN = "↑▲", "↓▼"
NUMBER = re.compile(rf"^\(?({_NUM})\)?$")
RANGE = re.compile(rf"^({_NUM})\s*(?:–|—|−|to)\s*({_NUM})$")
# '2¾ (3¼)' = current forecast (previous forecast); '1.4 (Sep)' = value (period).
WITH_NOTE = re.compile(rf"^({_NUM})\s*\([^()]*\)$")
MISSING = {"", "–", "—", "-", "na", "n.a.", "n/a", "(n/a)", "(na)", "..", "...", "…", "nil", "*", "nc", "n.c."}
FOOTNOTE_MARK = re.compile(r"\((?:[a-z]{1,2}|\d{1,2})\)|\*+$")


def to_number(text: str) -> float | None:
    """'2¾' -> 2.75, '−1,504' -> -1504.0; None if the text is not a single number."""
    text = text.replace(" ", "").replace("−-", "−").lstrip("~")
    match = NUMBER.match(text)
    if not match:
        return None
    token = match.group(1).replace(",", "").replace("−", "-").replace("–", "-").lstrip("+")
    negative = token.startswith("-")
    token = token.lstrip("-")
    fraction = FRACTIONS.get(token[-1:], 0.0) if token else 0.0
    if fraction:
        token = token[:-1]
    try:
        value = (float(token) if token else 0.0) + fraction
    except ValueError:
        return None
    return -value if negative else value


def parse_direction(text: str) -> int | None:
    """+1 / -1 for cells carrying an up or down arrow, 0 for 'No change', else None."""
    if any(a in text for a in UP):
        return 1
    if any(a in text for a in DOWN):
        return -1
    if text.strip().lower() in ("no change", "unchanged"):
        return 0
    return None


def parse_value(text: str) -> tuple[float | None, float | None, float | None]:
    """(value, low, high) from a cell's text. A range gives low/high and its midpoint.

    Arrows give the sign of the number next to them ('↓ 250' -> -250); a unit
    such as 'bps' is dropped ('↑ 50 bps' -> 50).
    """
    direction = parse_direction(text)
    cleaned = text.strip()
    for arrow in UP + DOWN:
        cleaned = cleaned.replace(arrow, "")
    cleaned = UNIT_SUFFIX.sub("", cleaned.strip().rstrip("%").strip())
    if cleaned.lower() in MISSING:
        return None, None, None
    if direction == -1:
        value = to_number(cleaned)
        if value is not None:
            return -abs(value), None, None
    value = to_number(cleaned)
    if value is not None:
        return value, None, None
    match = RANGE.match(cleaned)
    if match:
        low, high = to_number(match.group(1)), to_number(match.group(2))
        is_years = all(v is not None and v == int(v) and 1900 <= v <= 2100 for v in (low, high))
        if low is not None and high is not None and low <= high and not is_years:
            return (low + high) / 2, low, high
    match = WITH_NOTE.match(cleaned)
    if match:
        return to_number(match.group(1)), None, None
    return None, None, None


BLOCK_TAGS = ("p", "div", "li")
TRAILING_MARKS = re.compile(r"(?:\s*(?:\((?:[a-z]{1,2}|\d{1,2})\)|\*+))+$")
PERIOD = re.compile(
    r"^(?:(?:jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*\.?\s*(?:qtr\s*)?)?"
    r"(?:\d{4}(?:/\d{2,4})?|\d{2}(?:/\d{2})?)$|^(?:year|years|quarter|month)", re.IGNORECASE)
DATE_TEXT = re.compile(r"^(?:jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*\.?\s*\d{2,4}$", re.IGNORECASE)
TOTAL_LABEL = re.compile(r"^(?:total|world|all\b)|\btotal$", re.IGNORECASE)
INDENT = re.compile(r"^[–—−-]\s*")


def _text_of(el: Tag) -> str:
    """Text of an element without spaces injected at inline tags ('¥6', not '¥ 6')."""
    for br in el.find_all("br"):
        previous = br.previous_sibling
        joins_word = isinstance(previous, str) and previous.rstrip().endswith("-")
        br.replace_with("" if joins_word else " ")
    for block in el.find_all(BLOCK_TAGS):
        block.append(" ")
    text = normalise_text(el.get_text(""))
    # a word broken at a line wrap ('Weighted-\n average'), but not 'fixed- and variable-rate'
    return re.sub(r"(\w)- (?!(?:and|or|to)\b)(\w)", lambda m: m.group(1) + "-" + m.group(2), text) if "- " in text else text


def _cell_text(cell: Tag) -> tuple[str, str]:
    """(text without footnote markers, comma-separated footnote markers)."""
    marks = []
    for sup in cell.find_all("sup"):
        marks.append(normalise_text(sup.get_text(" ")))
        sup.decompose()
    text = _text_of(cell)
    if marks and re.search(r"\(\s*\)$", text):   # 'Corporations(<sup>a</sup>)': the brackets sit outside the sup
        marks = [f"({m})" if re.fullmatch(r"[a-z]{1,2}|\d{1,2}", m) else m for m in marks]
    trailing = TRAILING_MARKS.search(text)
    if trailing and trailing.start() > 0 and not re.fullmatch(r"\s*\(\d{1,2}\)", trailing.group(0)):
        marks += re.findall(r"\([a-z]{1,2}\)|\*+", trailing.group(0))   # plain-text marks: 'Unemployment rate(b)'
        text = text[:trailing.start()].rstrip()
    text = text.rstrip(",").rstrip()                                       # 'ABS(a),(b)' leaves 'ABS,'
    text = re.sub(r"\(\s*\)$", "", text).rstrip()
    return text, ",".join(m for m in marks if m)


# --- Table structure -------------------------------------------------------------------

def _grid(rows: list[Tag]) -> list[list[dict]]:
    """Expand rowspan/colspan into a rectangular grid of cell dicts."""
    grid: list[list[dict | None]] = []
    for r, row in enumerate(rows):
        while len(grid) <= r:
            grid.append([])
        c = 0
        for cell in row.find_all(["th", "td"], recursive=False):
            while c < len(grid[r]) and grid[r][c] is not None:
                c += 1
            text, marks = _cell_text(cell)
            classes = cell.get("class", [])
            info = {"text": text, "marks": marks, "is_th": cell.name == "th", "classes": classes,
                    "row_classes": row.get("class", []), "strong": cell.find(["strong", "b"]) is not None,
                    "colspan": int(cell.get("colspan", 1) or 1)}
            rowspan = int(cell.get("rowspan", 1) or 1)
            colspan = info["colspan"]
            for dr in range(rowspan):
                while len(grid) <= r + dr:
                    grid.append([])
                target = grid[r + dr]
                while len(target) < c + colspan:
                    target.append(None)
                for dc in range(colspan):
                    target[c + dc] = dict(info, spanned=(dr, dc) != (0, 0), row_spanned=dr > 0, col_spanned=dc > 0)
            c += colspan
    width = max((len(r) for r in grid), default=0)
    empty = {"text": "", "marks": "", "is_th": False, "classes": [], "row_classes": [], "strong": False,
             "colspan": 1, "spanned": False, "row_spanned": False, "col_spanned": False, "filler": True}
    return [[cell or dict(empty) for cell in row] + [dict(empty) for _ in range(width - len(row))] for row in grid]


def _caption(table: Tag) -> tuple[str, str, str, str]:
    """(table number, title, subtitle/units, caption footnote marks)."""
    caption = table.find("caption")
    marks = ""
    subtitle = ""
    if caption is None:
        holder = table.find_parent("div", class_="box-table")
        heading = holder.find(["h3", "h4", "p"]) if holder else None
        text = _text_of(heading) if heading else ""
    else:
        sub = caption.find(class_=["sub", "sup"])  # 'sup' is a typo for 'sub' in some issues
        sub_marks = ""
        if sub is not None:
            subtitle, sub_marks = _cell_text(sub)
            sub.decompose()
        text, marks = _cell_text(caption)
        marks = ",".join(m for m in (marks, sub_marks) if m)
    text = re.sub(r"^(Table\s+[A-Z])l\b", r"\g<1>1", text)   # 'Table Bl' (a typo for B1)
    match = (re.match(r"^Table\s+([A-Z]\.\d+|[A-Z]?\d+(?:\.\d+)?[A-Za-z]?|[A-Z]\d*)\s*[:.–-]?\s*(.*)$", text)
             or re.match(r"^([A-Z]?\d+(?:\.\d+)?)\s*:\s*(.*)$", text))      # "2.4: Foreign Exchange Reserves"
    number, title = (match.group(1), match.group(2)) if match else ("", re.sub(r"^Table\s*:\s*", "", text))
    return number, title.strip(), subtitle.strip(), marks


def _split_note(text: str, notes: list, sources: list) -> None:
    if re.match(r"^sources?\s*:", text, re.IGNORECASE):
        sources.append(re.sub(r"^sources?\s*:\s*", "", text, flags=re.IGNORECASE))
    elif text:
        notes.append(text)


def _footer(table: Tag) -> tuple[list, list]:
    """(notes, sources) from the table footer, which may also sit just after the table."""
    notes: list[str] = []
    sources: list[str] = []
    places = list(table.find_all("tfoot"))
    holder = table.find_parent("div", class_="box-table")
    if holder is not None:
        places += [el for el in holder.find_all(class_=re.compile("note|source")) if el.find_parent("table") is None]
    seen = set()
    for place in places:
        source_els = place.find_all(class_="source")
        for el in source_els:
            text = _text_of(el)
            if text and text not in seen:
                seen.add(text)
                sources.append(re.sub(r"^sources?\s*:\s*", "", text, flags=re.IGNORECASE))
            el.decompose()
        for el in place.find_all(["p", "li"]):
            text = _text_of(el)
            if text and text not in seen:
                seen.add(text)
                _split_note(text, notes, sources)
            el.decompose()
        rest = _text_of(place)   # notes written as bare text in the footer cell
        if rest and rest not in seen:
            seen.add(rest)
            _split_note(rest, notes, sources)
    return notes, sources


def _is_number_like(text: str) -> bool:
    return parse_value(text)[0] is not None or parse_direction(text) is not None


def _level(cell: dict) -> int:
    for cls in cell["classes"]:
        match = re.fullmatch(r"sub(\d?)", cls)
        if match:
            return int(match.group(1) or 1)
    return 1 if INDENT.match(cell["text"]) else 0


def parse_table(table: Tag) -> dict:
    """Turn one <table> into {caption fields, notes, sources, cells: [...]}."""
    html = str(table)
    number, title, subtitle, title_marks = _caption(table)
    notes, sources = _footer(table)
    for tfoot in table.find_all("tfoot"):
        tfoot.decompose()

    head_rows = [tr for thead in table.find_all("thead") for tr in thead.find_all("tr")]
    body_rows = [tr for tbody in table.find_all("tbody") for tr in tbody.find_all("tr", recursive=False)]
    if not body_rows:
        body_rows = [tr for tr in table.find_all("tr") if tr not in head_rows]
    if not head_rows:
        # No <thead>: leading rows made of several header cells are the column headers.
        while body_rows:
            cells = body_rows[0].find_all(["th", "td"], recursive=False)
            filled = [c for c in cells if c.get_text(strip=True)]
            stub_then_header = len(filled) == 1 and len(cells) > 1 and not cells[0].get_text(strip=True)
            if (len(filled) >= 2 or stub_then_header) and all(c.name == "th" for c in filled):
                head_rows.append(body_rows.pop(0))
            else:
                break

    head = _grid(head_rows)
    body = _grid(body_rows)
    width = max([len(r) for r in head + body] or [0])
    # A header row that leaves out the empty stub cell above the row labels is one cell
    # short: it belongs under the value columns, so it is shifted right (2007-08 Table 5).
    for r, row in enumerate(head):
        short = width - sum(1 for cell in row if not cell.get("filler"))
        if short > 0 and r > 0 and row and row[0]["text"] and not head[r - 1][0]["text"] \
                and all(cell.get("filler") for cell in row[len(row) - short:]) and all(c.get("filler") for c in row[width:]):
            head[r] = [dict(row[-1], text="", marks="", spanned=False) for _ in range(short)] + row[:len(row) - short]
    head = [r + [dict(r[-1] if r else {}, text="", spanned=False)] * (width - len(r)) for r in head]
    body = [r + [dict(r[-1], text="", spanned=False)] * (width - len(r)) for r in body]

    # Column header levels and their footnote marks.
    col_parts: list[list[str]] = [[] for _ in range(width)]
    col_marks: list[list[str]] = [[] for _ in range(width)]
    col_own = [False] * width  # a header cell starts in this column (not just spans into it)
    for row in head:
        for c in range(width):
            cell = row[c]
            if cell.get("text") and not cell.get("col_spanned"):
                col_own[c] = True
            if cell.get("text") and cell["text"] not in col_parts[c]:
                col_parts[c].append(cell["text"])
            if cell.get("marks") and not cell.get("row_spanned") and cell["marks"] not in col_marks[c]:
                col_marks[c].append(cell["marks"])

    # Body rows that are really notes ("Source: RBA" or "(a) ..." spanning the table).
    kept = []
    for row in body:
        texts = {c["text"] for c in row if c["text"]}
        spans = row[0]["colspan"] >= max(2, width - 1) if row else False
        if spans and len(texts) == 1 and re.match(r"^(sources?\s*:|\([a-z]\)|notes?\s*:|\*)", next(iter(texts)), re.I):
            _split_note(next(iter(texts)), notes, sources)
            continue
        if not texts:
            continue  # spacer row
        kept.append(row)
    body = kept

    # The label column: leading <th scope="row"> cells (usually one).
    label_cols = 1 if body and (body[0][0]["is_th"] or not col_parts[0]) else 0

    def is_heading(row) -> bool:
        label = row[0]["text"] if label_cols else ""
        values = row[label_cols:]
        has_values = any(v["text"] and not (v["spanned"] and v["text"] == label) for v in values)
        return bool(label) and not has_values

    def header_like(row) -> bool:
        """A row inside the body that is really column headers or a panel name."""
        if label_cols and row[0]["text"]:
            return False
        filled = [v for v in row[label_cols:] if v["text"] and not v["spanned"]]
        if not filled:
            return False
        if all(v["is_th"] for v in filled):
            return True
        return len(filled) == 1 and filled[0]["colspan"] >= max(2, width - label_cols - 1) or \
            len(filled) >= 2 and all(PERIOD.match(v["text"]) for v in filled)

    # Columns that never hold a value (spacer columns, or columns created by a
    # colspan wider than the table) are dropped unless they carry a real header.
    data_rows = [r for r in body if not is_heading(r) and not header_like(r)]
    keep = list(range(width))
    for c in range(label_cols, width):
        if all(not r[c]["text"] or (r[c]["spanned"] and c > 0 and r[c]["text"] == r[c - 1]["text"]) for r in data_rows):
            label = " | ".join(col_parts[c])
            neighbours = {" | ".join(col_parts[c - 1]) if c > 0 else None, " | ".join(col_parts[c + 1]) if c + 1 < width else None}
            if not label or label in neighbours or not col_own[c]:
                keep.remove(c)  # (a header spanning into an empty column, e.g. colspan=7 over 6 columns)

    # Extra leading label columns: text-only columns with no header (e.g. 'US' / '– Dow Jones').
    while label_cols < len(keep) - 1:
        c = keep[label_cols]
        texts = [r[c]["text"] for r in data_rows if r[c]["text"]]
        if texts and not col_parts[c] and not any(_is_number_like(t) for t in texts):
            label_cols += 1
        else:
            break
    label_idx, value_idx = keep[:label_cols], keep[label_cols:]

    # A header shared by every column ('Year-ended') is a panel name, like the
    # 'Year-average' heading row further down the body.
    def labels_for(parts_by_col):
        return {c: " | ".join(parts_by_col[c]) for c in range(width)}
    common = None
    value_parts = [col_parts[c] for c in value_idx if col_parts[c]]
    if len(value_parts) >= 2 and all(len(p) >= 2 for p in value_parts) and len({p[0] for p in value_parts}) == 1:
        common = value_parts[0][0]
        col_parts = [p[1:] if p and p[0] == common and len(p) >= 2 else p for p in col_parts]
    labels = labels_for(col_parts)

    # Side-by-side panels: a text column after numeric ones starts a new panel with its own labels.
    panels, current = [], {"label": label_idx[-1] if label_idx else None, "cols": [], "name": ""}
    for c in value_idx:
        texts = [r[c]["text"] for r in data_rows if r[c]["text"] and r[c]["text"].lower() not in MISSING]
        repeats_label_header = bool(label_idx) and bool(col_parts[c]) and col_parts[c] == col_parts[label_idx[-1]]
        row_headers = bool(texts) and all(r[c]["is_th"] for r in data_rows if r[c]["text"])  # <th> mid-row: labels
        # A text column is a second panel's labels only if it looks like one (the label column's
        # header repeated, <th> cells, or no header); otherwise it is a column of text values
        # ('3 years 7 months', or '*' marking which factor moved the exchange rate).
        # A header spanning this column and the next ('Assets' over 'Credit' and '33.6') also marks
        # a panel's label column, unless the column holds dates ('(24 Mar)' beside '−7').
        datelike = sum(bool(DATE_TEXT.match(t) or PERIOD.match(t) or t.startswith("(")) for t in texts) >= len(texts) / 2
        position = value_idx.index(c)
        spans_next = bool(col_parts[c]) and position + 1 < len(value_idx) and col_parts[value_idx[position + 1]] == col_parts[c] \
            and (position == 0 or col_parts[value_idx[position - 1]] != col_parts[c])
        # ...or the columns after it repeat the first panel's headers ('Exporters | Per cent | Importers | Per cent').
        repeats_structure = position + 1 < len(value_idx) and bool(col_parts[value_idx[0]]) \
            and col_parts[value_idx[position + 1]] == col_parts[value_idx[0]] and col_parts[c] != col_parts[value_idx[0]]
        textual = bool(texts) and not any(_is_number_like(t) for t in texts) and (
            repeats_label_header or row_headers or ((not col_parts[c] or spans_next or repeats_structure) and not datelike))
        if textual and current["cols"] and any(
                any(_is_number_like(r[x]["text"]) for r in data_rows) for x in current["cols"]):
            panels.append(current)
            current = {"label": c, "cols": [], "name": " | ".join(col_parts[c])}
        else:
            current["cols"].append(c)
    panels.append(current)
    multi_panel = len(panels) > 1
    if multi_panel:
        # Panel label columns hold row labels, not values; a panel name that only repeats
        # its columns' header adds nothing.
        value_idx = [c for c in value_idx if c not in {p["label"] for p in panels[1:]}]
        for p in panels[1:]:
            if all(labels[c] == p["name"] for c in p["cols"]):
                p["name"] = ""
            p["group"] = ""
        left_cols = set(panels[0]["cols"])

        def is_heading(row) -> bool:  # headings of the left panel; the other panels have their own
            label = row[0]["text"] if label_cols else ""
            return bool(label) and not any(row[c]["text"] for c in left_cols)

    # Indent levels: a 'sub' class or a leading dash. A dashed 'sub' row under a
    # dash-only row ('– Housing' then '– Owner-occupier') is one level deeper.
    def raw_level(row):
        cell = row[label_idx[-1]] if label_idx else None
        if cell is None:
            return 0, False
        cls = next((int(m.group(1) or 1) for m in (re.fullmatch(r"sub(\d?)", x) for x in cell["classes"]) if m), 0)
        return cls, bool(INDENT.match(cell["text"]))
    levels = []
    for i, row in enumerate(body):
        cls, dash = raw_level(row)
        level = cls or int(dash)
        if cls and dash and i > 0 and raw_level(body[i - 1]) == (0, True):
            level = cls + 1
        elif cls and dash and i > 0 and raw_level(body[i - 1]) == (cls, dash):
            level = levels[i - 1]  # its sibling's level
        levels.append(level)

    def span_label(row, c) -> str:
        """Column label for a value cell spanning several columns: the header levels they share."""
        span = [x for x in value_idx if c <= x < c + row[c]["colspan"]]
        if len(span) < 2:
            return labels.get(c, "")
        shared = []
        for parts in zip(*(col_parts[x] for x in span)):
            if len(set(parts)) != 1:
                break
            shared.append(parts[0])
        return " | ".join(shared) or labels.get(c, "")

    def lower_start(text) -> bool:
        text = INDENT.sub("", text)
        return bool(text) and text[0].isalpha() and text[0].islower() and not re.match(r"of which|o/w", text)

    def is_section_heading(row) -> bool:
        return is_heading(row) and ("tr-head" in row[0]["classes"] or "tr-head" in row[0]["row_classes"])
    later_section = [any(is_section_heading(r) for r in body[i + 1:]) for i in range(len(body))]

    def is_subtotal(i) -> bool:
        """Whether a bold row's first value is the sum of the rows since the last heading (Coal: 229 = 87+76+43+23)."""
        cols = [c for c in value_idx if parse_value(body[i][c]["text"])[0] is not None]
        if not cols:
            return False
        c, parts = cols[0], []
        for j in range(i - 1, -1, -1):
            if is_heading(body[j]) or header_like(body[j]):
                break
            if levels[j] == levels[i]:
                parts.append(parse_value(body[j][c]["text"])[0])
        if len(parts) < 2 or any(v is None for v in parts):
            return False
        total = parse_value(body[i][c]["text"])[0]
        return abs(sum(parts) - total) <= 0.02 * abs(total) + 0.05 * len(parts)

    # Group headings form a stack. Consecutive headings nest. A heading whose
    # first data row is indented covers only the indented rows below it;
    # otherwise it lasts until the next heading of the same or higher rank
    # (rank 0 = 'tr-head' headings, 1 = bold ones, 2 = plain ones).
    def rank(row) -> int:
        cell = row[0]
        if "tr-head" in cell["classes"] or "tr-head" in cell["row_classes"]:
            return 0
        return 1 if cell["strong"] or "bold" in cell["row_classes"] else 2

    def first_data_after(i):
        """The next row's index if it is a data row; None if a heading comes first (then the heading is a section)."""
        nxt = body[i + 1] if i + 1 < len(body) else None
        return i + 1 if nxt is not None and not is_heading(nxt) and not header_like(nxt) else None

    header_labels = [labels.get(c, "") for c in value_idx]  # 'labels' can change at in-body header rows
    cells, stack, row_idx = [], ([{"label": common, "rank": -1, "scope": "section", "marks": "", "level": -1}] if common else []), 0
    previous_heading, previous_label, carried, prefix = False, "", "", ""
    previous_total = False
    last_row_cells: list[dict] = []
    for i, row in enumerate(body):
        if header_like(row):
            filled = [v for v in row[label_cols:] if v["text"] and not v["spanned"]]
            if len(filled) == 1 and filled[0]["colspan"] >= max(2, width - label_cols - 1):
                stack = [{"label": filled[0]["text"], "rank": -1, "scope": "section", "marks": filled[0]["marks"], "level": -1}]
                labels = labels_for(col_parts)  # a new panel starts with the table's own column headers
            else:
                new_parts = [list(p) for p in col_parts]
                for c in value_idx:
                    if row[c]["text"]:
                        new_parts[c] = [row[c]["text"]]
                labels = labels_for(new_parts)
            previous_heading = True
            continue
        label_parts = [row[c]["text"] for c in label_idx if row[c]["text"]]
        if len(label_idx) > 1:
            # A label written once for a block of rows ('Secured by residential property' over
            # 'range' and 'average') is carried down to the rows below it.
            if row[label_idx[0]]["text"]:
                carried = row[label_idx[0]]["text"]
            elif carried and label_parts:
                label_parts = [carried] + label_parts
        left_heading = is_heading(row)
        label, group, level, is_total, group_marks, row_marks = "", "", 0, False, [], []
        if left_heading and not multi_panel and label_parts and lower_start(label_parts[-1]) \
                and last_row_cells and not label_parts[-1].endswith(":"):
            # The end of the previous row's label, wrapped onto a row of its own
            # ('Real GDP adjusted for' / 'changes in terms of trade').
            for cell in last_row_cells:
                cell["row_label"] = f"{cell['row_label']} {INDENT.sub('', label_parts[-1])}"
            previous_label = last_row_cells[0]["row_label"]
            continue
        nxt = first_data_after(i) if left_heading else None
        if left_heading and nxt is not None and label_parts and not label_parts[-1].endswith(":") \
                and lower_start(body[nxt][label_idx[-1]]["text"]) and levels[nxt] == levels[i]:
            # A label wrapped onto the next row ('GDP adjusted for changes' / 'in the terms of trade').
            prefix = INDENT.sub("", label_parts[-1])
            continue
        if left_heading:
            carried = ""
            heading = INDENT.sub("", label_parts[-1]) if label_parts else ""
            rk = rank(row)
            scope = "children" if nxt is not None and label_cols and levels[nxt] > levels[i] else "section"
            entry = {"label": heading, "rank": rk, "scope": scope, "marks": row[label_idx[-1]]["marks"], "level": levels[i],
                     "child_level": levels[nxt] if nxt is not None else None, "bold_members": False}
            if not previous_heading:
                # A heading indented like the rows of the heading above, with rows at its own
                # indent, is that heading's sibling ('Prime full-doc' / 'Prime low-doc').
                while stack and stack[-1]["scope"] == "children" and (
                        stack[-1]["level"] >= entry["level"] or
                        (entry["child_level"] == entry["level"] == stack[-1]["child_level"])):
                    stack.pop()
                while stack and stack[-1]["rank"] >= entry["rank"] and stack[-1]["level"] >= entry["level"]:
                    stack.pop()
            stack.append(entry)
        else:
            label_cell = row[label_idx[-1]] if label_idx else None
            level = levels[i]
            closed_children = False
            raw_total = bool(TOTAL_LABEL.search(" ".join(INDENT.sub("", t) for t in label_parts)))
            if not previous_heading:
                while stack and stack[-1]["scope"] == "children" and level <= stack[-1]["level"]:
                    if stack[-1]["rank"] == 0 and (raw_total or later_section[i]):
                        break  # a 'tr-head' section's own total, or an unindented row before the next section
                    stack.pop()
                    closed_children = True
            label = " – ".join(INDENT.sub("", t) for t in label_parts)
            if prefix:
                label, prefix = f"{prefix} {label}", ""
            row_values = [row[c]["text"] for c in panels[0]["cols"] if row[c]["text"]]
            if not label and label_idx and row_values:
                bracketed = all(t.startswith("(") and t.endswith(")") for t in row_values)
                if previous_heading and stack:
                    label = stack[-1]["label"]   # the only row under its heading ('Personal loans')
                elif previous_label and bracketed:
                    label = f"{previous_label} (bracketed)"  # per cent of GDP, targets, etc. under their row
                elif is_subtotal(i):
                    label = "Total"              # an unlabelled sum of the rows above (−9.6 + 2.0 = −7.6)
                elif previous_label:
                    label = previous_label       # a row continuing the one above (a second programme under 'RBA')
            if label_cell is not None and label_cell["strong"] and level == 0 and not TOTAL_LABEL.search(label) \
                    and not is_subtotal(i):
                if previous_heading and stack:
                    stack[-1]["bold_members"] = True   # the section's own rows are bold ('Domestic', 'Imported')
                else:
                    # A bold category row stands outside the groups above it.
                    stack = [e for e in stack if e["rank"] < 0 or e.get("bold_members")]
            if label.lower() == "(previous)" and previous_label:
                label = f"{previous_label} (previous)"
            is_total = bool(TOTAL_LABEL.search(label))
            if is_total and previous_total and not closed_children and len(stack) > (1 if common else 0) \
                    and stack[-1]["rank"] > 0:
                stack.pop()  # a second 'Total' in a row closes the inner group (e.g. onshore + offshore)
            group = " > ".join(e["label"] for e in stack if e["label"])
            group_marks = [e["marks"] for e in stack if e["marks"]]
            row_marks = [row[c]["marks"] for c in label_idx if row[c]["marks"]]
        emitted = False
        pending_sign = None
        first_cell = len(cells)
        for panel_no, panel in enumerate(panels):
            if panel_no == 0:
                if left_heading:
                    continue
                p_label, p_group, p_level, p_total, p_marks = label, group, level, is_total, group_marks + row_marks
            else:
                # A side-by-side panel keeps its own groups: a bold label with no values in the
                # panel starts one, a bold label with values stands outside it.
                lab = row[panel["label"]] if panel["label"] is not None else None
                p_label = INDENT.sub("", lab["text"]) if lab else ""
                has_values = any(row[c]["text"] for c in panel["cols"])
                if lab and p_label and lab["strong"]:
                    panel["group"] = "" if has_values else p_label
                    if not has_values:
                        continue
                sections = [e for e in stack if e["rank"] < 0]
                p_group = " > ".join([e["label"] for e in sections if e["label"]] + [x for x in (panel["name"], panel["group"]) if x])
                p_level = (next((int(m.group(1) or 1) for m in (re.fullmatch(r"sub(\d?)", x) for x in lab["classes"]) if m), 0)
                           or int(bool(INDENT.match(lab["text"])))) if lab else 0
                p_total = bool(TOTAL_LABEL.search(p_label))
                p_marks = [e["marks"] for e in sections if e["marks"]] + ([lab["marks"]] if lab and lab["marks"] else [])
            for c in panel["cols"]:
                cell = row[c]
                if cell["spanned"] and c > 0 and cell["text"] == row[c - 1]["text"] and cell["text"]:
                    continue
                if not cell["text"] and not p_label:
                    continue  # this panel has no entry on this row
                value, low, high = parse_value(cell["text"])
                direction = parse_direction(cell["text"])
                if pending_sign is not None and value is not None and direction is None:
                    value, direction = pending_sign * abs(value), pending_sign   # '↓' then '25 bps' in the next cell
                pending_sign = direction if direction in (1, -1) and value is None else None
                cells.append({
                    "row_idx": row_idx, "col_idx": value_idx.index(c),
                    "row_label": p_label,
                    "row_group": p_group,
                    "row_level": p_level,
                    "col_label": span_label(row, c) if cell["colspan"] > 1 else labels.get(c, ""),
                    "value_text": cell["text"], "value_num": value, "value_low": low, "value_high": high,
                    "value_direction": direction,
                    "footnotes": ",".join(m for m in p_marks + col_marks[c] + [cell["marks"]] if m),
                    "is_total": int(p_total),
                })
                emitted = True
        last_row_cells = cells[first_cell:] if not left_heading else []
        if left_heading:
            previous_heading, previous_total = True, False
            row_idx += emitted
            continue
        previous_heading, previous_total = False, is_total
        if label and not label.endswith("(previous)"):
            previous_label = label
        row_idx += 1

    return {
        "table_number": number, "title": title, "subtitle": subtitle, "title_footnotes": title_marks,
        "notes": " ".join(notes), "sources": "; ".join(sources), "html": html,
        "n_rows": row_idx, "n_cols": len(value_idx),
        "column_labels": header_labels, "cells": cells,
    }


def title_key(title: str) -> str:
    """A grouping key so the same table can be followed across issues."""
    return re.sub(r"[^a-z0-9]+", " ", re.sub(r"\(.*?\)", "", title.lower())).strip()


# --- Extraction over the downloaded issues ----------------------------------------------

def issue_tables(folder: Path) -> list[dict]:
    manifest = json.loads((folder / "manifest.json").read_text())
    tables, seen = [], set()
    for page in manifest["pages"] + manifest.get("table_pages", []):
        soup = BeautifulSoup((folder / page["file"]).read_bytes(), "lxml")
        content = _find_content(soup)
        heading = content.find("h1")
        page_title = normalise_text(heading.get_text(" ")) if heading else ""
        for table in content.find_all("table"):
            if table.find_parent("table") is not None:
                continue
            parsed = parse_table(table)
            if not parsed["cells"]:
                continue
            key = (parsed["table_number"], parsed["title"], len(parsed["cells"]))
            if key in seen:
                continue
            seen.add(key)
            tables.append({**parsed, "issue_date": manifest["issue_date"], "source_url": page["url"],
                           "page_title": page_title})
    return tables


SCHEMA = """
CREATE TABLE IF NOT EXISTS {pub}_tables (
    table_id        INTEGER PRIMARY KEY,
    issue_date      TEXT NOT NULL,          -- YYYY-MM of the issue
    position        INTEGER NOT NULL,       -- order within the issue
    table_number    TEXT,                   -- as printed, e.g. '14', '3.1', 'A1'
    title           TEXT NOT NULL,
    title_key       TEXT NOT NULL,          -- lower-case title without footnote marks, to follow a table over time
    subtitle        TEXT,                   -- usually the units
    title_footnotes TEXT,                   -- footnote marks on the caption
    notes           TEXT,
    sources         TEXT,
    n_rows          INTEGER NOT NULL,
    n_cols          INTEGER NOT NULL,
    column_labels   TEXT NOT NULL,          -- JSON array
    page_title      TEXT,
    source_url      TEXT NOT NULL,
    html            TEXT NOT NULL,          -- the published <table>, for checking
    UNIQUE (issue_date, position)
);
CREATE INDEX IF NOT EXISTS {pub}_tables_title_key ON {pub}_tables (title_key);

CREATE TABLE IF NOT EXISTS {pub}_table_cells (
    table_id        INTEGER NOT NULL REFERENCES {pub}_tables(table_id) ON DELETE CASCADE,
    row_idx         INTEGER NOT NULL,
    col_idx         INTEGER NOT NULL,
    row_label       TEXT,
    row_group       TEXT,                   -- e.g. 'Activity' in the detailed forecast table
    row_level       INTEGER NOT NULL,       -- indent depth: 0, 1 ('–' or class sub), 2 (sub2), 3 (sub3)
    col_label       TEXT,                   -- header levels joined with ' | '
    value_text      TEXT NOT NULL,          -- exactly as published (footnote marks removed)
    value_num       REAL,                   -- parsed number; midpoint for ranges
    value_low       REAL,                   -- for ranges such as '2½–3'
    value_high      REAL,
    value_direction INTEGER,                -- +1 / -1 for up / down arrows, 0 for 'No change'
    footnotes       TEXT,                   -- footnote marks that apply: group heading, row label, column header, cell
    is_total        INTEGER NOT NULL,
    PRIMARY KEY (table_id, row_idx, col_idx)
);

CREATE VIEW IF NOT EXISTS v_{pub}_table_cells AS
SELECT t.issue_date, t.table_number, t.title, t.subtitle, c.row_group, c.row_label, c.col_label,
       c.value_text, c.value_num, c.value_low, c.value_high, c.value_direction, c.footnotes,
       t.table_id, c.row_idx, c.col_idx
FROM {pub}_table_cells c JOIN {pub}_tables t USING (table_id)
ORDER BY t.issue_date, t.position, c.row_idx, c.col_idx;
"""


def build(conn: sqlite3.Connection, raw_dir: Path = RAW_DIR, pub: str = "smp") -> tuple[int, int]:
    """Re-extract every table from the downloaded issues. Returns (tables, cells)."""
    conn.executescript(f"DROP VIEW IF EXISTS v_{pub}_table_cells; DROP TABLE IF EXISTS {pub}_table_cells; "
                       f"DROP TABLE IF EXISTS {pub}_tables;")
    conn.executescript(SCHEMA.replace("{pub}", pub))
    n_tables = n_cells = 0
    for folder in sorted(p for p in raw_dir.iterdir() if (p / "manifest.json").exists()):
        for position, t in enumerate(issue_tables(folder)):
            table_id = conn.execute(
                f"INSERT INTO {pub}_tables (issue_date, position, table_number, title, title_key, subtitle, title_footnotes, "
                "notes, sources, n_rows, n_cols, column_labels, page_title, source_url, html) "
                "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING table_id",
                (t["issue_date"], position, t["table_number"], t["title"], title_key(t["title"]), t["subtitle"],
                 t["title_footnotes"],
                 t["notes"], t["sources"], t["n_rows"], t["n_cols"], json.dumps(t["column_labels"], ensure_ascii=False),
                 t["page_title"], t["source_url"], t["html"]),
            ).fetchone()[0]
            conn.executemany(
                f"INSERT INTO {pub}_table_cells (table_id, row_idx, col_idx, row_label, row_group, row_level, col_label, "
                "value_text, value_num, value_low, value_high, value_direction, footnotes, is_total) "
                "VALUES (:table_id, :row_idx, :col_idx, :row_label, :row_group, :row_level, :col_label, "
                ":value_text, :value_num, :value_low, :value_high, :value_direction, :footnotes, :is_total)",
                [{**c, "table_id": table_id} for c in t["cells"]],
            )
            n_tables += 1
            n_cells += len(t["cells"])
    conn.commit()
    return n_tables, n_cells


def export(conn: sqlite3.Connection, out_dir: Path = EXPORT_DIR, pub: str = "smp") -> int:
    """Write every cell to one long CSV, and each table to its own wide CSV."""
    out_dir.mkdir(parents=True, exist_ok=True)
    cursor = conn.execute(f"SELECT * FROM v_{pub}_table_cells")
    with open(out_dir / "all_cells.csv", "w", newline="", encoding="utf-8") as handle:
        writer = csv.writer(handle)
        writer.writerow([c[0] for c in cursor.description])
        writer.writerows(cursor)
    tables = conn.execute(f"SELECT table_id, issue_date, position, table_number, title, column_labels FROM {pub}_tables")
    count = 0
    for table_id, issue_date, position, number, title, column_labels in tables.fetchall():
        labels = json.loads(column_labels)
        rows: dict[int, dict] = {}
        for r, c, group, label, text in conn.execute(
            f"SELECT row_idx, col_idx, row_group, row_label, value_text FROM {pub}_table_cells "
            "WHERE table_id = ? ORDER BY row_idx, col_idx", (table_id,)
        ):
            row = rows.setdefault(r, {"group": group, "label": label, "values": {}})
            row["values"][c] = text
        slug = re.sub(r"[^a-z0-9]+", "-", f"{position:02d} table {number} {title}".lower()).strip("-")[:80]
        folder = out_dir / issue_date
        folder.mkdir(exist_ok=True)
        with open(folder / f"{slug}.csv", "w", newline="", encoding="utf-8") as handle:
            writer = csv.writer(handle)
            writer.writerow(["row_group", "row_label"] + [labels[i] if i < len(labels) else f"col{i}"
                                                       for i in range(len(labels))])
            for r in sorted(rows):
                writer.writerow([rows[r]["group"], rows[r]["label"]] +
                                [rows[r]["values"].get(i, "") for i in range(len(labels))])
        count += 1
    return count


def fetch_graph_data(raw_dir: Path = RAW_DIR, out_dir: Path = config.DATA_DIR / "smp_graph_data", pub: str = "smp") -> list[Path]:
    """Download the 'underlying data for selected graphs' workbooks (published from 2024).

    Saved as source files only: each sheet places many graphs side by side, so
    they are not parsed into tables here.
    """
    session = make_session()
    out_dir.mkdir(parents=True, exist_ok=True)
    saved = []
    for manifest_path in sorted(raw_dir.glob("*/manifest.json")):
        manifest = json.loads(manifest_path.read_text())
        if manifest["era"] != pub or manifest["issue_date"] < "2024":
            continue
        target = out_dir / f"{pub}-graph-data-{manifest['issue_date']}.xlsx"
        if target.exists():
            saved.append(target)
            continue
        response = session.get(manifest["url"] + f"xlsx/{pub}-graph-data.xlsx", timeout=config.REQUEST_TIMEOUT_SECONDS)
        if response.status_code == 200 and response.content[:2] == b"PK":
            target.write_bytes(response.content)
            saved.append(target)
        time.sleep(config.REQUEST_DELAY_SECONDS)
    return saved


def cross_check_forecast_archive(conn: sqlite3.Connection) -> dict:
    """Compare the RBA's forecast archive workbook with the forecast tables extracted from the HTML.

    The archive (November 2018 onwards, loaded by `forecasts-load`) is an
    independent publication of the same numbers, so every archive value
    should appear in the matching issue's forecast table. Returns counts and
    any mismatches.
    """
    def row_key(label: str) -> str:
        label = label.lower().replace("’", "'").replace("savings", "saving")
        return " ".join(sorted(re.findall(r"[a-z0-9$]+", label)))

    def period_key(label: str) -> str | None:
        match = re.search(r"(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*[\s-]*(\d{4})", label.lower())
        return f"{match.group(1)}-{match.group(2)}" if match else None

    if not conn.execute("SELECT name FROM sqlite_master WHERE name = 'smp_forecast_archive'").fetchone():
        return {"skipped": "run forecasts-load first"}
    cells: dict[tuple, list] = {}
    issues = set()
    for issue_date, row_label, col_label, value in conn.execute(
        "SELECT t.issue_date, c.row_label, c.col_label, c.value_num FROM smp_table_cells c "
        "JOIN smp_tables t USING (table_id) WHERE t.title LIKE '%Forecast Table%'"
    ):
        issues.add(issue_date)
        period = period_key(col_label)
        if period:
            cells.setdefault((issue_date, row_key(row_label), period), []).append(value)
    result = {"matched": 0, "different": [], "not_found": []}
    for smp_date, variable, target_label, value in conn.execute(
        "SELECT smp_date, variable, target_label, value FROM smp_forecast_archive"
    ):
        found = cells.get((smp_date, row_key(variable), period_key(target_label)))
        if not found:
            result["not_found"].append((smp_date, variable, target_label, value))
        elif any(v is not None and abs(v - value) < 1e-9 for v in found):
            result["matched"] += 1
        else:
            result["different"].append((smp_date, variable, target_label, value, found))
    return result
