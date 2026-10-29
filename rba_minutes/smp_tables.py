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
    """Download tables.html for each 2006-2023 issue and record it in the manifest."""
    session = make_session()
    fetched = 0
    for manifest_path in sorted(raw_dir.glob("*/manifest.json")):
        manifest = json.loads(manifest_path.read_text())
        if manifest["era"] != "smp" or ("table_pages" in manifest and not refresh):
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
_NUM = r"[−-]?(?:\d[\d,]*(?:\.\d+)?[¼½¾⅓⅔⅛⅜⅝⅞]?|[¼½¾⅓⅔⅛⅜⅝⅞])"
NUMBER = re.compile(rf"^\(?({_NUM})\)?$")
RANGE = re.compile(rf"^({_NUM})\s*(?:–|—|to)\s*({_NUM})$")
MISSING = {"", "–", "—", "-", "na", "n.a.", "n/a", "..", "...", "…", "nil", "*", "nc", "n.c."}
FOOTNOTE_MARK = re.compile(r"\((?:[a-z]{1,2}|\d{1,2})\)|\*+$")


def to_number(text: str) -> float | None:
    """'2¾' -> 2.75, '−1,504' -> -1504.0; None if the text is not a single number."""
    text = text.replace(" ", "")
    match = NUMBER.match(text)
    if not match:
        return None
    token = match.group(1).replace(",", "").replace("−", "-")
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


def parse_value(text: str) -> tuple[float | None, float | None, float | None]:
    """(value, low, high) from a cell's text. A range gives low/high and its midpoint."""
    cleaned = text.strip().rstrip("%").strip()
    if cleaned.lower() in MISSING:
        return None, None, None
    value = to_number(cleaned)
    if value is not None:
        return value, None, None
    match = RANGE.match(cleaned)
    if match:
        low, high = to_number(match.group(1)), to_number(match.group(2))
        if low is not None and high is not None:
            return (low + high) / 2, low, high
    return None, None, None


def _cell_text(cell: Tag) -> tuple[str, str]:
    """(text without footnote markers, comma-separated footnote markers)."""
    marks = []
    for sup in cell.find_all("sup"):
        marks.append(normalise_text(sup.get_text(" ")))
        sup.decompose()
    for br in cell.find_all("br"):
        br.replace_with(" ")
    text = normalise_text(cell.get_text(" "))
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
            info = {"text": text, "marks": marks, "is_th": cell.name == "th",
                    "classes": cell.get("class", []), "row_classes": row.get("class", [])}
            rowspan = int(cell.get("rowspan", 1) or 1)
            colspan = int(cell.get("colspan", 1) or 1)
            for dr in range(rowspan):
                while len(grid) <= r + dr:
                    grid.append([])
                target = grid[r + dr]
                while len(target) < c + colspan:
                    target.append(None)
                for dc in range(colspan):
                    target[c + dc] = dict(info, spanned=(dr, dc) != (0, 0))
            c += colspan
    width = max((len(r) for r in grid), default=0)
    empty = {"text": "", "marks": "", "is_th": False, "classes": [], "row_classes": [], "spanned": False}
    return [[cell or dict(empty) for cell in row] + [dict(empty)] * (width - len(row)) for row in grid]


def _caption(table: Tag) -> tuple[str, str, str, str]:
    """(table number, title, subtitle/units, caption footnote marks)."""
    caption = table.find("caption")
    if caption is None:
        holder = table.find_parent("div", class_="box-table")
        heading = holder.find(["h3", "h4", "p"]) if holder else None
        text = normalise_text(heading.get_text(" ")) if heading else ""
        subtitle = ""
    else:
        sub = caption.find(class_="sub")
        subtitle = ""
        if sub is not None:
            subtitle, _ = _cell_text(sub)
            sub.decompose()
        text, _ = _cell_text(caption)
    match = re.match(r"^Table\s+([A-Z]?\d+(?:\.\d+)?[A-Za-z]?|[A-Z]\d*)\s*[:.–-]?\s*(.*)$", text)
    number, title = (match.group(1), match.group(2)) if match else ("", text)
    return number, title.strip(), subtitle.strip(), ""


def _footer(table: Tag) -> tuple[str, str]:
    """(notes, sources) from the table footer, which may also sit just after the table."""
    notes, sources = [], []
    places = [table.find("tfoot")]
    holder = table.find_parent("div", class_="box-table")
    if holder is not None:
        places += holder.find_all(class_=re.compile("note|source"), recursive=True)
    seen = set()
    for place in places:
        if place is None:
            continue
        for p in place.find_all(["p", "li"]) or [place]:
            text = normalise_text(p.get_text(" "))
            if not text or text in seen:
                continue
            seen.add(text)
            if re.match(r"^sources?\s*:", text, re.IGNORECASE) or "source" in (p.get("class") or []):
                sources.append(re.sub(r"^sources?\s*:\s*", "", text, flags=re.IGNORECASE))
            else:
                notes.append(text)
    return " ".join(notes), "; ".join(sources)


def parse_table(table: Tag) -> dict:
    """Turn one <table> into {caption fields, notes, sources, cells: [...]}."""
    html = str(table)
    number, title, subtitle, _ = _caption(table)
    notes, sources = _footer(table)
    for tfoot in table.find_all("tfoot"):
        tfoot.decompose()

    head_rows = [tr for thead in table.find_all("thead") for tr in thead.find_all("tr")]
    body_rows = [tr for tbody in table.find_all("tbody") for tr in tbody.find_all("tr", recursive=False)]
    if not body_rows:
        body_rows = [tr for tr in table.find_all("tr") if tr not in head_rows]
    if not head_rows and body_rows and all(c.name == "th" for c in body_rows[0].find_all(["th", "td"])):
        head_rows, body_rows = body_rows[:1], body_rows[1:]

    head = _grid(head_rows)
    body = _grid(body_rows)
    width = max([len(r) for r in head + body] or [0])

    # Column labels: join the distinct header texts stacked above each column.
    col_labels = []
    for c in range(width):
        parts = []
        for row in head:
            text = row[c]["text"] if c < len(row) else ""
            if text and text not in parts:
                parts.append(text)
        col_labels.append(" | ".join(parts))

    # The label column is the leading run of <th scope="row"> cells (usually one).
    label_cols = 1 if body and body[0] and (body[0][0]["is_th"] or not col_labels[:1] or not col_labels[0]) else 0

    # Groups nest: a "tr-head" row (e.g. "Activity") starts a top-level group;
    # a label with no values (e.g. "Less revisions since Budget") starts a
    # sub-group that covers the indented rows beneath it.
    cells, top_group, sub_group, row_idx = [], "", "", 0
    for row in body:
        label = row[0]["text"] if label_cols else ""
        label_marks = row[0]["marks"] if label_cols else ""
        values = row[label_cols:]
        row_classes = row[0]["row_classes"] if row else []
        is_sub = "sub" in (row[0]["classes"] if row else [])
        all_empty = all(not v["text"] for v in values)
        spans_all = values and all(v["spanned"] for v in values) and label
        if label and ("tr-head" in row_classes or spans_all):
            top_group, sub_group = label, ""
            continue
        if label and all_empty:
            sub_group = label
            continue
        if sub_group and not is_sub:
            sub_group = ""
        group = " > ".join(g for g in (top_group, sub_group) if g)
        is_total = "bold" in row_classes or label.lower().startswith(("total", "world"))
        for c, cell in enumerate(values, start=label_cols):
            if cell["spanned"] and cell["text"] == (row[c - 1]["text"] if c else None):
                continue
            value, low, high = parse_value(cell["text"])
            cells.append({
                "row_idx": row_idx, "col_idx": c - label_cols,
                "row_label": label, "row_group": group, "row_level": 1 if is_sub else 0,
                "col_label": col_labels[c] if c < len(col_labels) else "",
                "value_text": cell["text"], "value_num": value, "value_low": low, "value_high": high,
                "footnotes": ",".join(m for m in (label_marks, cell["marks"]) if m),
                "is_total": int(is_total),
            })
        row_idx += 1

    return {
        "table_number": number, "title": title, "subtitle": subtitle,
        "notes": notes, "sources": sources, "html": html,
        "n_rows": row_idx, "n_cols": max(0, width - label_cols),
        "column_labels": col_labels[label_cols:], "cells": cells,
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
CREATE TABLE IF NOT EXISTS smp_tables (
    table_id        INTEGER PRIMARY KEY,
    issue_date      TEXT NOT NULL,          -- YYYY-MM of the SMP issue
    position        INTEGER NOT NULL,       -- order within the issue
    table_number    TEXT,                   -- as printed, e.g. '14', '3.1', 'A1'
    title           TEXT NOT NULL,
    title_key       TEXT NOT NULL,          -- lower-case title without footnote marks, to follow a table over time
    subtitle        TEXT,                   -- usually the units
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
CREATE INDEX IF NOT EXISTS smp_tables_title_key ON smp_tables (title_key);

CREATE TABLE IF NOT EXISTS smp_table_cells (
    table_id        INTEGER NOT NULL REFERENCES smp_tables(table_id) ON DELETE CASCADE,
    row_idx         INTEGER NOT NULL,
    col_idx         INTEGER NOT NULL,
    row_label       TEXT,
    row_group       TEXT,                   -- e.g. 'Activity' in the detailed forecast table
    row_level       INTEGER NOT NULL,       -- 1 for indented sub-rows
    col_label       TEXT,                   -- header levels joined with ' | '
    value_text      TEXT NOT NULL,          -- exactly as published (footnote marks removed)
    value_num       REAL,                   -- parsed number; midpoint for ranges
    value_low       REAL,                   -- for ranges such as '2½–3'
    value_high      REAL,
    footnotes       TEXT,                   -- footnote marks on the row label or cell, e.g. '(a)'
    is_total        INTEGER NOT NULL,
    PRIMARY KEY (table_id, row_idx, col_idx)
);

CREATE VIEW IF NOT EXISTS v_smp_table_cells AS
SELECT t.issue_date, t.table_number, t.title, t.subtitle, c.row_group, c.row_label, c.col_label,
       c.value_text, c.value_num, c.value_low, c.value_high, c.footnotes, t.table_id, c.row_idx, c.col_idx
FROM smp_table_cells c JOIN smp_tables t USING (table_id)
ORDER BY t.issue_date, t.position, c.row_idx, c.col_idx;
"""


def build(conn: sqlite3.Connection, raw_dir: Path = RAW_DIR) -> tuple[int, int]:
    """Re-extract every table from the downloaded issues. Returns (tables, cells)."""
    conn.executescript(SCHEMA)
    conn.execute("DELETE FROM smp_table_cells")
    conn.execute("DELETE FROM smp_tables")
    n_tables = n_cells = 0
    for folder in sorted(p for p in raw_dir.iterdir() if (p / "manifest.json").exists()):
        for position, t in enumerate(issue_tables(folder)):
            table_id = conn.execute(
                "INSERT INTO smp_tables (issue_date, position, table_number, title, title_key, subtitle, notes, "
                "sources, n_rows, n_cols, column_labels, page_title, source_url, html) "
                "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING table_id",
                (t["issue_date"], position, t["table_number"], t["title"], title_key(t["title"]), t["subtitle"],
                 t["notes"], t["sources"], t["n_rows"], t["n_cols"], json.dumps(t["column_labels"], ensure_ascii=False),
                 t["page_title"], t["source_url"], t["html"]),
            ).fetchone()[0]
            conn.executemany(
                "INSERT INTO smp_table_cells (table_id, row_idx, col_idx, row_label, row_group, row_level, col_label, "
                "value_text, value_num, value_low, value_high, footnotes, is_total) "
                "VALUES (:table_id, :row_idx, :col_idx, :row_label, :row_group, :row_level, :col_label, "
                ":value_text, :value_num, :value_low, :value_high, :footnotes, :is_total)",
                [{**c, "table_id": table_id} for c in t["cells"]],
            )
            n_tables += 1
            n_cells += len(t["cells"])
    conn.commit()
    return n_tables, n_cells


def export(conn: sqlite3.Connection, out_dir: Path = EXPORT_DIR) -> int:
    """Write every cell to one long CSV, and each table to its own wide CSV."""
    out_dir.mkdir(parents=True, exist_ok=True)
    cursor = conn.execute("SELECT * FROM v_smp_table_cells")
    with open(out_dir / "all_cells.csv", "w", newline="", encoding="utf-8") as handle:
        writer = csv.writer(handle)
        writer.writerow([c[0] for c in cursor.description])
        writer.writerows(cursor)
    tables = conn.execute("SELECT table_id, issue_date, position, table_number, title, column_labels FROM smp_tables")
    count = 0
    for table_id, issue_date, position, number, title, column_labels in tables.fetchall():
        labels = json.loads(column_labels)
        rows: dict[int, dict] = {}
        for r, c, group, label, text in conn.execute(
            "SELECT row_idx, col_idx, row_group, row_label, value_text FROM smp_table_cells "
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
