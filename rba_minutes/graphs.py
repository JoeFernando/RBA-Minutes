"""Graphs in RBA publications (SMP and Financial Stability Review): metadata, images and text.

How graphs are published:

- Bulletin era (to 2005): <img src="images/graph-....gif" alt="Graph 1: Title"> inline in the page
- 2006-2023: chapters link to one page per graph (graphs/graph-1.1.html) holding the image,
  whose alt text is "Graph 1.1: Title"
- 2024 onwards: inline <figure> with "Graph 2.1" in the caption, an SVG image whose
  text elements carry the title, axis labels, notes and sources, and a long alt-text
  description of the chart

Images are saved under data/<publication>_graphs/<issue>/ and described in a
<publication>_graphs table. SVG text is extracted so it can be searched.
"""

import datetime as dt
import html as html_lib
import json
import logging
import re
import sqlite3
import time
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from urllib.parse import urljoin, urlparse

import numpy as np
from bs4 import BeautifulSoup

from . import config, db
from .clean import _find_content, normalise_text
from .scrape import make_session

log = logging.getLogger(__name__)

GRAPH_LABEL = re.compile(
    r"^(Box\s+\w+\s+)?(?:Graph|Figure|Chart)\s*([A-Z]\.\d+|[A-Z]?\d+(?:\.\d+)*[a-z]?|[A-Z]\d*)\s*[:.–-]?\s*(.*)$", re.S)


# Alt text that describes a chart rather than naming it ('A two-panel line graph showing ...').
DESCRIPTION = re.compile(r"^(an?|the|this|two|three|four|five|six|single|multi)\b.*\b(graph|chart|panels?|plot)\b", re.I)


def _split_label(text: str) -> tuple[str, str]:
    """('2.1', 'Title') from 'Graph 2.1: Title'; ('Box 1 Graph 2', 'Title') from a 1997-2005 box graph."""
    text = normalise_text(text)
    match = GRAPH_LABEL.match(text)
    if not match:
        return "", text
    box = f"{match.group(1).strip()} Graph " if match.group(1) else ""
    return box + match.group(2), match.group(3).strip().lstrip(":").strip()


def _is_graph_img(img) -> bool:
    name = Path(urlparse(img.get("src") or "").path).name.lower()
    return name.startswith("graph") and name.endswith((".gif", ".png", ".svg", ".jpg"))


def find_graphs(page_html: bytes, page_url: str) -> list[dict]:
    """Graph references on one downloaded page, in reading order."""
    soup = BeautifulSoup(page_html, "lxml")
    content = _find_content(soup)
    found, seen = [], set()
    for el in content.find_all(["img", "a"]):
        if el.name == "img" and _is_graph_img(el) and not el.find_parent("a", href=re.compile(r"graphs/graph")):
            url = urljoin(page_url, el["src"].split("?")[0])
            if url in seen:
                continue
            seen.add(url)
            figure = el.find_parent("figure")
            caption = figure.find("figcaption") if figure else None
            caption_text = normalise_text(caption.get_text(" ")) if caption else ""
            alt = normalise_text(html_lib.unescape(el.get("alt") or ""))  # some alts are double-escaped
            alt_number, alt_title = _split_label(alt)
            if figure is not None:
                # 2023 onwards: the number (and sometimes the title) in the caption; the alt text is a
                # description of the chart ('Graph 2.1: A two-panel line graph...'); otherwise the
                # title is read from the SVG later.
                number, title = _split_label(caption_text)
                number = number or alt_number
                description = alt_title if alt_number else alt
                # 2018-2022 put the title itself in the alt text ('China - Interest Rates')
                if not title and description and len(description) <= 120 and not DESCRIPTION.match(description):
                    title, description = description, ""
            elif alt_number:  # alt reads "Graph 1: Title"
                number, title, description = alt_number, alt_title, ""
            else:
                number, title = _split_label(caption_text)
                description = alt
            # The section it sits in: its box (1997-2005 boxes sit inside chapters), else the heading above it.
            box = el.find_parent("div", class_="box-info")
            heading = box.find(["h2", "h3"]) if box is not None else el.find_previous("h2")
            found.append({"number": number, "title": title, "description": description,
                          "image_url": url, "graph_page": None,
                          "section": normalise_text(heading.get_text(" ")) if heading is not None else ""})
        elif el.name == "a" and re.search(r"/graphs/graph-[\w.-]+\.html$", urljoin(page_url, el.get("href", ""))):
            url = urljoin(page_url, el["href"]).split("#")[0]
            if url in seen:
                continue
            seen.add(url)
            number, _ = _split_label(el.get_text(" "))
            found.append({"number": number, "title": "", "description": "", "image_url": None, "graph_page": url})
    return found


def _unsquash(text: str) -> str:
    """The RBA's SVGs drop spaces between words ('BrentOilPrice'); restore them at case changes."""
    return re.sub(r"(?<=[a-z0-9%)])(?=[A-Z])", " ", text)


def _glyph_runs(soup) -> list[tuple]:
    """(text element, [(string, x positions)], font size, bold) for each <text> in an SVG.

    The RBA's SVGs position every glyph (x="141.5 161.7 ...") and leave out the spaces, so a
    space shows only as a wider gap between two glyphs.
    """
    runs = []
    for t in soup.find_all("text"):
        try:
            size = float(re.sub(r"[^\d.]", "", t.get("font-size", "0")) or 0)
        except ValueError:
            size = 0.0
        bold = t.get("font-weight") == "bold"
        parts = []
        for span in t.find_all("tspan") or [t]:
            text = span.get_text("")
            xs = [float(v) for v in re.findall(r"-?\d+(?:\.\d+)?", span.get("x", ""))]
            parts.append((text, xs if len(xs) == len(text) else []))
        runs.append((t, parts, size, bold))
    return runs


def glyph_widths(svg_files) -> dict:
    """Each character's advance width (in ems, by weight) when no space follows it, learned from the SVGs.

    The 10th percentile of the observed advances: a glyph followed by a space advances further.
    """
    observed: dict[tuple, list] = {}
    for path in svg_files:
        try:
            soup = BeautifulSoup(Path(path).read_bytes(), "xml")
        except Exception:  # noqa: BLE001
            continue
        for _, parts, size, bold in _glyph_runs(soup):
            for text, xs in parts:
                if size <= 0 or len(xs) < 2:
                    continue
                for i in range(len(xs) - 1):
                    advance = (xs[i + 1] - xs[i]) / size
                    if 0 < advance < 2:
                        observed.setdefault((text[i], bold), []).append(advance)
    return {k: float(np.percentile(v, 10)) for k, v in observed.items() if len(v) >= 5}


SPACE_EM = 0.278  # the width of a space in Arial, in ems


def _respace(text: str, xs: list, size: float, bold: bool, widths: dict) -> str:
    """Put back the spaces that the glyph positions show."""
    if not widths or len(xs) != len(text) or size <= 0:
        return text
    out = []
    for i, ch in enumerate(text):
        out.append(ch)
        if i + 1 < len(text) and ch != " " and text[i + 1] != " ":
            width = widths.get((ch, bold)) or widths.get((ch, not bold))
            advance = (xs[i + 1] - xs[i]) / size
            if advance < 0 or (width is not None and advance - width > SPACE_EM * 0.55):  # (or a jump back: axis labels)
                out.append(" ")
    return "".join(out)


def _svg_text(svg: bytes, widths: dict | None = None) -> tuple[str, str]:
    """(title, all text) from an SVG graph. The title is the largest (bold) text element.

    With glyph widths (see glyph_widths) the missing spaces are rebuilt from the glyph positions;
    without, words are split at case changes only.
    """
    try:
        soup = BeautifulSoup(svg, "xml")
    except Exception:  # noqa: BLE001 - a malformed SVG should not stop the run
        return "", ""
    texts = []
    for t, parts, size, bold in _glyph_runs(soup):
        if widths:
            value = normalise_text(" ".join(_respace(text, xs, size, bold, widths) for text, xs in parts))
        else:
            value = _unsquash(normalise_text(t.get_text(" ")))
        texts.append((size + (1 if bold else 0), value))
    named = [t for t in texts if re.search(r"[A-Za-z]{2}", t[1])]  # not a stray glyph such as an apostrophe
    title = max(named, default=(0, ""))[1]
    return title, " | ".join(v for _, v in texts if v)


SCHEMA = """
CREATE TABLE IF NOT EXISTS {pub}_graphs (
    graph_id        INTEGER PRIMARY KEY,
    issue_date      TEXT NOT NULL,
    chapter_title   TEXT,
    position        INTEGER NOT NULL,       -- order within the issue
    graph_number    TEXT,                   -- as printed, e.g. '82', '2.1', 'A5'
    title           TEXT,
    description     TEXT,                   -- long alt-text description (2024 onwards)
    svg_text        TEXT,                   -- every text element in an SVG graph: title, axes, notes, sources
    image_url       TEXT,
    image_file      TEXT,                   -- path relative to the project root
    image_bytes     INTEGER,
    graph_page      TEXT,                   -- the graph's own page (2006-2023)
    UNIQUE (issue_date, position)
);
"""


def collect(raw_dir: Path, pub: str, image_dir: Path, workers: int = 3, delay: float = 1.0) -> list[dict]:
    """Find every graph in the downloaded issues, fetch graph pages and images. Returns the records.

    Results are cached per issue in <raw issue folder>/graphs.json, so re-runs only fetch what is missing.
    """
    session = make_session()

    def fetch(url: str) -> bytes | None:
        for attempt in range(3):
            try:
                response = session.get(url, timeout=config.REQUEST_TIMEOUT_SECONDS)
                if response.status_code == 404:
                    return None
                response.raise_for_status()
                time.sleep(delay)
                return response.content
            except Exception as e:  # noqa: BLE001 - retried, then recorded as missing
                log.warning("Retry %d for %s: %s", attempt + 1, url, e)
                time.sleep(5 * (attempt + 1))
        return None

    def process_issue(folder: Path) -> list[dict]:
        cache = folder / "graphs.json"
        if cache.exists():
            return json.loads(cache.read_text())
        manifest = json.loads((folder / "manifest.json").read_text())
        records = []
        for page in manifest["pages"]:
            html = (folder / page["file"]).read_bytes()
            heading = _find_content(BeautifulSoup(html, "lxml")).find("h1")
            chapter = normalise_text(heading.get_text(" ")) if heading else ""
            for graph in find_graphs(html, page["url"]):
                section = graph.pop("section", "")
                graph["chapter_title"] = section if manifest["era"] == "bulletin" and section else re.sub(r"^.*?(Statement on Monetary Policy|Financial Stability Review)\s*[–-]\s*\w+\s+\d{4}\s*", "", chapter)
                if graph["graph_page"]:
                    page_html = fetch(graph["graph_page"])
                    if page_html:
                        inner = find_graphs(page_html, graph["graph_page"])
                        image = next((g for g in inner if g["image_url"]), None)
                        if image:
                            image.pop("section", None)
                        if image:
                            graph["image_url"] = image["image_url"]
                            number, title = image["number"], image["title"]
                            graph["number"] = graph["number"] or number
                            graph["title"] = title
                            graph["description"] = image["description"]
                records.append(graph)
        out_dir = image_dir / manifest["issue_date"]
        out_dir.mkdir(parents=True, exist_ok=True)
        for graph in records:
            graph["image_file"] = graph["image_bytes"] = graph["svg_text"] = None
            if not graph["image_url"]:
                continue
            name = Path(urlparse(graph["image_url"]).path).name
            target = out_dir / name
            data = target.read_bytes() if target.exists() else fetch(graph["image_url"])
            if data:
                target.write_bytes(data)
                graph["image_file"] = (str(target.relative_to(config.PROJECT_ROOT))
                                       if target.is_relative_to(config.PROJECT_ROOT) else str(target))
                graph["image_bytes"] = len(data)
                if name.lower().endswith(".svg"):
                    svg_title, graph["svg_text"] = _svg_text(data)
                    graph["title"] = graph["title"] or svg_title
        cache.write_text(json.dumps(records, indent=1, ensure_ascii=False))
        log.info("%s: %d graphs", manifest["issue_date"], len(records))
        return records

    folders = sorted(p for p in raw_dir.iterdir() if (p / "manifest.json").exists())
    with ThreadPoolExecutor(max_workers=workers) as pool:
        results = list(pool.map(process_issue, folders))
    return [dict(r, issue_date=json.loads((f / "manifest.json").read_text())["issue_date"])
            for f, recs in zip(folders, results) for r in recs]


def _poor_title(title: str) -> bool:
    """No title, a title read from SVG text with its spaces lost ('CPIItems Rising Morethan2.5'), or alt
    text that runs on into the chart description."""
    return (not re.search(r"[A-Za-z]{2}", title) or len(title) > 150
            or bool(re.search(r"[a-z]{2}[A-Z][a-z]|[a-z]\d|\d[a-z]{2}", title)))


def fill_missing_titles(raw_dir: Path, delay: float = 1.0) -> int:
    """Titles from each issue's 'List of graphs' page, for graph pages that give only 'Graph 2'.

    Some years (e.g. SMP 2007, FSR September 2005) put the title only in the image; some SVG-only
    titles lose their spaces. Titles that are missing or look damaged are replaced.
    Updates the cached graphs.json files; returns the number of titles filled.
    """
    session = make_session()
    filled = 0
    for cache in sorted(raw_dir.glob("*/graphs.json")):
        records = json.loads(cache.read_text())
        missing = [r for r in records if r.get("graph_page") and _poor_title(r.get("title") or "")]
        if not missing:
            continue
        manifest = json.loads((cache.parent / "manifest.json").read_text())
        response = session.get(manifest["url"] + "graphs.html", timeout=config.REQUEST_TIMEOUT_SECONDS)
        time.sleep(delay)
        if response.status_code != 200:
            continue
        content = _find_content(BeautifulSoup(response.content, "lxml"))
        titles = {}
        for anchor in content.find_all("a", href=re.compile(r"graphs/graph-[\w.-]+\.html")):
            holder = anchor.find_parent(["li", "p", "td", "tr"]) or anchor.parent
            text = normalise_text(holder.get_text(" ")).replace(normalise_text(anchor.get_text(" ")), "", 1)
            titles[urljoin(manifest["url"] + "graphs.html", anchor["href"]).split("#")[0]] = text.strip(" :–-")
        for r in missing:
            if titles.get(r["graph_page"]) and titles[r["graph_page"]] != r.get("title"):
                r["title"] = titles[r["graph_page"]]
                filled += 1
        cache.write_text(json.dumps(records, indent=1, ensure_ascii=False))
    return filled


def respace_svg_text(raw_dirs: list[Path]) -> int:
    """Rebuild the spaces in SVG titles and text in the cached graphs.json files. Returns titles changed."""
    caches = [c for d in raw_dirs for c in sorted(d.glob("*/graphs.json"))]
    svgs = [config.PROJECT_ROOT / r["image_file"] for c in caches for r in json.loads(c.read_text())
            if (r.get("image_file") or "").endswith(".svg")]
    widths = glyph_widths(p for p in svgs if p.exists())
    changed = 0
    for cache in caches:
        records = json.loads(cache.read_text())
        for r in records:
            path = config.PROJECT_ROOT / (r.get("image_file") or "")
            if not path.name.endswith(".svg") or not path.exists():
                continue
            data = path.read_bytes()
            old_title, _ = _svg_text(data)
            new_title, r["svg_text"] = _svg_text(data, widths)
            if (r.get("title") in ("", None, old_title) or _poor_title(r.get("title") or "")) \
                    and new_title and new_title != r.get("title"):
                r["title"] = new_title  # the title came from the SVG: use the respaced one
                changed += 1
        cache.write_text(json.dumps(records, indent=1, ensure_ascii=False))
    return changed


def store(conn: sqlite3.Connection, records: list[dict], pub: str) -> int:
    conn.executescript(f"DROP TABLE IF EXISTS {pub}_graphs;" + SCHEMA.format(pub=pub))
    positions: dict[str, int] = {}
    for r in records:
        position = positions.get(r["issue_date"], 0)
        positions[r["issue_date"]] = position + 1
        conn.execute(
            f"INSERT INTO {pub}_graphs (issue_date, chapter_title, position, graph_number, title, description, "
            "svg_text, image_url, image_file, image_bytes, graph_page) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
            (r["issue_date"], r.get("chapter_title"), position, r["number"], r["title"], r["description"],
             r.get("svg_text"), r["image_url"], r.get("image_file"), r.get("image_bytes"), r["graph_page"]),
        )
    conn.commit()
    return len(records)


# --- Underlying data for graphs (published as workbooks from 2024) ---------------------

DATA_SCHEMA = """
CREATE TABLE IF NOT EXISTS {pub}_graph_series (
    series_id       INTEGER PRIMARY KEY,
    issue_date      TEXT NOT NULL,
    sheet           TEXT NOT NULL,          -- usually a chapter or box
    graph_number    TEXT,
    graph_title     TEXT,
    source          TEXT,
    panel           TEXT,
    series          TEXT NOT NULL,          -- the column header
    x_label         TEXT                    -- the header of the x column ('Date', 'Date (quarterly)', 'Country')
);
CREATE TABLE IF NOT EXISTS {pub}_graph_values (
    series_id       INTEGER NOT NULL REFERENCES {pub}_graph_series(series_id),
    x               TEXT,                   -- ISO date or category
    value           REAL NOT NULL
);
CREATE VIEW IF NOT EXISTS v_{pub}_graph_data AS
SELECT s.issue_date, s.sheet, s.graph_number, s.graph_title, s.source, s.panel, s.series, s.x_label, v.x, v.value
FROM {pub}_graph_values v JOIN {pub}_graph_series s USING (series_id);
"""

_ROW_NAMES = {"publication", "title", "graph number", "graph title", "source", "sources", "panel", "data"}


def _cell(value) -> str:
    if value is None:
        return ""
    if isinstance(value, (dt.datetime, dt.date)):
        return value.strftime("%Y-%m-%d")
    if isinstance(value, float) and value.is_integer():
        return str(int(value))
    return normalise_text(str(value))


def parse_graph_data(path: Path, issue_date: str) -> tuple[list[dict], list[dict]]:
    """(values, restricted graphs) from one graph-data workbook.

    Each sheet has label rows (Graph number, Graph title, Source, Panel, Data)
    over blocks of columns, one block per graph: a 'Graph number' cell starts
    a block. In a block, an x column (dates or categories) is followed by the
    series plotted against it; a block can hold several x columns (panels or
    different frequencies). Graphs whose data cannot be published say 'Data
    restricted'.
    """
    import openpyxl

    workbook = openpyxl.load_workbook(path, read_only=True, data_only=True)
    values, restricted = [], []
    for sheet in workbook.worksheets:
        rows = [list(r) for r in sheet.iter_rows(values_only=True)]
        labels = {}
        for i, row in enumerate(rows[:20]):
            name = _cell(row[0] if row else "").lower()
            if name in _ROW_NAMES and name not in labels:
                labels[name] = i
        if "graph number" not in labels or "data" not in labels:
            continue
        width = max(len(r) for r in rows)
        rows = [r + [None] * (width - len(r)) for r in rows]
        number_row, header_row = rows[labels["graph number"]], rows[labels["data"]]
        starts = [c for c in range(1, width) if _cell(number_row[c])]
        body = rows[labels["data"] + 1:]
        for b, start in enumerate(starts):
            end = starts[b + 1] if b + 1 < len(starts) else width
            meta = {
                "issue_date": issue_date, "sheet": sheet.title, "graph_number": _cell(number_row[start]),
                "graph_title": _cell(rows[labels["graph title"]][start]) if "graph title" in labels else "",
                "source": _cell(rows[labels.get("source", labels.get("sources", -1))][start])
                if ("source" in labels or "sources" in labels) else "",
            }
            if _cell(header_row[start]).lower().startswith("data restricted"):
                restricted.append(meta)
                continue
            panel, x_col = "", None
            for c in range(start, end):
                header = _cell(header_row[c])
                if "panel" in labels and _cell(rows[labels["panel"]][c]):
                    panel = _cell(rows[labels["panel"]][c])
                column = [r[c] for r in body if r[c] is not None]
                if not header and not column:
                    continue
                numeric = [v for v in column if isinstance(v, (int, float)) and not isinstance(v, bool)]
                if not column or len(numeric) < len(column) / 2 or header.lower().startswith("date"):
                    x_col = c  # dates or categories: the x axis for the columns that follow
                    continue
                for r in body:
                    v = r[c]
                    if isinstance(v, (int, float)) and not isinstance(v, bool):
                        values.append({**meta, "panel": panel, "series": header,
                                       "x_label": _cell(header_row[x_col]) if x_col is not None else "",
                                       "x": _cell(r[x_col]) if x_col is not None else "", "value": float(v)})
    return values, restricted


def store_graph_data(conn: sqlite3.Connection, data_dir: Path, pub: str) -> tuple[int, int]:
    """Parse every graph-data workbook in data_dir into {pub}_graph_series / {pub}_graph_values
    (long format in v_{pub}_graph_data). Returns (values, restricted graphs)."""
    conn.executescript(f"DROP VIEW IF EXISTS v_{pub}_graph_data; DROP TABLE IF EXISTS {pub}_graph_data; "
                       f"DROP TABLE IF EXISTS {pub}_graph_values; DROP TABLE IF EXISTS {pub}_graph_series;"
                       + DATA_SCHEMA.format(pub=pub))
    series_cols = ("issue_date", "sheet", "graph_number", "graph_title", "source", "panel", "series", "x_label")
    n_values = n_restricted = 0
    for path in sorted(data_dir.glob(f"{pub}-graph-data-*.xlsx")):
        issue_date = re.search(r"(\d{4}-\d{2})", path.name).group(1)
        values, restricted = parse_graph_data(path, issue_date)
        ids: dict[tuple, int] = {}
        for v in values:
            key = tuple(v[c] for c in series_cols)
            if key not in ids:
                ids[key] = conn.execute(
                    f"INSERT INTO {pub}_graph_series ({', '.join(series_cols)}) VALUES ({', '.join('?' * len(series_cols))}) "
                    "RETURNING series_id", key).fetchone()[0]
        conn.executemany(f"INSERT INTO {pub}_graph_values (series_id, x, value) VALUES (?, ?, ?)",
                         [(ids[tuple(v[c] for c in series_cols)], v["x"], v["value"]) for v in values])
        n_values += len(values)
        n_restricted += len(restricted)
    conn.commit()
    return n_values, n_restricted
