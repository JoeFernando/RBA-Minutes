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

import html as html_lib
import json
import logging
import re
import sqlite3
import time
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from urllib.parse import urljoin, urlparse

from bs4 import BeautifulSoup

from . import config, db
from .clean import _find_content, normalise_text
from .scrape import make_session

log = logging.getLogger(__name__)

GRAPH_LABEL = re.compile(r"^(?:Graph|Figure|Chart)\s*([A-Z]?\d+(?:\.\d+)?[a-z]?|[A-Z]\d*)\s*[:.–-]?\s*(.*)$", re.S)


def _split_label(text: str) -> tuple[str, str]:
    text = normalise_text(text)
    match = GRAPH_LABEL.match(text)
    return (match.group(1), match.group(2).strip()) if match else ("", text)


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
            if alt_number:  # alt reads "Graph 1: Title"
                number, title, description = alt_number, alt_title, ""
            else:  # 2024+: number in the caption, a long description in alt
                number, title = _split_label(caption_text)
                description = alt
            found.append({"number": number, "title": title, "description": description,
                          "image_url": url, "graph_page": None})
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


def _svg_text(svg: bytes) -> tuple[str, str]:
    """(title, all text) from an SVG graph. The title is the largest (bold) text element."""
    try:
        soup = BeautifulSoup(svg, "xml")
    except Exception:  # noqa: BLE001 - a malformed SVG should not stop the run
        return "", ""
    texts = []
    for t in soup.find_all("text"):
        value = _unsquash(normalise_text(t.get_text(" ")))
        try:
            size = float(re.sub(r"[^\d.]", "", t.get("font-size", "0")) or 0)
        except ValueError:
            size = 0.0
        texts.append((size + (1 if t.get("font-weight") == "bold" else 0), value))
    title = max(texts, default=(0, ""))[1] if texts else ""
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
                graph["chapter_title"] = re.sub(r"^.*?(Statement on Monetary Policy|Financial Stability Review)\s*[–-]\s*\w+\s+\d{4}\s*", "", chapter)
                if graph["graph_page"]:
                    page_html = fetch(graph["graph_page"])
                    if page_html:
                        inner = find_graphs(page_html, graph["graph_page"])
                        image = next((g for g in inner if g["image_url"]), None)
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
