"""Statement on Monetary Policy (SMP): download, clean, store and score every issue since 1997.

The SMP has been published in three layouts:

- 1997-2005: one page per issue in the RBA Bulletin, chapters marked by <h2>
- 2006-2023: an issue page linking to one page per chapter and per box
- 2024 onwards: an "In Brief" issue page plus a contents page linking to chapters

Each issue is saved under data/raw_smp/<YYYY-MM>/ with a manifest.json listing
its pages in reading order, then parsed into issues, chapters and paragraphs.
"""

import datetime as dt
import hashlib
import json
import logging
import re
import sqlite3
import time
from pathlib import Path
from typing import Literal
from urllib.parse import urljoin, urlparse

from bs4 import BeautifulSoup
from pydantic import BaseModel, Field

from . import config, db, sentiment_dict
from .clean import _blocks, _find_content, _remove_noise, normalise_text
from .scrape import make_session

log = logging.getLogger(__name__)

SMP_INDEX = f"{config.BASE_URL}/publications/smp/"
RAW_DIR = config.DATA_DIR / "raw_smp"
PARSER_VERSION = "1"

MONTHS = {m: i for i, m in enumerate(
    ["jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"], 1)}
BULLETIN_ISSUE = re.compile(r"^/publications/bulletin/(\d{4})/([a-z]{3})/\d+\.html$")
SMP_ISSUE = re.compile(r"^/publications/smp/(\d{4})/([a-z]{3})/$")
SKIP_PAGES = {"graphs.html", "tables.html", "contents.html", "boxes.html", "index.html"}

CHAPTER_CATEGORIES = [
    ("administrative", r"copyright|disclaimer|abbreviations|glossary|^list of (figures|graphs|tables)"),
    ("forecast_table", r"^forecast table|^forecasts?$"),
    ("in_depth", r"^in depth\b"),
    ("overview", r"overview|introduction|in brief"),
    ("outlook", r"outlook|forecast"),
    ("inflation", r"inflation|price|wage"),
    ("financial", r"financial|market|foreign exchange|credit|interest rate"),
    ("international", r"international|world|global|east asia|united states|europe|china"),
    ("domestic", r"domestic|australia|economic|activity|labour|payments|foreign trade|capital flows|drought"),
]
# Stored, but left out of the issue's narrative text and its sentiment scores.
# Boxes and "In Depth" chapters are the special studies; they are kept as
# chapters of their own (see v_smp_special_studies).
NON_NARRATIVE = {"box", "in_depth", "administrative", "forecast_table"}
END_OF_TEXT = re.compile(r"^(footnotes?|endnotes?|references)\b", re.IGNORECASE)
BOX_MARKER = re.compile(r"^@@BOX-(\d+)@@$")
CHART_NOTE = re.compile(r"^(sources?|notes?)\s*:", re.IGNORECASE)
# Page furniture and chart fragments that are not prose: download links,
# bare axis numbers, and short chart notes such as "(a) Excluding exports".
FRAGMENT = re.compile(
    r"^download\b.*(\d\s*(kb|mb)|pdf version of this (table|graph|page))$|^[−–+-]?\d+(\.\d+)?$|^\([a-z]\)\s+\S+(\s+\S+){0,4}$",
    re.IGNORECASE,
)


# --- Listing and downloading --------------------------------------------------

def list_issues(html: str) -> list[dict]:
    """Every issue linked from the SMP index page, oldest first."""
    soup = BeautifulSoup(html, "lxml")
    issues = {}
    for anchor in soup.find_all("a", href=True):
        path = urlparse(urljoin(SMP_INDEX, anchor["href"])).path
        for pattern, era in ((BULLETIN_ISSUE, "bulletin"), (SMP_ISSUE, "smp")):
            match = pattern.match(path)
            if match and match.group(2) in MONTHS:
                year, month = int(match.group(1)), MONTHS[match.group(2)]
                issue_date = f"{year:04d}-{month:02d}"
                issues.setdefault(issue_date, {"issue_date": issue_date, "era": era,
                                               "url": f"{config.BASE_URL}{path}"})
    return [issues[k] for k in sorted(issues)]


def _issue_pages(session, issue: dict) -> list[str]:
    """URLs of the pages that make up one issue, in reading order."""
    if issue["era"] == "bulletin":
        return [issue["url"]]
    base = issue["url"]
    html = session.get(base, timeout=config.REQUEST_TIMEOUT_SECONDS).text
    content = _find_content(BeautifulSoup(html, "lxml"))
    hrefs = [urljoin(base, a["href"]) for a in content.find_all("a", href=True)]
    contents_page = next((h for h in hrefs if h.split("#")[0].endswith("/contents.html")), None)
    pages = []
    if contents_page:  # 2024 onwards: the issue page itself is the "In Brief"
        pages.append(base)
        html = session.get(contents_page, timeout=config.REQUEST_TIMEOUT_SECONDS).text
        content = _find_content(BeautifulSoup(html, "lxml"))
        hrefs = [urljoin(contents_page, a["href"]) for a in content.find_all("a", href=True)]
    for href in hrefs:
        url = href.split("#")[0]
        path = urlparse(url).path
        name = path.rsplit("/", 1)[-1]
        if (url.startswith(base) and name.endswith(".html") and name not in SKIP_PAGES
                and url not in pages):
            pages.append(url)
    return pages


def scrape_all(raw_dir: Path = RAW_DIR, refresh: bool = False) -> list[dict]:
    """Download every issue not yet cached. Returns the issue list."""
    session = make_session()
    issues = list_issues(session.get(SMP_INDEX, timeout=config.REQUEST_TIMEOUT_SECONDS).text)
    for issue in issues:
        folder = raw_dir / issue["issue_date"]
        manifest_path = folder / "manifest.json"
        if manifest_path.exists() and not refresh:
            continue
        pages = _issue_pages(session, issue)
        folder.mkdir(parents=True, exist_ok=True)
        files = []
        for n, url in enumerate(pages, 1):
            name = urlparse(url).path.rstrip("/").rsplit("/", 1)[-1]
            name = "in-brief.html" if url == issue["url"] and issue["era"] == "smp" else name
            filename = f"{n:02d}_{name}"
            response = session.get(url, timeout=config.REQUEST_TIMEOUT_SECONDS)
            response.raise_for_status()
            (folder / filename).write_bytes(response.content)
            files.append({"url": url, "file": filename})
            time.sleep(config.REQUEST_DELAY_SECONDS)
        manifest_path.write_text(json.dumps({**issue, "pages": files}, indent=2))
        log.info("%s: %d pages", issue["issue_date"], len(files))
    raw_dir.mkdir(parents=True, exist_ok=True)
    (raw_dir / "issues.json").write_text(json.dumps(issues, indent=2))
    return issues


# --- Parsing ------------------------------------------------------------------

def categorise_chapter(title: str, url: str = "") -> str:
    if url.rsplit("/", 1)[-1].startswith("box") or re.match(r"^box\b", title, re.IGNORECASE):
        return "box"
    lowered = title.lower()
    for category, pattern in CHAPTER_CATEGORIES:
        if re.search(pattern, lowered):
            return category
    return "other"


def _clean_title(title: str) -> str:
    title = re.sub(r"^Statement on Monetary Policy\s*[–-]\s*[A-Za-z]+\s+\d{4}\s*", "", title)
    return re.sub(r"^\d+(\.\d+)*\.?\s+", "", title).strip() or "Untitled"


def _paragraphs(content) -> list[tuple[str, str, str]]:
    """(tag name, kind, text) for each block, skipping footnote sections."""
    out, skipping = [], False
    for name, el in _blocks(content):
        text = normalise_text(el.get_text(" "))
        if not text:
            continue
        if name in ("h2", "h3", "h4") and END_OF_TEXT.match(text):
            skipping = True  # e.g. "Footnotes" or "Footnotes Box 1": skip until the next chapter heading
            continue
        if skipping and name == "h2":
            skipping = False
        if skipping:
            continue
        if CHART_NOTE.match(text) and len(text) < 300:
            continue  # "Sources: ABS; RBA." under graphs and tables
        if name in ("p", "li") and FRAGMENT.match(text):
            continue
        kind = {"h1": "title", "h2": "subheading", "h3": "subheading", "h4": "subheading",
                "li": "list_item"}.get(name, "text")
        out.append((name, kind, text))
    return out


def _extract_inline_boxes(content, url: str) -> list[dict]:
    """Take 1997-2005 boxes (div.box-info, embedded mid-chapter) out as chapters of their own.

    Each box is replaced by a marker heading so it can be put back in reading
    order, and so the chapter text that follows it stays with the chapter.
    """
    boxes = []
    for div in content.find_all("div", class_="box-info"):
        if div.decomposed or div.find_parent("div", class_="box-info") is not None:
            continue
        blocks = _paragraphs(div)
        title = next((t for n, k, t in blocks if n in ("h2", "h3")), "Box")
        paragraphs = [(k, t) for n, k, t in blocks if not (n in ("h2", "h3") and t == title)]
        boxes.append({"title": title, "url": url, "paragraphs": paragraphs, "content_html": str(div)})
        marker = BeautifulSoup(f"<h2>@@BOX-{len(boxes) - 1}@@</h2>", "lxml").h2
        div.replace_with(marker)
    return boxes


def _published_date(soup) -> str | None:
    meta = soup.find("meta", attrs={"name": "dc.date"})
    return meta["content"][:10] if meta and meta.get("content") else None


def parse_issue(folder: Path) -> dict:
    """Parse one downloaded issue into {issue fields, chapters: [...]}."""
    manifest = json.loads((folder / "manifest.json").read_text())
    chapters, published, title = [], None, None
    for page in manifest["pages"]:
        raw = (folder / page["file"]).read_bytes()
        soup = BeautifulSoup(raw, "lxml")
        published = published or _published_date(soup)
        content = _find_content(soup)
        content_html = str(content)
        _remove_noise(content)
        boxes = _extract_inline_boxes(content, page["url"]) if manifest["era"] == "bulletin" else []
        blocks = _paragraphs(content)
        if manifest["era"] == "bulletin":
            # One page holds the whole issue: each <h2> starts a chapter. In
            # some years the introduction has no heading and sits before the
            # first <h2>, after an "On This Page" table of contents.
            current = {"title": "Introduction", "url": page["url"], "paragraphs": [], "content_html": ""}
            chapters.append(current)
            for name, kind, text in blocks:
                if name == "h1":
                    title = title or text
                elif name == "h2" and BOX_MARKER.match(text):
                    # A box sat here, inside the chapter; the chapter's text carries on after it.
                    chapters.append(boxes[int(BOX_MARKER.match(text).group(1))])
                elif name == "h2" and text.lower() == "on this page":
                    current = None  # skip the contents list
                elif name == "h2":
                    current = {"title": text, "url": page["url"], "paragraphs": [], "content_html": ""}
                    chapters.append(current)
                elif current is not None and not text.startswith("Download the complete"):
                    current["paragraphs"].append((kind, text))
            if chapters:
                chapters[0]["content_html"] = content_html
        else:
            heading = next((t for n, k, t in blocks if n == "h1"), "")
            if page["file"].endswith("in-brief.html"):
                heading = "In Brief"
            title = title or f"Statement on Monetary Policy – {manifest['issue_date']}"
            chapters.append({
                "title": _clean_title(heading),
                "url": page["url"],
                "paragraphs": [(k, t) for n, k, t in blocks if n != "h1"],
                "content_html": content_html,
            })
    chapters = [c for c in chapters if c["paragraphs"]]
    for c in chapters:
        c["category"] = categorise_chapter(c["title"], c["url"])
        c["text"] = "\n\n".join(t for _, t in c["paragraphs"])
    if not chapters:
        raise ValueError(f"No chapters found in {folder}")
    return {
        "issue_date": manifest["issue_date"],
        "era": manifest["era"],
        "url": manifest["url"],
        "title": title,
        "published_date": published,
        "chapters": chapters,
    }


# --- Storage ------------------------------------------------------------------

SCHEMA = """
CREATE TABLE IF NOT EXISTS smp_issues (
    issue_id        INTEGER PRIMARY KEY,
    issue_date      TEXT NOT NULL UNIQUE,   -- YYYY-MM of the issue
    year            INTEGER NOT NULL,
    quarter_month   INTEGER NOT NULL,
    era             TEXT NOT NULL,          -- bulletin (1997-2005) / smp
    url             TEXT NOT NULL,
    title           TEXT,
    published_date  TEXT,                   -- from the page's dc.date metadata where present
    n_chapters      INTEGER NOT NULL,
    n_words         INTEGER NOT NULL,
    full_text       TEXT NOT NULL,          -- narrative chapters with titles (no boxes, forecast tables or notices)
    parser_version  TEXT NOT NULL,
    parsed_at       TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS smp_chapters (
    chapter_id      INTEGER PRIMARY KEY,
    issue_id        INTEGER NOT NULL REFERENCES smp_issues(issue_id) ON DELETE CASCADE,
    position        INTEGER NOT NULL,
    title           TEXT NOT NULL,
    category        TEXT NOT NULL,          -- overview / international / financial / domestic / inflation / outlook / box / forecast_table / administrative / other
    url             TEXT NOT NULL,
    text            TEXT NOT NULL,
    n_words         INTEGER NOT NULL,
    content_html    TEXT NOT NULL,
    UNIQUE (issue_id, position)
);

CREATE TABLE IF NOT EXISTS smp_paragraphs (
    paragraph_id    INTEGER PRIMARY KEY,
    issue_id        INTEGER NOT NULL REFERENCES smp_issues(issue_id) ON DELETE CASCADE,
    chapter_id      INTEGER NOT NULL REFERENCES smp_chapters(chapter_id) ON DELETE CASCADE,
    position        INTEGER NOT NULL,
    kind            TEXT NOT NULL,
    text            TEXT NOT NULL,
    UNIQUE (issue_id, position)
);

CREATE VIRTUAL TABLE IF NOT EXISTS smp_paragraphs_fts USING fts5(
    text, content='smp_paragraphs', content_rowid='paragraph_id', tokenize='porter unicode61'
);

CREATE TABLE IF NOT EXISTS smp_dictionary_sentiment (
    issue_id            INTEGER NOT NULL REFERENCES smp_issues(issue_id) ON DELETE CASCADE,
    scope               TEXT NOT NULL,      -- issue / overview / outlook / chapter
    chapter_id          INTEGER REFERENCES smp_chapters(chapter_id) ON DELETE CASCADE,
    n_tokens            INTEGER NOT NULL,
    lm_positive         INTEGER NOT NULL,
    lm_negative         INTEGER NOT NULL,
    lm_uncertainty      INTEGER NOT NULL,
    lm_constraining     INTEGER NOT NULL,
    lm_tone             REAL,
    lm_net_per_1k       REAL,
    lm_uncertainty_per_1k REAL,
    hawkish             INTEGER NOT NULL,
    dovish              INTEGER NOT NULL,
    hawk_dove_index     REAL,
    dictionary_version  TEXT NOT NULL,
    scored_at           TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS smp_llm_sentiment (
    smp_llm_sentiment_id INTEGER PRIMARY KEY,
    issue_date          TEXT NOT NULL,
    model               TEXT NOT NULL,
    prompt_version      TEXT NOT NULL,
    input_scope         TEXT NOT NULL,
    input_sha256        TEXT NOT NULL,
    stance              TEXT,
    stance_score        INTEGER,
    confidence          INTEGER,
    policy_bias         TEXT,
    inflation_outlook   TEXT,               -- above_target / within_target / below_target
    growth_outlook      TEXT,               -- strengthening / steady / weakening
    inflation_risks     TEXT,               -- upside / balanced / downside
    dominant_concern    TEXT,
    key_quotes          TEXT,
    quotes_verified     INTEGER,
    reasoning           TEXT,
    created_at          TEXT NOT NULL,
    UNIQUE (issue_date, model, prompt_version, input_scope, input_sha256)
);

CREATE VIEW IF NOT EXISTS v_smp_special_studies AS
SELECT i.issue_date, c.position, c.category, c.title, c.n_words, c.url, c.text,
       d.lm_tone, d.lm_uncertainty_per_1k, d.hawk_dove_index
FROM smp_chapters c
JOIN smp_issues i USING (issue_id)
LEFT JOIN smp_dictionary_sentiment d ON d.chapter_id = c.chapter_id AND d.scope = 'chapter'
WHERE c.category IN ('box', 'in_depth')
ORDER BY i.issue_date, c.position;

CREATE VIEW IF NOT EXISTS v_smp_sentiment AS
SELECT
    i.issue_date, i.year, i.era, i.published_date, i.n_words,
    d.lm_tone, d.lm_net_per_1k, d.lm_uncertainty_per_1k, d.hawk_dove_index,
    o.lm_tone AS overview_lm_tone, o.hawk_dove_index AS overview_hawk_dove_index,
    l.stance AS llm_stance, l.stance_score AS llm_stance_score, l.confidence AS llm_confidence,
    l.policy_bias AS llm_policy_bias, l.inflation_outlook AS llm_inflation_outlook,
    l.growth_outlook AS llm_growth_outlook, l.inflation_risks AS llm_inflation_risks,
    l.dominant_concern AS llm_dominant_concern, i.url
FROM smp_issues i
LEFT JOIN smp_dictionary_sentiment d ON d.issue_id = i.issue_id AND d.scope = 'issue'
LEFT JOIN smp_dictionary_sentiment o ON o.issue_id = i.issue_id AND o.scope = 'overview'
LEFT JOIN smp_llm_sentiment l ON l.smp_llm_sentiment_id = (
    SELECT MAX(smp_llm_sentiment_id) FROM smp_llm_sentiment x WHERE x.issue_date = i.issue_date
)
ORDER BY i.issue_date;
"""


def connect(path: Path = config.DB_PATH) -> sqlite3.Connection:
    conn = db.connect(path)
    conn.executescript(SCHEMA)
    return conn


def upsert_issue(conn: sqlite3.Connection, issue: dict) -> int:
    main = [c for c in issue["chapters"] if c["category"] not in NON_NARRATIVE]
    full_text = "\n\n".join(f"{c['title']}\n\n{c['text']}" for c in main)
    row = {
        "issue_date": issue["issue_date"],
        "year": int(issue["issue_date"][:4]),
        "quarter_month": int(issue["issue_date"][5:7]),
        "era": issue["era"],
        "url": issue["url"],
        "title": issue["title"],
        "published_date": issue["published_date"],
        "n_chapters": len(issue["chapters"]),
        "n_words": db.word_count(full_text),
        "full_text": full_text,
        "parser_version": PARSER_VERSION,
        "parsed_at": db.now(),
    }
    updates = ", ".join(f"{c} = excluded.{c}" for c in row if c != "issue_date")
    issue_id = conn.execute(
        f"INSERT INTO smp_issues ({', '.join(row)}) VALUES ({', '.join(':' + c for c in row)}) "
        f"ON CONFLICT(issue_date) DO UPDATE SET {updates} RETURNING issue_id",
        row,
    ).fetchone()[0]
    conn.execute("DELETE FROM smp_paragraphs WHERE issue_id = ?", (issue_id,))
    conn.execute("DELETE FROM smp_chapters WHERE issue_id = ?", (issue_id,))
    position = 0
    for n, chapter in enumerate(issue["chapters"]):
        chapter_id = conn.execute(
            "INSERT INTO smp_chapters (issue_id, position, title, category, url, text, n_words, content_html) "
            "VALUES (?, ?, ?, ?, ?, ?, ?, ?) RETURNING chapter_id",
            (issue_id, n, chapter["title"], chapter["category"], chapter["url"], chapter["text"],
             db.word_count(chapter["text"]), chapter["content_html"]),
        ).fetchone()[0]
        for kind, text in chapter["paragraphs"]:
            conn.execute(
                "INSERT INTO smp_paragraphs (issue_id, chapter_id, position, kind, text) VALUES (?, ?, ?, ?, ?)",
                (issue_id, chapter_id, position, kind, text),
            )
            position += 1
    return issue_id


def build(conn: sqlite3.Connection, raw_dir: Path = RAW_DIR) -> tuple[int, list[str]]:
    failures, loaded = [], 0
    for folder in sorted(p for p in raw_dir.iterdir() if (p / "manifest.json").exists()):
        try:
            upsert_issue(conn, parse_issue(folder))
            loaded += 1
        except ValueError as e:
            failures.append(f"{folder.name}: {e}")
    conn.execute("INSERT INTO smp_paragraphs_fts(smp_paragraphs_fts) VALUES ('rebuild')")
    conn.commit()
    return loaded, failures


# --- Dictionary sentiment -----------------------------------------------------

def score_dictionary(conn: sqlite3.Connection) -> int:
    version = sentiment_dict.dictionary_version()
    scored_at = db.now()
    conn.execute("DELETE FROM smp_dictionary_sentiment")
    issues = conn.execute("SELECT issue_id FROM smp_issues ORDER BY issue_date").fetchall()
    for (issue_id,) in issues:
        chapters = conn.execute(
            "SELECT chapter_id, category, text FROM smp_chapters WHERE issue_id = ? ORDER BY position",
            (issue_id,),
        ).fetchall()
        main = [c for c in chapters if c["category"] not in NON_NARRATIVE]
        scopes = [("issue", None, "\n\n".join(c["text"] for c in main))]
        for scope in ("overview", "outlook"):
            text = "\n\n".join(c["text"] for c in main if c["category"] == scope)
            if text:
                scopes.append((scope, None, text))
        scopes += [("chapter", c["chapter_id"], c["text"]) for c in chapters]
        for scope, chapter_id, text in scopes:
            row = {"issue_id": issue_id, "scope": scope, "chapter_id": chapter_id,
                   **sentiment_dict.score_text(text), "dictionary_version": version, "scored_at": scored_at}
            conn.execute(
                f"INSERT INTO smp_dictionary_sentiment ({', '.join(row)}) VALUES ({', '.join(':' + c for c in row)})",
                row,
            )
    conn.commit()
    return len(issues)


# --- LLM labelling inside a Claude Code session --------------------------------

PROMPT_VERSION = "smp-v1"
SESSION_MODEL = "claude-opus-5-5 (Claude Code session)"

SYSTEM_PROMPT = """You are an expert on Australian monetary policy. You will read the Overview (or, \
for 1997-2005 issues, the Introduction) of one Reserve Bank of Australia Statement on Monetary Policy \
and assess the monetary policy stance and outlook it communicates.

Judge this document only. Do not use knowledge of what happened after it was published.

Definitions:
- stance: "hawkish" means leaning towards tighter policy or concerned that inflation is too high; \
"dovish" means leaning towards easier policy or concerned about weak activity or rising unemployment; \
"neutral" means balanced or with no clear lean.
- stance_score: -2 strongly dovish, -1 mildly dovish, 0 neutral, +1 mildly hawkish, +2 strongly hawkish.
- confidence: how committed the language is, from 1 (heavily hedged) to 5 (unambiguous).
- policy_bias: the direction signalled for future policy: "tightening", "easing" or "neutral".
- inflation_outlook: where inflation is forecast to be over the forecast horizon relative to the \
2-3 per cent target: "above_target", "within_target" or "below_target".
- growth_outlook: the direction of economic growth over the forecast horizon: "strengthening", \
"steady" or "weakening".
- inflation_risks: the stated balance of risks to the inflation outlook: "upside", "balanced" or \
"downside".
- dominant_concern: "inflation", "growth_employment", "financial_stability", "balanced" or "other".
- key_quotes: up to three short sentences copied exactly, word for word, from the text that best \
support your assessment.
- reasoning: two to four sentences explaining the assessment."""

SESSION_INSTRUCTIONS = """For each issue in this file, read its text and assess it using the \
definitions below. Write a JSON array to the output file named in the header, with one object per \
issue, in the same order, each with exactly these keys: issue_date, input_sha256 (both copied from \
the issue header), stance, stance_score, confidence, policy_bias, inflation_outlook, growth_outlook, \
inflation_risks, dominant_concern, key_quotes, reasoning. Assess each issue on its own text only.

"""


class SMPStance(BaseModel):
    stance: Literal["hawkish", "neutral", "dovish"]
    stance_score: int = Field(description="-2 strongly dovish to +2 strongly hawkish")
    confidence: int = Field(description="1 heavily hedged to 5 unambiguous")
    policy_bias: Literal["tightening", "neutral", "easing"]
    inflation_outlook: Literal["above_target", "within_target", "below_target"]
    growth_outlook: Literal["strengthening", "steady", "weakening"]
    inflation_risks: Literal["upside", "balanced", "downside"]
    dominant_concern: Literal["inflation", "growth_employment", "financial_stability", "balanced", "other"]
    key_quotes: list[str]
    reasoning: str


def issue_input(conn: sqlite3.Connection, issue_id: int) -> str:
    """The overview chapter(s); the first non-box chapter if an issue has none."""
    rows = conn.execute(
        "SELECT title, category, text FROM smp_chapters WHERE issue_id = ? ORDER BY position", (issue_id,)
    ).fetchall()
    chosen = [r for r in rows if r["category"] == "overview" and r["title"] != "In Brief"]
    chosen = chosen or [r for r in rows if r["category"] == "overview"]
    chosen = chosen or [next(r for r in rows if r["category"] != "box")]
    return "\n\n".join(f"## {r['title']}\n\n{r['text']}" for r in chosen)


def _current_inputs(conn: sqlite3.Connection) -> dict:
    out = {}
    for issue in conn.execute("SELECT issue_id, issue_date FROM smp_issues ORDER BY issue_date"):
        text = issue_input(conn, issue["issue_id"])
        out[issue["issue_date"]] = (hashlib.sha256(text.encode()).hexdigest(), text)
    return out


def export_batches(conn: sqlite3.Connection, out_dir: Path, batch_size: int = 10,
                   model: str = SESSION_MODEL) -> list[Path]:
    out_dir.mkdir(parents=True, exist_ok=True)
    pending = []
    for issue_date, (sha, text) in _current_inputs(conn).items():
        if not conn.execute(
            "SELECT 1 FROM smp_llm_sentiment WHERE issue_date = ? AND model = ? AND prompt_version = ? "
            "AND input_sha256 = ?", (issue_date, model, PROMPT_VERSION, sha),
        ).fetchone():
            pending.append((issue_date, sha, text))
    paths = []
    for n, start in enumerate(range(0, len(pending), batch_size), 1):
        batch = pending[start:start + batch_size]
        path = out_dir / f"smp_batch_{n:03d}.md"
        parts = [
            f"# SMP batch {n:03d}: {len(batch)} issues ({batch[0][0]} to {batch[-1][0]})\n",
            f"Output file: smp_batch_{n:03d}.labels.json (same folder)\n",
            f"Prompt version: {PROMPT_VERSION}\n",
            "## Instructions\n",
            SESSION_INSTRUCTIONS + SYSTEM_PROMPT + "\n",
        ]
        parts += [f"\n=== ISSUE {d} | input_sha256 {sha} ===\n\n{text}\n" for d, sha, text in batch]
        path.write_text("\n".join(parts), encoding="utf-8")
        paths.append(path)
    return paths


def import_labels(conn: sqlite3.Connection, label_files, model: str = SESSION_MODEL) -> tuple[int, list[str]]:
    from .sentiment_llm import quotes_verified

    current = _current_inputs(conn)
    stored, problems = 0, []
    for label_file in label_files:
        try:
            items = json.loads(Path(label_file).read_text(encoding="utf-8"))
        except json.JSONDecodeError as e:
            problems.append(f"{label_file}: invalid JSON ({e})")
            continue
        for item in items:
            issue_date = item.get("issue_date")
            if issue_date not in current:
                problems.append(f"{label_file}: unknown issue {issue_date}")
                continue
            sha, text = current[issue_date]
            if item.get("input_sha256") != sha:
                problems.append(f"{label_file}: {issue_date} was labelled on different text; re-export it")
                continue
            try:
                result = SMPStance.model_validate(item)
            except ValueError as e:
                problems.append(f"{label_file}: {issue_date} failed validation ({e})")
                continue
            row = {
                "issue_date": issue_date, "model": model, "prompt_version": PROMPT_VERSION,
                "input_scope": "overview", "input_sha256": sha,
                **result.model_dump(exclude={"key_quotes", "stance_score", "confidence"}),
                "stance_score": max(-2, min(2, result.stance_score)),
                "confidence": max(1, min(5, result.confidence)),
                "key_quotes": json.dumps(result.key_quotes, ensure_ascii=False),
                "quotes_verified": int(quotes_verified(result.key_quotes, text)),
                "created_at": db.now(),
            }
            conn.execute(
                f"INSERT OR REPLACE INTO smp_llm_sentiment ({', '.join(row)}) "
                f"VALUES ({', '.join(':' + c for c in row)})",
                row,
            )
            stored += 1
    conn.commit()
    return stored, problems


# --- Forecast files -------------------------------------------------------------

# The archive has detailed SMP forecast tables from November 2018; the
# historical files have central forecasts, actuals and errors since 1990
# (released with a one-year lag).
FORECAST_FILES = {
    name: f"{config.BASE_URL}/statistics/xls/{name}"
    for name in (
        "smp-forecast-archive.xlsx",
        "forecast-date-by-event-date.xls",
        "cpi-by-horizon.xls",
        "underlying-inflation-by-horizon.xls",
        "gdp-by-horizon.xls",
        "unemployment-rate-by-horizon.xls",
    )
}


def download_forecasts(out_dir: Path = config.DATA_DIR / "forecasts") -> list[Path]:
    """Download the RBA's forecast archive workbook(s) for later analysis."""
    session = make_session()
    out_dir.mkdir(parents=True, exist_ok=True)
    paths = []
    for name, url in FORECAST_FILES.items():
        response = session.get(url, timeout=config.REQUEST_TIMEOUT_SECONDS)
        response.raise_for_status()
        (out_dir / name).write_bytes(response.content)
        paths.append(out_dir / name)
    (out_dir / "downloaded_at.txt").write_text(dt.datetime.now(dt.timezone.utc).isoformat() + "\n")
    return paths


# --- Coverage checks ---------------------------------------------------------------

BOX_LINK = re.compile(
    r"/publications/(?:bulletin|smp)/(\d{4})/([a-z]{3})/(?:\d+\.html#box-?([a-z]|\d+)|box-([a-z]|\d+)(?:-[\w-]*)?\.html)$"
)


def fetch_box_index(raw_dir: Path = RAW_DIR, first_year: int = 1997, last_year: int | None = None) -> list[dict]:
    """The RBA's own list of every SMP box, from each year's boxes.html page."""
    session = make_session()
    last_year = last_year or dt.date.today().year
    boxes = []
    for year in range(first_year, last_year + 1):
        url = f"{SMP_INDEX}{year}/boxes.html"
        response = session.get(url, timeout=config.REQUEST_TIMEOUT_SECONDS)
        if response.status_code == 404:
            continue
        response.raise_for_status()
        content = _find_content(BeautifulSoup(response.text, "lxml"))
        for anchor in content.find_all("a", href=True):
            link = urljoin(url, anchor["href"])
            match = BOX_LINK.search(urlparse(link).path + ("#" + urlparse(link).fragment if urlparse(link).fragment else ""))
            if match and match.group(2) in MONTHS:
                boxes.append({
                    "issue_date": f"{int(match.group(1)):04d}-{MONTHS[match.group(2)]:02d}",
                    "letter": (match.group(3) or match.group(4)).upper(),
                    "title": normalise_text(anchor.get_text(" ")),
                    "url": link,
                })
        time.sleep(config.REQUEST_DELAY_SECONDS)
    raw_dir.mkdir(parents=True, exist_ok=True)
    (raw_dir / "box_index.json").write_text(json.dumps(boxes, indent=2, ensure_ascii=False))
    return boxes


def check(conn: sqlite3.Connection, raw_dir: Path = RAW_DIR) -> tuple[list[str], list[str]]:
    """(problems, notes) for the SMP tables: issue coverage, overviews, and every listed box captured."""
    problems, notes = [], []
    issues_file = raw_dir / "issues.json"
    listed = {i["issue_date"] for i in json.loads(issues_file.read_text())} if issues_file.exists() else set()
    stored = {r[0] for r in conn.execute("SELECT issue_date FROM smp_issues")}
    for missing in sorted(listed - stored):
        problems.append(f"{missing}: listed on the RBA site but not in the database")
    for issue_date, n_words, overviews in conn.execute(
        "SELECT i.issue_date, i.n_words, SUM(c.category = 'overview') FROM smp_issues i "
        "JOIN smp_chapters c USING (issue_id) GROUP BY i.issue_id ORDER BY i.issue_date"
    ):
        if not overviews:
            problems.append(f"{issue_date}: no overview/introduction chapter")
        if n_words < 5000:
            problems.append(f"{issue_date}: only {n_words} words outside boxes")
    box_file = raw_dir / "box_index.json"
    if not box_file.exists():
        notes.append("box index not downloaded; run smp-boxes to check box coverage")
        return problems, notes
    def title_words(title: str) -> str:
        title = re.sub(r"^box\s*([a-z]|\d+)?\s*[:.–-]?\s*", "", title, flags=re.IGNORECASE)
        return re.sub(r"[^a-z0-9]+", " ", title.lower()).strip()

    captured: dict[str, list[tuple[str, str]]] = {}
    for issue_date, title in conn.execute(
        "SELECT i.issue_date, c.title FROM smp_chapters c JOIN smp_issues i USING (issue_id) WHERE c.category = 'box'"
    ):
        match = re.match(r"^box\s+([a-z]|\d+)\b", title, re.IGNORECASE)
        captured.setdefault(issue_date, []).append((match.group(1).upper() if match else "", title_words(title)))
    boxes = json.loads(box_file.read_text())
    matched = set()
    for box in boxes:
        if box["issue_date"] not in stored:
            continue
        candidates = captured.get(box["issue_date"], [])
        hit = next((c for c in candidates if c[0] == box["letter"]), None) or \
            next((c for c in candidates if c[1] == title_words(box["title"])), None)
        if hit is None:
            problems.append(f"{box['issue_date']}: Box {box['letter']} ({box['title']}) is not captured")
        else:
            matched.add((box["issue_date"], hit))
    for issue_date, items in captured.items():
        for item in items:
            if (issue_date, item) not in matched:
                notes.append(f"{issue_date}: captured box '{item[1]}' is not on the RBA's box list")
    notes.append(f"{len(boxes)} boxes listed by the RBA; {sum(len(v) for v in captured.values())} captured")
    return problems, notes
