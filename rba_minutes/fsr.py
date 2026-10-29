"""Financial Stability Review (FSR): download, clean, store and score every issue since March 2004.

The RBA's first Financial Stability Review was published in March 2004; it
has appeared twice a year since (March/September, April/October from 2015).
Layouts:

- 2004-2019: an issue page linking to one page per chapter, box and article
- 2020-2023: an "At a Glance" issue page plus a contents page
- 2024 onwards: a summary issue page plus a contents page; numbered chapters
  and "Focus Topics" (the successors of boxes)

Each issue is saved under data/raw_fsr/<YYYY-MM>/ with a manifest.json listing
its pages in reading order (the same format as the SMP), then parsed into
issues, chapters and paragraphs in data/rba_fsr.sqlite. Boxes, Focus Topics
and the articles published in the 2004-2007 issues are the special studies:
stored as chapters of their own and kept out of the narrative text.
"""

import datetime as dt
import hashlib
import json
import logging
import re
import sqlite3
import time
import zlib
from pathlib import Path
from typing import Literal
from urllib.parse import urljoin, urlparse

from bs4 import BeautifulSoup
from pydantic import BaseModel, Field

from . import config, db, sentiment_dict
from .clean import _find_content, _remove_noise, normalise_text
from .scrape import make_session
from .smp import MONTHS, _issue_pages, _paragraphs, _published_date

log = logging.getLogger(__name__)

FSR_INDEX = f"{config.BASE_URL}/publications/fsr/"
RAW_DIR = config.DATA_DIR / "raw_fsr"
PARSER_VERSION = "1"

FSR_ISSUE = re.compile(r"^/publications/fsr/(\d{4})/([a-z]{3})/$")
SKIP_PAGES = {"graphs.html", "tables.html", "figures.html", "contents.html", "boxes.html", "index.html"}

# Checked in order; the first match wins. Boxes, Focus Topics and articles are
# recognised from their URL or the RBA's own lists before these are tried.
CHAPTER_CATEGORIES = [
    ("administrative", r"copyright|disclaimer|abbreviations|glossary|^list of (figures|graphs|tables)"),
    ("focus_topic", r"^focus topic\b"),
    ("overview", r"overview|foreword|at a glance|in brief|financial stability assessment|^summary"),
    ("annex", r"^annex\b"),
    ("global", r"global|international|world|macroeconomic|macro-financial"),
    ("regulation", r"regulat|architecture|infrastructure|developments in the financial system"),
    ("household_business", r"household|business|property|real estate|balance sheet|corporate"),
    ("financial_system", r"financial system|financial intermediar|financial institutions|\bbanks?\b"),
]
SPECIAL_STUDIES = {"box", "focus_topic", "article", "special_chapter"}
# A long chapter outside the regular sequence ('Assessing the Effects of Housing
# Lending Policy Measures', October 2018) is a special study too.
SPECIAL_CHAPTER_MIN_WORDS = 2000
NON_NARRATIVE = SPECIAL_STUDIES | {"administrative", "annex"}


# --- Listing and downloading --------------------------------------------------

def list_issues(html: str) -> list[dict]:
    """Every issue linked from the FSR index page, oldest first."""
    soup = BeautifulSoup(html, "lxml")
    issues = {}
    for anchor in soup.find_all("a", href=True):
        path = urlparse(urljoin(FSR_INDEX, anchor["href"])).path
        match = FSR_ISSUE.match(path)
        if match and match.group(2) in MONTHS:
            issue_date = f"{int(match.group(1)):04d}-{MONTHS[match.group(2)]:02d}"
            issues.setdefault(issue_date, {"issue_date": issue_date, "era": "fsr",
                                           "url": f"{config.BASE_URL}{path}"})
    return [issues[k] for k in sorted(issues)]


def scrape_all(raw_dir: Path = RAW_DIR, refresh: bool = False) -> list[dict]:
    """Download every issue not yet cached, plus the RBA's lists of articles and boxes."""
    session = make_session()
    issues = list_issues(session.get(FSR_INDEX, timeout=config.REQUEST_TIMEOUT_SECONDS).text)
    for issue in issues:
        folder = raw_dir / issue["issue_date"]
        manifest_path = folder / "manifest.json"
        if manifest_path.exists() and not refresh:
            continue
        pages = [p for p in _issue_pages(session, issue) if p.rsplit("/", 1)[-1] not in SKIP_PAGES]
        folder.mkdir(parents=True, exist_ok=True)
        files = []
        for n, url in enumerate(pages, 1):
            name = urlparse(url).path.rstrip("/").rsplit("/", 1)[-1]
            name = "summary.html" if url == issue["url"] else name
            response = session.get(url, timeout=config.REQUEST_TIMEOUT_SECONDS)
            response.raise_for_status()
            (folder / f"{n:02d}_{name}").write_bytes(response.content)
            files.append({"url": url, "file": f"{n:02d}_{name}"})
            time.sleep(config.REQUEST_DELAY_SECONDS)
        manifest_path.write_text(json.dumps({**issue, "pages": files}, indent=2))
        log.info("%s: %d pages", issue["issue_date"], len(files))
    raw_dir.mkdir(parents=True, exist_ok=True)
    (raw_dir / "issues.json").write_text(json.dumps(issues, indent=2))
    fetch_special_study_index(raw_dir, session)
    return issues


SPECIAL_LINK = re.compile(r"^/publications/fsr/(\d{4})/([a-z]{3})/([\w.-]+\.html)$")


def fetch_special_study_index(raw_dir: Path = RAW_DIR, session=None) -> dict:
    """The RBA's own lists of FSR articles and of boxes / Focus Topics (one boxes.html per year)."""
    session = session or make_session()
    out = {"articles": [], "boxes": []}
    pages = [("articles", f"{FSR_INDEX}articles/")]
    pages += [("boxes", f"{FSR_INDEX}{year}/boxes.html") for year in range(2004, dt.date.today().year + 1)]
    for kind, url in pages:
        response = session.get(url, timeout=config.REQUEST_TIMEOUT_SECONDS)
        if response.status_code == 404:
            continue
        response.raise_for_status()
        content = _find_content(BeautifulSoup(response.text, "lxml"))
        for anchor in content.find_all("a", href=True):
            link = urljoin(url, anchor["href"]).split("#")[0]
            match = SPECIAL_LINK.match(urlparse(link).path)
            if match and match.group(2) in MONTHS:
                out[kind].append({
                    "issue_date": f"{int(match.group(1)):04d}-{MONTHS[match.group(2)]:02d}",
                    "title": normalise_text(anchor.get_text(" ")),
                    "url": f"{config.BASE_URL}{urlparse(link).path}",
                })
        time.sleep(config.REQUEST_DELAY_SECONDS)
    raw_dir.mkdir(parents=True, exist_ok=True)
    (raw_dir / "special_studies.json").write_text(json.dumps(out, indent=2, ensure_ascii=False))
    return out


# --- Parsing ------------------------------------------------------------------

def _article_urls(raw_dir: Path) -> set[str]:
    path = raw_dir / "special_studies.json"
    return {a["url"] for a in json.loads(path.read_text())["articles"]} if path.exists() else set()


def categorise_chapter(title: str, url: str = "", articles: frozenset | set = frozenset()) -> str:
    name = url.rsplit("/", 1)[-1]
    if name.startswith("box") or re.match(r"^box\b", title, re.IGNORECASE):
        return "box"
    if name.startswith("focus-topic"):
        return "focus_topic"
    if url in articles:
        return "article"
    lowered = title.lower()
    for category, pattern in CHAPTER_CATEGORIES:
        if re.search(pattern, lowered):
            return category
    return "other"


def _clean_title(title: str) -> str:
    title = re.sub(r"^Financial Stability Review\s*[–-]\s*[A-Za-z]+\s+\d{4}\s*", "", title)
    return re.sub(r"^\d+(\.\d+)*\.?\s+", "", title).strip() or "Untitled"


def parse_issue(folder: Path, articles: set[str] = frozenset()) -> dict:
    """Parse one downloaded issue into {issue fields, chapters: [...]}."""
    manifest = json.loads((folder / "manifest.json").read_text())
    chapters, published = [], None
    for page in manifest["pages"]:
        soup = BeautifulSoup((folder / page["file"]).read_bytes(), "lxml")
        published = published or _published_date(soup)
        content = _find_content(soup)
        content_html = str(content)
        _remove_noise(content)
        blocks = _paragraphs(content)
        heading = next((t for n, k, t in blocks if n == "h1"), "")
        if page["file"].endswith("_summary.html"):
            # 2020-2023 'At a Glance'; 2024 onwards a summary with links into the chapters.
            heading = "At a Glance" if int(manifest["issue_date"][:4]) < 2024 else "In Brief"
        paragraphs = [(k, t) for n, k, t in blocks if n != "h1" and not re.match(r"^(more on|read (the|full))\b", t, re.I)]
        chapters.append({"title": _clean_title(heading), "url": page["url"], "paragraphs": paragraphs,
                         "content_html": content_html})
    for c in chapters:
        c["category"] = categorise_chapter(c["title"], c["url"], articles)
        c["text"] = "\n\n".join(t for _, t in c["paragraphs"])
        if c["category"] == "other" and db.word_count(c["text"]) >= SPECIAL_CHAPTER_MIN_WORDS:
            c["category"] = "special_chapter"
    # Pages with no prose are dropped, except special studies that are only a table (Box E, March 2005).
    chapters = [c for c in chapters if c["paragraphs"] or c["category"] in SPECIAL_STUDIES]
    if not chapters:
        raise ValueError(f"No chapters found in {folder}")
    return {
        "issue_date": manifest["issue_date"],
        "url": manifest["url"],
        "title": f"Financial Stability Review – {dt.date(int(manifest['issue_date'][:4]), int(manifest['issue_date'][5:]), 1):%B %Y}",
        "published_date": published,
        "chapters": chapters,
    }


# --- Storage ------------------------------------------------------------------

SCHEMA = """
CREATE TABLE IF NOT EXISTS fsr_issues (
    issue_id        INTEGER PRIMARY KEY,
    issue_date      TEXT NOT NULL UNIQUE,   -- YYYY-MM of the issue
    year            INTEGER NOT NULL,
    month           INTEGER NOT NULL,
    url             TEXT NOT NULL,
    title           TEXT,
    published_date  TEXT,                   -- from the page's dc.date metadata where present
    n_chapters      INTEGER NOT NULL,
    n_words         INTEGER NOT NULL,
    full_text       TEXT NOT NULL,          -- narrative chapters with titles (no boxes, Focus Topics, articles or notices)
    parser_version  TEXT NOT NULL,
    parsed_at       TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS fsr_chapters (
    chapter_id      INTEGER PRIMARY KEY,
    issue_id        INTEGER NOT NULL REFERENCES fsr_issues(issue_id) ON DELETE CASCADE,
    position        INTEGER NOT NULL,
    title           TEXT NOT NULL,
    category        TEXT NOT NULL,          -- overview / global / household_business / financial_system / regulation / box / focus_topic / article / special_chapter / annex / administrative / other
    url             TEXT NOT NULL,
    text            TEXT NOT NULL,
    n_words         INTEGER NOT NULL,
    content_html_zlib BLOB NOT NULL,        -- the page's content block, zlib-compressed (see chapter_html())
    UNIQUE (issue_id, position)
);

CREATE TABLE IF NOT EXISTS fsr_paragraphs (
    paragraph_id    INTEGER PRIMARY KEY,
    issue_id        INTEGER NOT NULL REFERENCES fsr_issues(issue_id) ON DELETE CASCADE,
    chapter_id      INTEGER NOT NULL REFERENCES fsr_chapters(chapter_id) ON DELETE CASCADE,
    position        INTEGER NOT NULL,
    kind            TEXT NOT NULL,
    text            TEXT NOT NULL,
    UNIQUE (issue_id, position)
);

CREATE VIRTUAL TABLE IF NOT EXISTS fsr_paragraphs_fts USING fts5(
    text, content='fsr_paragraphs', content_rowid='paragraph_id', tokenize='porter unicode61'
);

CREATE TABLE IF NOT EXISTS fsr_dictionary_sentiment (
    issue_id            INTEGER NOT NULL REFERENCES fsr_issues(issue_id) ON DELETE CASCADE,
    scope               TEXT NOT NULL,      -- issue / overview / chapter
    chapter_id          INTEGER REFERENCES fsr_chapters(chapter_id) ON DELETE CASCADE,
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

CREATE TABLE IF NOT EXISTS fsr_llm_assessment (
    fsr_llm_assessment_id INTEGER PRIMARY KEY,
    issue_date          TEXT NOT NULL,
    model               TEXT NOT NULL,
    prompt_version      TEXT NOT NULL,
    input_scope         TEXT NOT NULL,
    input_sha256        TEXT NOT NULL,
    risk_level          TEXT,               -- low / moderate / elevated / high
    risk_score          INTEGER,            -- 1 low to 5 high
    risk_trend          TEXT,               -- easing / stable / rising
    system_resilience   TEXT,               -- strong / adequate / weak
    household_stress    TEXT,               -- low / moderate / elevated
    housing_market      TEXT,               -- weakening / stable / strong / overheating
    credit_conditions   TEXT,               -- tight / neutral / loose
    main_risk_source    TEXT,
    policy_implication  TEXT,               -- tightening / neutral / easing: what the concerns imply for interest rates
    confidence          INTEGER,
    key_quotes          TEXT,
    quotes_verified     INTEGER,
    reasoning           TEXT,
    created_at          TEXT NOT NULL,
    UNIQUE (issue_date, model, prompt_version, input_scope, input_sha256)
);

CREATE VIEW IF NOT EXISTS v_fsr_special_studies AS
SELECT i.issue_date, c.position, c.category, c.title, c.n_words, c.url, c.text,
       d.lm_tone, d.lm_uncertainty_per_1k
FROM fsr_chapters c
JOIN fsr_issues i USING (issue_id)
LEFT JOIN fsr_dictionary_sentiment d ON d.chapter_id = c.chapter_id AND d.scope = 'chapter'
WHERE c.category IN ('box', 'focus_topic', 'article', 'special_chapter')
ORDER BY i.issue_date, c.position;

CREATE VIEW IF NOT EXISTS v_fsr_sentiment AS
SELECT
    i.issue_date, i.year, i.published_date, i.n_words,
    d.lm_tone, d.lm_net_per_1k, d.lm_uncertainty_per_1k, d.hawk_dove_index,
    o.lm_tone AS overview_lm_tone, o.lm_uncertainty_per_1k AS overview_lm_uncertainty_per_1k,
    l.risk_level AS llm_risk_level,
    -- the four risk levels as 1-4; use this rather than risk_score, whose 1-5 scale the
    -- labelling batches mapped onto the levels slightly differently
    CASE l.risk_level WHEN 'low' THEN 1 WHEN 'moderate' THEN 2 WHEN 'elevated' THEN 3 WHEN 'high' THEN 4 END
        AS llm_risk_level_ordinal,
    l.risk_score AS llm_risk_score, l.risk_trend AS llm_risk_trend,
    l.system_resilience AS llm_system_resilience, l.household_stress AS llm_household_stress,
    l.housing_market AS llm_housing_market, l.credit_conditions AS llm_credit_conditions,
    l.main_risk_source AS llm_main_risk_source, l.policy_implication AS llm_policy_implication,
    l.confidence AS llm_confidence, i.url
FROM fsr_issues i
LEFT JOIN fsr_dictionary_sentiment d ON d.issue_id = i.issue_id AND d.scope = 'issue'
LEFT JOIN fsr_dictionary_sentiment o ON o.issue_id = i.issue_id AND o.scope = 'overview'
LEFT JOIN fsr_llm_assessment l ON l.fsr_llm_assessment_id = (
    SELECT MAX(fsr_llm_assessment_id) FROM fsr_llm_assessment x WHERE x.issue_date = i.issue_date
)
ORDER BY i.issue_date;
"""


def connect(path: Path = config.FSR_DB_PATH) -> sqlite3.Connection:
    path.parent.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(path)
    conn.row_factory = sqlite3.Row
    conn.execute("PRAGMA foreign_keys = ON")
    conn.executescript(SCHEMA)
    return conn


def chapter_html(conn: sqlite3.Connection, chapter_id: int) -> str:
    blob = conn.execute("SELECT content_html_zlib FROM fsr_chapters WHERE chapter_id = ?", (chapter_id,)).fetchone()[0]
    return zlib.decompress(blob).decode("utf-8")


def upsert_issue(conn: sqlite3.Connection, issue: dict) -> int:
    main = [c for c in issue["chapters"] if c["category"] not in NON_NARRATIVE]
    full_text = "\n\n".join(f"{c['title']}\n\n{c['text']}" for c in main)
    row = {
        "issue_date": issue["issue_date"],
        "year": int(issue["issue_date"][:4]),
        "month": int(issue["issue_date"][5:7]),
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
        f"INSERT INTO fsr_issues ({', '.join(row)}) VALUES ({', '.join(':' + c for c in row)}) "
        f"ON CONFLICT(issue_date) DO UPDATE SET {updates} RETURNING issue_id",
        row,
    ).fetchone()[0]
    conn.execute("DELETE FROM fsr_paragraphs WHERE issue_id = ?", (issue_id,))
    conn.execute("DELETE FROM fsr_chapters WHERE issue_id = ?", (issue_id,))
    position = 0
    for n, chapter in enumerate(issue["chapters"]):
        chapter_id = conn.execute(
            "INSERT INTO fsr_chapters (issue_id, position, title, category, url, text, n_words, content_html_zlib) "
            "VALUES (?, ?, ?, ?, ?, ?, ?, ?) RETURNING chapter_id",
            (issue_id, n, chapter["title"], chapter["category"], chapter["url"], chapter["text"],
             db.word_count(chapter["text"]), zlib.compress(chapter["content_html"].encode("utf-8"), 9)),
        ).fetchone()[0]
        for kind, text in chapter["paragraphs"]:
            conn.execute(
                "INSERT INTO fsr_paragraphs (issue_id, chapter_id, position, kind, text) VALUES (?, ?, ?, ?, ?)",
                (issue_id, chapter_id, position, kind, text),
            )
            position += 1
    return issue_id


def build(conn: sqlite3.Connection, raw_dir: Path = RAW_DIR) -> tuple[int, list[str]]:
    articles = _article_urls(raw_dir)
    failures, loaded = [], 0
    for folder in sorted(p for p in raw_dir.iterdir() if (p / "manifest.json").exists()):
        try:
            upsert_issue(conn, parse_issue(folder, articles))
            loaded += 1
        except ValueError as e:
            failures.append(f"{folder.name}: {e}")
    conn.execute("INSERT INTO fsr_paragraphs_fts(fsr_paragraphs_fts) VALUES ('rebuild')")
    conn.commit()
    return loaded, failures


# --- Dictionary sentiment -----------------------------------------------------

def score_dictionary(conn: sqlite3.Connection) -> int:
    version = sentiment_dict.dictionary_version()
    scored_at = db.now()
    conn.execute("DELETE FROM fsr_dictionary_sentiment")
    issues = conn.execute("SELECT issue_id FROM fsr_issues ORDER BY issue_date").fetchall()
    for (issue_id,) in issues:
        chapters = conn.execute(
            "SELECT chapter_id, category, title, text FROM fsr_chapters WHERE issue_id = ? ORDER BY position",
            (issue_id,),
        ).fetchall()
        main = [c for c in chapters if c["category"] not in NON_NARRATIVE]
        scopes = [("issue", None, "\n\n".join(c["text"] for c in main))]
        overview = issue_input_rows(chapters)
        if overview:
            scopes.append(("overview", None, "\n\n".join(c["text"] for c in overview)))
        scopes += [("chapter", c["chapter_id"], c["text"]) for c in chapters]
        for scope, chapter_id, text in scopes:
            row = {"issue_id": issue_id, "scope": scope, "chapter_id": chapter_id,
                   **sentiment_dict.score_text(text), "dictionary_version": version, "scored_at": scored_at}
            conn.execute(
                f"INSERT INTO fsr_dictionary_sentiment ({', '.join(row)}) VALUES ({', '.join(':' + c for c in row)})",
                row,
            )
    conn.commit()
    return len(issues)


# --- LLM assessment inside a Claude Code session --------------------------------

PROMPT_VERSION = "fsr-v1"
SESSION_MODEL = "claude-opus-5-5 (Claude Code session)"

SYSTEM_PROMPT = """You are an expert on Australian financial stability and monetary policy. You will \
read the Overview (from 2024, the Financial Stability Assessment) of one Reserve Bank of Australia \
Financial Stability Review and record the assessment it communicates.

Judge this document only. Do not use knowledge of what happened after it was published.

Definitions:
- risk_level: the overall level of threats to financial stability the Review describes: "low", \
"moderate", "elevated" or "high".
- risk_score: the same on a scale from 1 (low) to 5 (high).
- risk_trend: whether risks are described as "rising", "stable" or "easing" since the previous Review.
- system_resilience: how the Review judges the Australian financial system's capacity to absorb \
shocks: "strong", "adequate" or "weak".
- household_stress: the degree of financial stress among Australian households (arrears, \
repayment difficulty, cash-flow pressure): "low", "moderate" or "elevated".
- housing_market: the state of Australian housing prices and housing credit: "weakening", "stable", \
"strong" or "overheating".
- credit_conditions: how easy it is to borrow: "tight", "neutral" or "loose".
- main_risk_source: the most prominent source of risk: "global", "household_housing", \
"business_commercial_property", "banks_funding", "financial_markets" or "other".
- policy_implication: what the Review's concerns, taken on their own, imply for interest rates: \
"tightening" (e.g. concern about a credit or housing boom or rising leverage), "easing" (e.g. \
concern about financial stress, deleveraging or tight credit), or "neutral" (no clear implication).
- confidence: how committed the language is, from 1 (heavily hedged) to 5 (unambiguous).
- key_quotes: up to three short sentences copied exactly, word for word, from the text that best \
support your assessment.
- reasoning: two to four sentences explaining the assessment."""

SESSION_INSTRUCTIONS = """For each issue in this file, read its text and assess it using the \
definitions below. Write a JSON array to the output file named in the header, with one object per \
issue, in the same order, each with exactly these keys: issue_date, input_sha256 (both copied from \
the issue header), risk_level, risk_score, risk_trend, system_resilience, household_stress, \
housing_market, credit_conditions, main_risk_source, policy_implication, confidence, key_quotes, \
reasoning. Assess each issue on its own text only.

"""


class FSRAssessment(BaseModel):
    risk_level: Literal["low", "moderate", "elevated", "high"]
    risk_score: int = Field(description="1 low to 5 high")
    risk_trend: Literal["rising", "stable", "easing"]
    system_resilience: Literal["strong", "adequate", "weak"]
    household_stress: Literal["low", "moderate", "elevated"]
    housing_market: Literal["weakening", "stable", "strong", "overheating"]
    credit_conditions: Literal["tight", "neutral", "loose"]
    main_risk_source: Literal["global", "household_housing", "business_commercial_property", "banks_funding",
                              "financial_markets", "other"]
    policy_implication: Literal["tightening", "neutral", "easing"]
    confidence: int = Field(description="1 heavily hedged to 5 unambiguous")
    key_quotes: list[str]
    reasoning: str


def issue_input_rows(chapters) -> list:
    """The Overview (2004-2023) or Financial Stability Assessment (2024 on); not the summary page."""
    rows = [c for c in chapters if c["category"] == "overview"]
    chosen = [c for c in rows if re.search(r"^overview$|financial stability assessment", c["title"], re.I)]
    chosen = chosen or [c for c in rows if c["title"] not in ("At a Glance", "In Brief")]
    return chosen or [next(c for c in chapters if c["category"] not in NON_NARRATIVE)]


def issue_input(conn: sqlite3.Connection, issue_id: int) -> str:
    rows = conn.execute(
        "SELECT title, category, text FROM fsr_chapters WHERE issue_id = ? ORDER BY position", (issue_id,)
    ).fetchall()
    return "\n\n".join(f"## {r['title']}\n\n{r['text']}" for r in issue_input_rows(rows))


def _current_inputs(conn: sqlite3.Connection) -> dict:
    out = {}
    for issue in conn.execute("SELECT issue_id, issue_date FROM fsr_issues ORDER BY issue_date"):
        text = issue_input(conn, issue["issue_id"])
        out[issue["issue_date"]] = (hashlib.sha256(text.encode()).hexdigest(), text)
    return out


def export_batches(conn: sqlite3.Connection, out_dir: Path, batch_size: int = 8,
                   model: str = SESSION_MODEL) -> list[Path]:
    out_dir.mkdir(parents=True, exist_ok=True)
    pending = [
        (issue_date, sha, text) for issue_date, (sha, text) in _current_inputs(conn).items()
        if not conn.execute(
            "SELECT 1 FROM fsr_llm_assessment WHERE issue_date = ? AND model = ? AND prompt_version = ? "
            "AND input_sha256 = ?", (issue_date, model, PROMPT_VERSION, sha)).fetchone()
    ]
    paths = []
    for n, start in enumerate(range(0, len(pending), batch_size), 1):
        batch = pending[start:start + batch_size]
        path = out_dir / f"fsr_batch_{n:03d}.md"
        parts = [
            f"# FSR batch {n:03d}: {len(batch)} issues ({batch[0][0]} to {batch[-1][0]})\n",
            f"Output file: fsr_batch_{n:03d}.labels.json (same folder)\n",
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
                result = FSRAssessment.model_validate(item)
            except ValueError as e:
                problems.append(f"{label_file}: {issue_date} failed validation ({e})")
                continue
            row = {
                "issue_date": issue_date, "model": model, "prompt_version": PROMPT_VERSION,
                "input_scope": "overview", "input_sha256": sha,
                **result.model_dump(exclude={"key_quotes", "risk_score", "confidence"}),
                "risk_score": max(1, min(5, result.risk_score)),
                "confidence": max(1, min(5, result.confidence)),
                "key_quotes": json.dumps(result.key_quotes, ensure_ascii=False),
                "quotes_verified": int(quotes_verified(result.key_quotes, text)),
                "created_at": db.now(),
            }
            conn.execute(
                f"INSERT OR REPLACE INTO fsr_llm_assessment ({', '.join(row)}) "
                f"VALUES ({', '.join(':' + c for c in row)})",
                row,
            )
            stored += 1
    conn.commit()
    return stored, problems


# --- Coverage checks ---------------------------------------------------------------

def check(conn: sqlite3.Connection, raw_dir: Path = RAW_DIR) -> tuple[list[str], list[str]]:
    """(problems, notes): every listed issue stored, each has an overview, every listed box/article captured."""
    problems, notes = [], []
    issues_file = raw_dir / "issues.json"
    listed = {i["issue_date"] for i in json.loads(issues_file.read_text())} if issues_file.exists() else set()
    stored = {r[0] for r in conn.execute("SELECT issue_date FROM fsr_issues")}
    for missing in sorted(listed - stored):
        problems.append(f"{missing}: listed on the RBA site but not in the database")
    for issue_date, n_words, overviews in conn.execute(
        "SELECT i.issue_date, i.n_words, SUM(c.category = 'overview') FROM fsr_issues i "
        "JOIN fsr_chapters c USING (issue_id) GROUP BY i.issue_id ORDER BY i.issue_date"
    ):
        if not overviews:
            problems.append(f"{issue_date}: no overview chapter")
        if n_words < 4000:
            problems.append(f"{issue_date}: only {n_words} words outside special studies")
    index_file = raw_dir / "special_studies.json"
    if not index_file.exists():
        notes.append("special-study index not downloaded")
        return problems, notes
    index = json.loads(index_file.read_text())
    captured = {r[0]: r[1] for r in conn.execute("SELECT url, category FROM fsr_chapters")}
    for kind in ("boxes", "articles"):
        for item in index[kind]:
            if item["issue_date"] not in stored:
                continue
            category = captured.get(item["url"])
            if category is None:
                problems.append(f"{item['issue_date']}: {kind[:-1]} '{item['title']}' ({item['url']}) is not captured")
            elif category not in SPECIAL_STUDIES:
                problems.append(f"{item['issue_date']}: '{item['title']}' is stored as '{category}'")
    listed_urls = {i["url"] for kind in ("boxes", "articles") for i in index[kind]}
    for url, category in captured.items():
        if category in SPECIAL_STUDIES and url not in listed_urls:
            notes.append(f"captured {category} not on the RBA's lists: {url}")
    notes.append(f"{len(index['boxes'])} boxes/Focus Topics and {len(index['articles'])} articles listed by the RBA; "
                 f"{sum(c in SPECIAL_STUDIES for c in captured.values())} special studies captured")
    return problems, notes
