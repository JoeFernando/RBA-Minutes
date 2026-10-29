"""SQLite storage for cleaned minutes and sentiment scores."""

import datetime as dt
import hashlib
import sqlite3
from pathlib import Path

from . import config
from .clean import PARSER_VERSION, Minutes, extract_decision

SCHEMA = """
CREATE TABLE IF NOT EXISTS meetings (
    meeting_id          INTEGER PRIMARY KEY,
    meeting_date        TEXT NOT NULL UNIQUE,   -- ISO date the board met
    year                INTEGER NOT NULL,
    url                 TEXT NOT NULL UNIQUE,
    title               TEXT,
    location_date_line  TEXT,
    attendance_text     TEXT,                   -- header and members-present text
    decision            TEXT,                   -- raise / lower / hold (regex heuristic)
    cash_rate_target    REAL,                   -- per cent, from the decision sentence
    decision_sentence   TEXT,
    n_sections          INTEGER NOT NULL,
    n_paragraphs        INTEGER NOT NULL,
    n_words             INTEGER NOT NULL,
    full_text           TEXT NOT NULL,          -- cleaned discussion text with headings
    content_html        TEXT NOT NULL,          -- the page's content block, for re-parsing
    raw_sha256          TEXT NOT NULL,
    parser_version      TEXT NOT NULL,
    parsed_at           TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS sections (
    section_id      INTEGER PRIMARY KEY,
    meeting_id      INTEGER NOT NULL REFERENCES meetings(meeting_id) ON DELETE CASCADE,
    position        INTEGER NOT NULL,
    heading         TEXT NOT NULL,
    category        TEXT NOT NULL,              -- see clean.SECTION_CATEGORIES
    text            TEXT NOT NULL,
    n_words         INTEGER NOT NULL,
    UNIQUE (meeting_id, position)
);

CREATE TABLE IF NOT EXISTS paragraphs (
    paragraph_id    INTEGER PRIMARY KEY,
    meeting_id      INTEGER NOT NULL REFERENCES meetings(meeting_id) ON DELETE CASCADE,
    section_id      INTEGER NOT NULL REFERENCES sections(section_id) ON DELETE CASCADE,
    position        INTEGER NOT NULL,           -- order within the meeting
    kind            TEXT NOT NULL,              -- text / list_item / subheading
    text            TEXT NOT NULL,
    UNIQUE (meeting_id, position)
);

CREATE VIRTUAL TABLE IF NOT EXISTS paragraphs_fts USING fts5(
    text, content='paragraphs', content_rowid='paragraph_id', tokenize='porter unicode61'
);

-- Recomputed in full on every run of `score`.
CREATE TABLE IF NOT EXISTS dictionary_sentiment (
    meeting_id          INTEGER NOT NULL REFERENCES meetings(meeting_id) ON DELETE CASCADE,
    scope               TEXT NOT NULL,          -- meeting / policy / section
    section_id          INTEGER REFERENCES sections(section_id) ON DELETE CASCADE,
    n_tokens            INTEGER NOT NULL,
    lm_positive         INTEGER NOT NULL,
    lm_negative         INTEGER NOT NULL,
    lm_uncertainty      INTEGER NOT NULL,
    lm_constraining     INTEGER NOT NULL,
    lm_tone             REAL,                   -- (pos - neg) / (pos + neg)
    lm_net_per_1k       REAL,                   -- (pos - neg) per 1,000 tokens
    lm_uncertainty_per_1k REAL,
    hawkish             INTEGER NOT NULL,
    dovish              INTEGER NOT NULL,
    hawk_dove_index     REAL,                   -- (hawkish - dovish) / (hawkish + dovish)
    dictionary_version  TEXT NOT NULL,
    scored_at           TEXT NOT NULL
);
CREATE INDEX IF NOT EXISTS dictionary_sentiment_meeting ON dictionary_sentiment (meeting_id, scope);

-- Keyed by meeting_date (not meeting_id) and never cascaded, so paid LLM
-- results survive a rebuild of the text tables.
CREATE TABLE IF NOT EXISTS llm_sentiment (
    llm_sentiment_id    INTEGER PRIMARY KEY,
    meeting_date        TEXT NOT NULL,
    model               TEXT NOT NULL,          -- model requested
    served_model        TEXT,                   -- model that answered (differs after a fallback)
    prompt_version      TEXT NOT NULL,
    input_scope         TEXT NOT NULL,          -- policy / full
    input_sha256        TEXT NOT NULL,
    stance              TEXT,                   -- hawkish / neutral / dovish
    stance_score        INTEGER,                -- -2 (very dovish) .. +2 (very hawkish)
    confidence          INTEGER,                -- 1 (hedged) .. 5 (committed)
    policy_bias         TEXT,                   -- tightening / neutral / easing
    decision            TEXT,                   -- raise / lower / hold / other
    dominant_concern    TEXT,
    key_quotes          TEXT,                   -- JSON array
    quotes_verified     INTEGER,                -- 1 if every quote appears verbatim in the input
    reasoning           TEXT,
    stop_reason         TEXT,
    input_tokens        INTEGER,
    output_tokens       INTEGER,
    created_at          TEXT NOT NULL,
    UNIQUE (meeting_date, model, prompt_version, input_scope, input_sha256)
);

CREATE VIEW IF NOT EXISTS v_meeting_sentiment AS
SELECT
    m.meeting_date, m.year, m.decision, m.cash_rate_target, m.n_words,
    d.lm_tone, d.lm_net_per_1k, d.lm_uncertainty_per_1k, d.hawk_dove_index,
    p.lm_tone AS policy_lm_tone, p.hawk_dove_index AS policy_hawk_dove_index,
    l.stance AS llm_stance, l.stance_score AS llm_stance_score,
    l.confidence AS llm_confidence, l.policy_bias AS llm_policy_bias,
    l.dominant_concern AS llm_dominant_concern, l.model AS llm_model,
    m.url
FROM meetings m
LEFT JOIN dictionary_sentiment d ON d.meeting_id = m.meeting_id AND d.scope = 'meeting'
LEFT JOIN dictionary_sentiment p ON p.meeting_id = m.meeting_id AND p.scope = 'policy'
LEFT JOIN llm_sentiment l ON l.llm_sentiment_id = (
    SELECT MAX(llm_sentiment_id) FROM llm_sentiment x WHERE x.meeting_date = m.meeting_date
)
ORDER BY m.meeting_date;
"""


def now() -> str:
    return dt.datetime.now(dt.timezone.utc).isoformat(timespec="seconds")


def connect(path: Path = config.DB_PATH) -> sqlite3.Connection:
    path.parent.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(path)
    conn.row_factory = sqlite3.Row
    conn.execute("PRAGMA foreign_keys = ON")
    conn.executescript(SCHEMA)
    return conn


def word_count(text: str) -> int:
    return len(text.split())


def upsert_minutes(conn: sqlite3.Connection, minutes: Minutes, raw: bytes) -> int:
    """Insert or replace one meeting and its sections and paragraphs. Returns meeting_id."""
    decision, cash_rate, decision_sentence = extract_decision(minutes)
    full_text = minutes.full_text
    row = {
        "meeting_date": minutes.meeting_date,
        "year": int(minutes.meeting_date[:4]),
        "url": minutes.url,
        "title": minutes.title,
        "location_date_line": minutes.location_date_line,
        "attendance_text": minutes.attendance_text,
        "decision": decision,
        "cash_rate_target": cash_rate,
        "decision_sentence": decision_sentence,
        "n_sections": len(minutes.sections),
        "n_paragraphs": sum(len(s.paragraphs) for s in minutes.sections),
        "n_words": word_count(full_text),
        "full_text": full_text,
        "content_html": minutes.content_html,
        "raw_sha256": hashlib.sha256(raw).hexdigest(),
        "parser_version": PARSER_VERSION,
        "parsed_at": now(),
    }
    columns = ", ".join(row)
    placeholders = ", ".join(f":{c}" for c in row)
    updates = ", ".join(f"{c} = excluded.{c}" for c in row if c != "url")
    meeting_id = conn.execute(
        f"INSERT INTO meetings ({columns}) VALUES ({placeholders}) "
        f"ON CONFLICT(url) DO UPDATE SET {updates} RETURNING meeting_id",
        row,
    ).fetchone()[0]

    conn.execute("DELETE FROM paragraphs WHERE meeting_id = ?", (meeting_id,))
    conn.execute("DELETE FROM sections WHERE meeting_id = ?", (meeting_id,))
    paragraph_position = 0
    for position, section in enumerate(minutes.sections):
        section_id = conn.execute(
            "INSERT INTO sections (meeting_id, position, heading, category, text, n_words) "
            "VALUES (?, ?, ?, ?, ?, ?) RETURNING section_id",
            (meeting_id, position, section.heading, section.category, section.text,
             word_count(section.text)),
        ).fetchone()[0]
        for paragraph in section.paragraphs:
            conn.execute(
                "INSERT INTO paragraphs (meeting_id, section_id, position, kind, text) "
                "VALUES (?, ?, ?, ?, ?)",
                (meeting_id, section_id, paragraph_position, paragraph.kind, paragraph.text),
            )
            paragraph_position += 1
    return meeting_id


def rebuild_search_index(conn: sqlite3.Connection) -> None:
    conn.execute("INSERT INTO paragraphs_fts(paragraphs_fts) VALUES ('rebuild')")
