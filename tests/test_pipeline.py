"""Offline tests. The HTML fixtures are synthetic pages that mimic the RBA layout."""

import sqlite3
from pathlib import Path

import pytest

from rba_minutes import db, sentiment_dict
from rba_minutes.clean import categorise, date_from_url, extract_decision, normalise_text, parse_minutes
from rba_minutes.scrape import find_minutes_links, raw_path
from rba_minutes.sentiment_llm import meeting_input, quotes_verified

FIXTURES = Path(__file__).parent / "fixtures"
URL = "https://www.rba.gov.au/monetary-policy/rba-board-minutes/2006/03102006.html"


@pytest.fixture
def minutes():
    return parse_minutes((FIXTURES / "minutes_synthetic.html").read_bytes(), URL)


def test_find_minutes_links_dedupes_and_filters():
    html = (FIXTURES / "index_synthetic.html").read_text()
    links = find_minutes_links(html, "https://www.rba.gov.au/monetary-policy/rba-board-minutes/2006/")
    assert links == [
        "https://www.rba.gov.au/monetary-policy/rba-board-minutes/2006/05122006.html",
        "https://www.rba.gov.au/monetary-policy/rba-board-minutes/2006/07112006.html",
        URL,
        "https://www.rba.gov.au/monetary-policy/rba-board-minutes/2025/2025-02-18.html",
    ]


def test_raw_path():
    assert raw_path(URL, Path("raw")) == Path("raw/2006/03102006.html")


def test_date_from_url_both_formats():
    assert date_from_url(URL).isoformat() == "2006-10-03"
    assert date_from_url(".../2024/2024-02-06.html").isoformat() == "2024-02-06"
    assert date_from_url(".../2024/index.html") is None


def test_normalise_text():
    assert normalise_text("3 October  2006 ,  rose­ further ( a )") == "3 October 2006, rose further (a)"


@pytest.mark.parametrize("heading, category", [
    ("International Financial Markets", "financial_markets"),
    ("Financial Markets and the Bank's Operations", "financial_markets"),
    ("International Economic Conditions", "international"),
    ("Domestic Economic Developments and Outlook", "outlook"),
    ("Outlook for the Australian Economy", "outlook"),
    ("Prices and Wages", "domestic"),
    ("Considerations for monetary policy", "policy_considerations"),
    ("The Decision", "decision"),
    ("Members Present", "attendance"),
    ("Something New", "other"),
])
def test_categorise(heading, category):
    assert categorise(heading) == category


def test_parse_minutes_structure(minutes):
    assert minutes.meeting_date == "2006-10-03"
    assert minutes.title.startswith("Minutes of the Monetary Policy Meeting")
    assert minutes.location_date_line == "Sydney – 3 October 2006"
    assert [s.category for s in minutes.sections] == [
        "header", "attendance", "international", "domestic", "policy_considerations", "decision", "footnotes",
    ]
    domestic = minutes.sections[3]
    assert [p.kind for p in domestic.paragraphs] == ["text", "list_item", "list_item", "subheading", "text"]
    assert domestic.paragraphs[0].text.endswith("above the target.")  # footnote marker removed
    assert minutes.sections[2].paragraphs[0].text == "Global growth remained strong and commodity prices rose further."


def test_parse_minutes_drops_navigation_and_footer(minutes):
    everything = " ".join(p.text for s in minutes.sections for p in s.paragraphs)
    for junk in ("Related Information", "Media Release", "Footer text", "Home"):
        assert junk not in everything
    assert "footnote text is synthetic" in minutes.sections[-1].text
    assert "footnote" not in minutes.full_text  # footnotes are not part of the discussion text
    assert "Members Present" not in minutes.full_text


def test_extract_decision(minutes):
    decision, rate, sentence = extract_decision(minutes)
    assert (decision, rate) == ("raise", 6.25)
    assert sentence.startswith("The Board decided to increase")


@pytest.mark.parametrize("sentence, expected", [
    ("The Board decided to leave the cash rate unchanged at 4.75 per cent.", ("hold", 4.75)),
    ("The Board decided to lower the cash rate target by 15 basis points to 0.1 per cent.", ("lower", 0.1)),
    ("The Board decided that the cash rate target would remain unchanged at 4.35 per cent.", ("hold", 4.35)),
    ("The Board decided to reduce the cash rate by 25 basis points to 4.10 per cent.", ("lower", 4.10)),
])
def test_extract_decision_variants(minutes, sentence, expected):
    minutes.sections[5].paragraphs[0].text = sentence
    assert extract_decision(minutes)[:2] == expected


@pytest.mark.parametrize("text, expected", [
    ("Inflation was rising.", (1, 0)),
    ("Inflation had eased.", (0, 1)),
    ("Unemployment was rising.", (0, 1)),  # a rise in unemployment is dovish
    ("Unemployment had declined.", (1, 0)),
    ("Business investment was not weak.", (1, 0)),  # negation flips
    ("Members discussed the case for increasing the cash rate.", (1, 0)),
    ("Members discussed the case for lowering the cash rate.", (0, 1)),
    ("The meeting was held in Sydney.", (0, 0)),
])
def test_hawk_dove_counts(text, expected):
    assert sentiment_dict.hawk_dove_counts(text) == expected


def test_lm_dictionary_loads():
    lm = sentiment_dict.load_lm()
    assert "loss" in lm.negative and "strong" in lm.positive and "uncertainty" in lm.uncertainty
    scores = sentiment_dict.score_text("Strong gains. Uncertainty and losses.")
    assert scores["lm_positive"] >= 1 and scores["lm_negative"] >= 1 and scores["lm_uncertainty"] >= 1


def test_quotes_verified():
    source = "Members agreed that there was a case for increasing the cash rate, given that inflation was rising."
    assert quotes_verified(["there was a case for increasing the cash rate"], source)
    assert quotes_verified(["“Members agreed that there was a case”"[1:-1]], source)
    assert not quotes_verified(["members agreed to cut rates"], source)


def test_database_round_trip(tmp_path, minutes):
    path = tmp_path / "test.sqlite"
    conn = db.connect(path)
    raw = (FIXTURES / "minutes_synthetic.html").read_bytes()
    meeting_id = db.upsert_minutes(conn, minutes, raw)
    assert db.upsert_minutes(conn, minutes, raw) == meeting_id  # re-load keeps the id, no duplicates
    db.rebuild_search_index(conn)
    assert sentiment_dict.score_database(conn) == 1
    conn.commit()

    conn = sqlite3.connect(path)
    assert conn.execute("SELECT COUNT(*) FROM meetings").fetchone()[0] == 1
    assert conn.execute("SELECT decision, cash_rate_target FROM meetings").fetchone() == ("raise", 6.25)
    assert conn.execute("SELECT COUNT(*) FROM sections").fetchone()[0] == 7
    hits = conn.execute("SELECT COUNT(*) FROM paragraphs_fts WHERE paragraphs_fts MATCH 'commodity'").fetchone()[0]
    assert hits == 1
    scopes = dict(conn.execute("SELECT scope, COUNT(*) FROM dictionary_sentiment GROUP BY scope").fetchall())
    assert scopes == {"meeting": 1, "policy": 1, "section": 4}
    hawk, dove = conn.execute(
        "SELECT hawkish, dovish FROM dictionary_sentiment WHERE scope = 'meeting'").fetchone()
    assert hawk > dove
    row = conn.execute("SELECT meeting_date, decision, hawk_dove_index FROM v_meeting_sentiment").fetchone()
    assert row[0] == "2006-10-03" and row[1] == "raise" and row[2] > 0

    conn = db.connect(path)
    text = meeting_input(conn, meeting_id, "policy")
    assert text.startswith("## Considerations for Monetary Policy") and "The Decision" in text
    assert "Global growth" not in text


def test_session_label_round_trip(tmp_path, minutes):
    import json

    from rba_minutes import sentiment_llm

    conn = db.connect(tmp_path / "test.sqlite")
    db.upsert_minutes(conn, minutes, b"raw")
    batch_dir = tmp_path / "batches"
    paths = sentiment_llm.export_batches(conn, batch_dir, model="session")
    assert [p.name for p in paths] == ["batch_001.md"]
    body = paths[0].read_text()
    assert "Output file: batch_001.labels.json" in body and "=== MEETING 2006-10-03" in body
    sha = body.split("| input_sha256 ")[1].split(" ")[0]

    label = {"meeting_date": "2006-10-03", "input_sha256": sha, "stance": "hawkish", "stance_score": 2,
             "confidence": 5, "policy_bias": "tightening", "decision": "raise", "dominant_concern": "inflation",
             "key_quotes": ["The Board decided to increase the cash rate by 25 basis points to 6.25 per cent"],
             "reasoning": "Test."}
    bad = dict(label, meeting_date="1999-01-01")
    (batch_dir / "batch_001.labels.json").write_text(json.dumps([label, bad]))
    stored, problems = sentiment_llm.import_labels(conn, [batch_dir / "batch_001.labels.json"], "session")
    assert stored == 1 and len(problems) == 1
    row = conn.execute("SELECT stance, quotes_verified, model FROM llm_sentiment").fetchone()
    assert tuple(row) == ("hawkish", 1, "session")
    assert sentiment_llm.export_batches(conn, batch_dir / "again", model="session") == []
