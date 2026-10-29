"""Dictionary-based sentiment: Loughran-McDonald word lists plus a hawkish/dovish index.

Both measures are transparent word counts, so every score can be traced back
to the words that produced it and re-computed exactly.
"""

import csv
import re
import sqlite3
from dataclasses import dataclass
from functools import lru_cache
from importlib import metadata, resources

from . import db
from .clean import split_sentences

TOKEN = re.compile(r"[a-z]+(?:'[a-z]+)?")

# --- Hawkish / dovish ----------------------------------------------------------
# Each economic term is paired with a nearby direction word (within WINDOW
# tokens in the same sentence). "inflation ... rising" is hawkish; "inflation
# ... easing" is dovish. Terms where a rise is dovish (unemployment, spare
# capacity) carry a sign of -1. This follows the approach of Apel, Blix
# Grimaldi & Hull (2022), "How much information do monetary policy committees
# disclose? Evidence from the FOMC's minutes and transcripts", Journal of
# Money, Credit and Banking, with word lists adapted to RBA wording. It is not
# an exact replication of their lists.

WINDOW = 4

TERMS = {
    "inflation": 1, "inflationary": 1, "price": 1, "prices": 1, "cpi": 1,
    "wage": 1, "wages": 1, "cost": 1, "costs": 1, "rents": 1,
    "growth": 1, "demand": 1, "activity": 1, "economy": 1, "spending": 1,
    "consumption": 1, "investment": 1, "employment": 1, "credit": 1,
    "lending": 1, "borrowing": 1, "expectations": 1,
    "unemployment": -1, "underemployment": -1, "underutilisation": -1,
    "spare": -1, "slack": -1,
}

UP_WORDS = {
    "rise", "rises", "rising", "rose", "risen", "higher", "high", "elevated",
    "strong", "stronger", "strongly", "robust", "solid", "firm", "firmer",
    "buoyant", "upward", "upside", "tight", "tighter", "persistent",
    "sticky", "above", "expanded", "expanding", "expansion", "pickup",
}
UP_STEMS = ("increas", "accelerat", "strengthen", "overheat", "elevat")

DOWN_WORDS = {
    "fall", "falls", "falling", "fell", "fallen", "lower", "low", "weak",
    "weaker", "weakly", "soft", "softer", "subdued", "slow", "slower",
    "slowing", "slowed", "slowdown", "ease", "eased", "eases", "easing",
    "downward", "downside", "below", "contracted", "contracting",
    "contraction", "sluggish", "modest", "contained", "benign",
}
DOWN_STEMS = ("declin", "decreas", "weaken", "soften", "moderat")

NEGATORS = {"not", "no", "nor", "never", "unlikely", "less"}

# Direct statements about the stance of policy, counted alongside the pairs.
HAWKISH_POLICY = re.compile(
    r"\b(?:increas|rais|lift|tighten)\w*\s+(?:in\s+|of\s+)?(?:the\s+)?"
    r"(?:cash\s+rate|interest\s+rates?|monetary\s+policy)"
    r"|\bfurther\s+tightening\b|\btightening\s+(?:of\s+)?monetary\s+policy\b",
    re.IGNORECASE,
)
DOVISH_POLICY = re.compile(
    r"\b(?:lower|reduc|cut|eas|loosen)\w*\s+(?:in\s+|of\s+)?(?:the\s+)?"
    r"(?:cash\s+rate|interest\s+rates?|monetary\s+policy)"
    r"|\bfurther\s+easing\b|\baccommodative\b|\bstimul(?:us|atory)\b",
    re.IGNORECASE,
)


def direction(token: str) -> int:
    if token in UP_WORDS or token.startswith(UP_STEMS):
        return 1
    if token in DOWN_WORDS or token.startswith(DOWN_STEMS):
        return -1
    return 0


def hawk_dove_counts(text: str) -> tuple[int, int]:
    """Count hawkish and dovish signals in a text. Returns (hawkish, dovish)."""
    hawkish = dovish = 0
    for sentence in split_sentences(text):
        tokens = TOKEN.findall(sentence.lower())
        for i, token in enumerate(tokens):
            sign = TERMS.get(token)
            if sign is None:
                continue
            # The nearest direction word wins; on a tie, the later one.
            best = None
            for distance in range(1, WINDOW + 1):
                for j in (i + distance, i - distance):
                    if 0 <= j < len(tokens) and direction(tokens[j]):
                        best = j
                        break
                if best is not None:
                    break
            if best is None:
                continue
            score = sign * direction(tokens[best])
            if any(t in NEGATORS for t in tokens[max(0, best - 2):best]):
                score = -score
            if score > 0:
                hawkish += 1
            else:
                dovish += 1
        hawkish += len(HAWKISH_POLICY.findall(sentence))
        dovish += len(DOVISH_POLICY.findall(sentence))
    return hawkish, dovish


# --- Loughran-McDonald ---------------------------------------------------------

@dataclass(frozen=True)
class LMLists:
    positive: frozenset
    negative: frozenset
    uncertainty: frozenset
    constraining: frozenset
    version: str


@lru_cache(maxsize=1)
def load_lm() -> LMLists:
    """Load the Loughran-McDonald master dictionary bundled with the pysentiment2 package."""
    path = resources.files("pysentiment2") / "static" / "LM.csv"
    lists = {"Positive": set(), "Negative": set(), "Uncertainty": set(), "Constraining": set()}
    with path.open(newline="", encoding="utf-8") as handle:
        for row in csv.DictReader(handle):
            for name, words in lists.items():
                if row[name] not in ("", "0"):
                    words.add(row["Word"].lower())
    version = f"Loughran-McDonald (pysentiment2 {metadata.version('pysentiment2')})"
    return LMLists(*(frozenset(lists[k]) for k in lists), version=version)


def score_text(text: str) -> dict:
    lm = load_lm()
    tokens = TOKEN.findall(text.lower())
    n = len(tokens)
    pos = sum(t in lm.positive for t in tokens)
    neg = sum(t in lm.negative for t in tokens)
    unc = sum(t in lm.uncertainty for t in tokens)
    con = sum(t in lm.constraining for t in tokens)
    hawkish, dovish = hawk_dove_counts(text)
    return {
        "n_tokens": n,
        "lm_positive": pos,
        "lm_negative": neg,
        "lm_uncertainty": unc,
        "lm_constraining": con,
        "lm_tone": (pos - neg) / (pos + neg) if pos + neg else None,
        "lm_net_per_1k": 1000 * (pos - neg) / n if n else None,
        "lm_uncertainty_per_1k": 1000 * unc / n if n else None,
        "hawkish": hawkish,
        "dovish": dovish,
        "hawk_dove_index": (hawkish - dovish) / (hawkish + dovish) if hawkish + dovish else None,
    }


def dictionary_version() -> str:
    return f"{load_lm().version}; hawk-dove v1 (window {WINDOW})"


def score_database(conn: sqlite3.Connection) -> int:
    """Recompute dictionary sentiment for every meeting. Returns the number of meetings scored."""
    from .clean import NON_DISCUSSION

    version = dictionary_version()
    scored_at = db.now()
    conn.execute("DELETE FROM dictionary_sentiment")
    meetings = conn.execute("SELECT meeting_id FROM meetings ORDER BY meeting_date").fetchall()
    placeholders = ", ".join("?" for _ in NON_DISCUSSION)
    for (meeting_id,) in meetings:
        sections = conn.execute(
            f"SELECT section_id, category, text FROM sections "
            f"WHERE meeting_id = ? AND category NOT IN ({placeholders}) ORDER BY position",
            (meeting_id, *NON_DISCUSSION),
        ).fetchall()
        scopes = [("meeting", None, "\n\n".join(s["text"] for s in sections))]
        policy = [s["text"] for s in sections if s["category"] in ("policy_considerations", "decision")]
        if policy:
            scopes.append(("policy", None, "\n\n".join(policy)))
        scopes += [("section", s["section_id"], s["text"]) for s in sections]
        for scope, section_id, text in scopes:
            scores = score_text(text)
            row = {"meeting_id": meeting_id, "scope": scope, "section_id": section_id,
                   **scores, "dictionary_version": version, "scored_at": scored_at}
            conn.execute(
                f"INSERT INTO dictionary_sentiment ({', '.join(row)}) "
                f"VALUES ({', '.join(':' + c for c in row)})",
                row,
            )
    return len(meetings)
