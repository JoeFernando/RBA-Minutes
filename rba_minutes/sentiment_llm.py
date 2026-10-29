"""Hawkish/dovish classification of each meeting with Claude.

Two ways to produce the labels, both stored in the llm_sentiment table:

- score_database() calls the Anthropic API (needs ANTHROPIC_API_KEY). A
  meeting is skipped when a result already exists for the same model, prompt
  version and input text, so re-runs only pay for new meetings.
- export_batches() / import_labels() hand the same inputs and instructions to
  Claude working in a Claude Code session, then load its JSON answers back,
  validated against the same schema.
"""

import hashlib
import json
import logging
import re
import sqlite3
from typing import Literal

import anthropic
from pydantic import BaseModel, Field

from . import db

log = logging.getLogger(__name__)

DEFAULT_MODEL = "claude-opus-5-5"
DEFAULT_EFFORT = "medium"
PROMPT_VERSION = "v1"

SYSTEM_PROMPT = """You are an expert on Australian monetary policy. You will read the minutes of one \
Reserve Bank of Australia monetary policy meeting and assess the stance the Board communicated.

Judge the tone of the Board's own deliberations and decision, as written in this document only. \
Do not use knowledge of what the RBA did at later meetings.

Definitions:
- stance: "hawkish" means leaning towards tighter policy or concerned that inflation is too high; \
"dovish" means leaning towards easier policy or concerned about weak activity or rising unemployment; \
"neutral" means balanced or with no clear lean.
- stance_score: -2 strongly dovish, -1 mildly dovish, 0 neutral, +1 mildly hawkish, +2 strongly hawkish.
- confidence: how committed the Board's language is, from 1 (heavily hedged) to 5 (unambiguous).
- policy_bias: the direction the Board signals for future policy: "tightening", "easing" or "neutral".
- decision: what the Board decided about the cash rate at this meeting: "raise", "lower", "hold", \
or "other" if the cash rate was not the main decision.
- dominant_concern: the Board's main concern: "inflation", "growth_employment", "financial_stability", \
"balanced" or "other".
- key_quotes: up to three short sentences copied exactly, word for word, from the minutes that best \
support your assessment.
- reasoning: two to four sentences explaining the assessment."""


class MeetingStance(BaseModel):
    stance: Literal["hawkish", "neutral", "dovish"]
    stance_score: int = Field(description="-2 strongly dovish to +2 strongly hawkish")
    confidence: int = Field(description="1 heavily hedged to 5 unambiguous")
    policy_bias: Literal["tightening", "neutral", "easing"]
    decision: Literal["raise", "lower", "hold", "other"]
    dominant_concern: Literal["inflation", "growth_employment", "financial_stability", "balanced", "other"]
    key_quotes: list[str]
    reasoning: str


def meeting_input(conn: sqlite3.Connection, meeting_id: int, scope: str) -> str:
    """Text sent to the model: the policy sections (default) or all discussion sections."""
    from .clean import NON_DISCUSSION

    categories = ("policy_considerations", "decision") if scope == "policy" else None
    rows = conn.execute(
        "SELECT heading, category, text FROM sections WHERE meeting_id = ? ORDER BY position",
        (meeting_id,),
    ).fetchall()
    rows = [r for r in rows if r["category"] not in NON_DISCUSSION]
    chosen = [r for r in rows if categories is None or r["category"] in categories]
    # Some meetings may lack a recognisable policy heading; fall back to the whole text.
    chosen = chosen or rows
    return "\n\n".join(f"## {r['heading']}\n\n{r['text']}" for r in chosen)


def _squash(text: str) -> str:
    return re.sub(r"\s+", " ", text.replace("’", "'").replace("‘", "'")).strip().lower()


def quotes_verified(quotes: list[str], source: str) -> bool:
    haystack = _squash(source)
    return all(_squash(q).strip("\"' .") in haystack for q in quotes)


def insert_result(conn: sqlite3.Connection, meeting_date: str, model: str, served_model: str | None,
                  scope: str, input_sha: str, result: MeetingStance, text: str, stop_reason: str | None = None,
                  input_tokens: int | None = None, output_tokens: int | None = None,
                  prompt_version: str = PROMPT_VERSION) -> None:
    row = {
        "meeting_date": meeting_date,
        "model": model,
        "served_model": served_model,
        "prompt_version": prompt_version,
        "input_scope": scope,
        "input_sha256": input_sha,
        "stance": result.stance,
        "stance_score": max(-2, min(2, result.stance_score)),
        "confidence": max(1, min(5, result.confidence)),
        "policy_bias": result.policy_bias,
        "decision": result.decision,
        "dominant_concern": result.dominant_concern,
        "key_quotes": json.dumps(result.key_quotes, ensure_ascii=False),
        "quotes_verified": int(quotes_verified(result.key_quotes, text)),
        "reasoning": result.reasoning,
        "stop_reason": stop_reason,
        "input_tokens": input_tokens,
        "output_tokens": output_tokens,
        "created_at": db.now(),
    }
    conn.execute(
        f"INSERT OR REPLACE INTO llm_sentiment ({', '.join(row)}) VALUES ({', '.join(':' + c for c in row)})",
        row,
    )


def classify(client: anthropic.Anthropic, meeting_date: str, text: str,
             model: str, effort: str):
    return client.messages.parse(
        model=model,
        max_tokens=16000,
        system=SYSTEM_PROMPT,
        messages=[{
            "role": "user",
            "content": f"Minutes of the RBA monetary policy meeting held on {meeting_date}:\n\n{text}",
        }],
        output_format=MeetingStance,
        output_config={"effort": effort},
        # If a safety classifier declines the request, retry server-side on
        # Anthropic's recommended fallback model instead of failing.
        extra_headers={"anthropic-beta": "server-side-fallback-2026-07-01"},
        extra_body={"fallbacks": "default"},
    )


def score_database(conn: sqlite3.Connection, model: str = DEFAULT_MODEL, effort: str = DEFAULT_EFFORT,
                   scope: str = "policy", limit: int | None = None) -> int:
    """Classify every meeting without a stored result. Returns the number classified."""
    client = anthropic.Anthropic(max_retries=5)
    meetings = conn.execute("SELECT meeting_id, meeting_date FROM meetings ORDER BY meeting_date").fetchall()
    done = 0
    for meeting in meetings:
        if limit is not None and done >= limit:
            break
        text = meeting_input(conn, meeting["meeting_id"], scope)
        input_sha = hashlib.sha256(text.encode()).hexdigest()
        exists = conn.execute(
            "SELECT 1 FROM llm_sentiment WHERE meeting_date = ? AND model = ? AND prompt_version = ? "
            "AND input_scope = ? AND input_sha256 = ?",
            (meeting["meeting_date"], model, PROMPT_VERSION, scope, input_sha),
        ).fetchone()
        if exists:
            continue
        try:
            response = classify(client, meeting["meeting_date"], text, model, effort)
        except anthropic.BadRequestError as e:
            log.error("%s: request rejected: %s", meeting["meeting_date"], e.message)
            continue
        except (anthropic.RateLimitError, anthropic.APIConnectionError, anthropic.InternalServerError) as e:
            log.error("%s: giving up after retries (%s); re-run to resume", meeting["meeting_date"], e)
            break

        result = response.parsed_output if response.stop_reason == "end_turn" else None
        if result is None:
            # Not stored, so the next run tries this meeting again.
            log.warning("%s: no classification (stop_reason=%s)", meeting["meeting_date"], response.stop_reason)
            continue
        insert_result(conn, meeting["meeting_date"], model, response.model, scope, input_sha, result, text,
                      response.stop_reason, response.usage.input_tokens, response.usage.output_tokens)
        conn.commit()
        done += 1
        log.info("%s: %s (%s)", meeting["meeting_date"], result.stance, result.stance_score)
    return done


# --- Labelling inside a Claude Code session ------------------------------------

SESSION_INSTRUCTIONS = """For each meeting in this file, read its text and assess it using the \
definitions below. Write a JSON array to the output file named in the header, with one object per \
meeting, in the same order, each with exactly these keys: meeting_date, input_sha256 (both copied \
from the meeting header), stance, stance_score, confidence, policy_bias, decision, dominant_concern, \
key_quotes, reasoning. Assess each meeting on its own text only.

"""


def export_batches(conn: sqlite3.Connection, out_dir, scope: str = "policy", batch_size: int = 10,
                   model: str | None = None) -> list:
    """Write meetings that still need a label to Markdown batch files. Returns the paths.

    If model is given, meetings already labelled by that model for the current
    text are left out.
    """
    from pathlib import Path

    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    pending = []
    for meeting in conn.execute("SELECT meeting_id, meeting_date FROM meetings ORDER BY meeting_date"):
        text = meeting_input(conn, meeting["meeting_id"], scope)
        input_sha = hashlib.sha256(text.encode()).hexdigest()
        if model and conn.execute(
            "SELECT 1 FROM llm_sentiment WHERE meeting_date = ? AND model = ? AND prompt_version = ? "
            "AND input_scope = ? AND input_sha256 = ?",
            (meeting["meeting_date"], model, PROMPT_VERSION, scope, input_sha),
        ).fetchone():
            continue
        pending.append((meeting["meeting_date"], input_sha, text))

    paths = []
    for n, start in enumerate(range(0, len(pending), batch_size), 1):
        batch = pending[start:start + batch_size]
        path = out_dir / f"batch_{n:03d}.md"
        label_path = out_dir / f"batch_{n:03d}.labels.json"
        parts = [
            f"# Batch {n:03d}: {len(batch)} meetings ({batch[0][0]} to {batch[-1][0]})\n",
            f"Output file: {label_path.name} (same folder)\n",
            f"Input scope: {scope}; prompt version: {PROMPT_VERSION}\n",
            "## Instructions\n",
            SESSION_INSTRUCTIONS + SYSTEM_PROMPT + "\n",
        ]
        for meeting_date, input_sha, text in batch:
            parts.append(f"\n=== MEETING {meeting_date} | input_sha256 {input_sha} ===\n\n{text}\n")
        path.write_text("\n".join(parts), encoding="utf-8")
        paths.append(path)
    return paths


def import_labels(conn: sqlite3.Connection, label_files, model: str, scope: str = "policy") -> tuple[int, list]:
    """Validate and store labels written by a Claude Code session. Returns (stored, problems)."""
    from pathlib import Path

    current = {}
    for meeting in conn.execute("SELECT meeting_id, meeting_date FROM meetings"):
        text = meeting_input(conn, meeting["meeting_id"], scope)
        current[meeting["meeting_date"]] = (hashlib.sha256(text.encode()).hexdigest(), text)

    stored, problems = 0, []
    for label_file in label_files:
        try:
            items = json.loads(Path(label_file).read_text(encoding="utf-8"))
        except json.JSONDecodeError as e:
            problems.append(f"{label_file}: invalid JSON ({e})")
            continue
        for item in items:
            meeting_date = item.get("meeting_date")
            if meeting_date not in current:
                problems.append(f"{label_file}: unknown meeting {meeting_date}")
                continue
            input_sha, text = current[meeting_date]
            if item.get("input_sha256") != input_sha:
                problems.append(f"{label_file}: {meeting_date} was labelled on different text; re-export it")
                continue
            try:
                result = MeetingStance.model_validate(item)
            except ValueError as e:
                problems.append(f"{label_file}: {meeting_date} failed validation ({e})")
                continue
            insert_result(conn, meeting_date, model, model, scope, input_sha, result, text)
            stored += 1
    conn.commit()
    return stored, problems
