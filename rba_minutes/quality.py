"""Data-quality checks on the built database."""

import re
import sqlite3


def check(conn: sqlite3.Connection) -> list[str]:
    """Return human-readable problems found in the database (empty if none)."""
    problems = []
    meetings = conn.execute(
        "SELECT meeting_id, meeting_date, decision, cash_rate_target, n_words, full_text "
        "FROM meetings ORDER BY meeting_date"
    ).fetchall()

    # Each decision should move the cash rate the way it says it does.
    previous = None
    for m in meetings:
        if m["decision"] is None or m["cash_rate_target"] is None:
            problems.append(f"{m['meeting_date']}: decision or cash rate not found")
            previous = None
            continue
        if previous is not None:
            change = round(m["cash_rate_target"] - previous, 4)
            expected = {"raise": change > 0, "lower": change < 0, "hold": change == 0}[m["decision"]]
            if not expected:
                problems.append(
                    f"{m['meeting_date']}: decision '{m['decision']}' but cash rate went "
                    f"{previous} -> {m['cash_rate_target']}"
                )
        previous = m["cash_rate_target"]

    for m in meetings:
        categories = {r[0] for r in conn.execute(
            "SELECT category FROM sections WHERE meeting_id = ?", (m["meeting_id"],))}
        for needed in ("policy_considerations", "decision"):
            if needed not in categories:
                problems.append(f"{m['meeting_date']}: no '{needed}' section")
        if m["n_words"] < 1000:
            problems.append(f"{m['meeting_date']}: only {m['n_words']} words of discussion")
        # Footnote markers glued to words ("rate1") would mean markup slipped through.
        stray = re.findall(r"\b[a-z]{3,}\d\b", m["full_text"])
        if stray:
            problems.append(f"{m['meeting_date']}: possible footnote markers {stray[:5]}")
        for junk in ("Related Information", "Print this page", "More on the"):
            if junk in m["full_text"]:
                problems.append(f"{m['meeting_date']}: navigation text '{junk}' in discussion")

    other = conn.execute(
        "SELECT heading, COUNT(*) FROM sections WHERE category = 'other' GROUP BY heading ORDER BY 2 DESC"
    ).fetchall()
    for heading, count in other:
        problems.append(f"uncategorised heading '{heading}' ({count} meetings)")
    return problems
