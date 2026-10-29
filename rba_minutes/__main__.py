"""Command line: python -m rba_minutes <command>

  scrape   download every minutes page into data/raw (cached; re-runs fetch only new ones)
  build    parse the cached pages into the SQLite database
  score    compute dictionary sentiment for every meeting
  llm      classify each meeting with Claude (needs ANTHROPIC_API_KEY)
  llm-export  write batch files for labelling inside a Claude Code session
  llm-import  load the JSON labels written for those batch files
  check    report data-quality problems in the database
  smp-scrape / smp-build / smp-score / smp-llm-export / smp-llm-import / smp-forecasts
           the same steps for the Statement on Monetary Policy (1997 onwards)
  export   write CSV files of the main tables to data/exports
  all      scrape, build, score and export
  status   print a summary of the database
"""

import argparse
import csv
import logging
import sys
from pathlib import Path

from . import config, db, scrape, sentiment_dict
from .clean import parse_minutes

log = logging.getLogger("rba_minutes")


def cmd_scrape(args) -> None:
    urls = scrape.scrape_all(args.first_year, args.last_year, refresh=args.refresh)
    log.info("%d minutes pages listed, cached in %s", len(urls), config.RAW_DIR)


def cmd_build(args) -> None:
    urls_file = config.RAW_DIR / "urls.txt"
    if not urls_file.exists():
        sys.exit("No scraped pages found. Run `python -m rba_minutes scrape` first.")
    urls = urls_file.read_text().split()
    conn = db.connect(args.db)
    failures = []
    for url in urls:
        raw = scrape.raw_path(url).read_bytes()
        try:
            minutes = parse_minutes(raw, url)
        except ValueError as e:
            failures.append((url, str(e)))
            continue
        db.upsert_minutes(conn, minutes, raw)
    db.rebuild_search_index(conn)
    conn.commit()
    log.info("Loaded %d meetings into %s", len(urls) - len(failures), args.db)
    for url, error in failures:
        log.error("Skipped %s: %s", url, error)


def cmd_score(args) -> None:
    conn = db.connect(args.db)
    n = sentiment_dict.score_database(conn)
    conn.commit()
    log.info("Dictionary sentiment computed for %d meetings", n)


def cmd_llm(args) -> None:
    from . import sentiment_llm

    conn = db.connect(args.db)
    n = sentiment_llm.score_database(conn, args.model, args.effort, args.scope, args.limit)
    log.info("Classified %d meetings with %s", n, args.model)


SESSION_MODEL = "claude-opus-5-5 (Claude Code session)"


def cmd_llm_export(args) -> None:
    from . import sentiment_llm

    conn = db.connect(args.db)
    paths = sentiment_llm.export_batches(conn, args.out, args.scope, args.batch_size, args.model)
    log.info("Wrote %d batch files to %s", len(paths), args.out)


def cmd_llm_import(args) -> None:
    from . import sentiment_llm

    conn = db.connect(args.db)
    files = sorted(Path(args.dir).glob("*.labels.json"))
    stored, problems = sentiment_llm.import_labels(conn, files, args.model, args.scope)
    log.info("Stored %d labels from %d files", stored, len(files))
    for problem in problems:
        log.error(problem)
    if problems:
        sys.exit(1)


def cmd_smp_scrape(args) -> None:
    from . import smp

    issues = smp.scrape_all(refresh=args.refresh)
    log.info("%d SMP issues listed, cached in %s", len(issues), smp.RAW_DIR)


def cmd_smp_build(args) -> None:
    from . import smp

    loaded, failures = smp.build(smp.connect(args.db))
    log.info("Loaded %d SMP issues", loaded)
    for failure in failures:
        log.error("Skipped %s", failure)


def cmd_smp_score(args) -> None:
    from . import smp

    log.info("Dictionary sentiment computed for %d SMP issues", smp.score_dictionary(smp.connect(args.db)))


def cmd_smp_llm_export(args) -> None:
    from . import smp

    paths = smp.export_batches(smp.connect(args.db), args.out, args.batch_size)
    log.info("Wrote %d SMP batch files to %s", len(paths), args.out)


def cmd_smp_llm_import(args) -> None:
    from . import smp

    files = sorted(Path(args.dir).glob("smp_batch_*.labels.json"))
    stored, problems = smp.import_labels(smp.connect(args.db), files)
    log.info("Stored %d SMP labels from %d files", stored, len(files))
    for problem in problems:
        log.error(problem)
    if problems:
        sys.exit(1)


def cmd_smp_forecasts(args) -> None:
    from . import smp

    for path in smp.download_forecasts():
        log.info("Saved %s", path)


def cmd_check(args) -> None:
    from . import quality

    problems, notes = quality.check(db.connect(args.db))
    for note in notes:
        print(f"note: {note}")
    for problem in problems:
        print(f"PROBLEM: {problem}")
    print(f"{len(problems)} problems, {len(notes)} notes")


EXPORTS = {
    "meeting_sentiment.csv": "SELECT * FROM v_meeting_sentiment",
    "meetings.csv": "SELECT meeting_id, meeting_date, year, url, title, decision, cash_rate_target, "
                    "decision_sentence, n_sections, n_paragraphs, n_words, full_text FROM meetings "
                    "ORDER BY meeting_date",
    "sections.csv": "SELECT s.section_id, m.meeting_date, s.position, s.heading, s.category, s.n_words, s.text "
                    "FROM sections s JOIN meetings m USING (meeting_id) ORDER BY m.meeting_date, s.position",
    "section_sentiment.csv": "SELECT m.meeting_date, s.heading, s.category, d.* FROM dictionary_sentiment d "
                             "JOIN meetings m USING (meeting_id) JOIN sections s USING (section_id) "
                             "WHERE d.scope = 'section' ORDER BY m.meeting_date, s.position",
    "llm_sentiment.csv": "SELECT * FROM llm_sentiment ORDER BY meeting_date, llm_sentiment_id",
}


def cmd_export(args) -> None:
    conn = db.connect(args.db)
    config.EXPORT_DIR.mkdir(parents=True, exist_ok=True)
    for name, sql in EXPORTS.items():
        cursor = conn.execute(sql)
        with open(config.EXPORT_DIR / name, "w", newline="", encoding="utf-8") as handle:
            writer = csv.writer(handle)
            writer.writerow([c[0] for c in cursor.description])
            writer.writerows(cursor)
    log.info("Wrote %d CSV files to %s", len(EXPORTS), config.EXPORT_DIR)


def cmd_all(args) -> None:
    for step in (cmd_scrape, cmd_build, cmd_score, cmd_export):
        step(args)


def cmd_status(args) -> None:
    conn = db.connect(args.db)
    q = lambda sql: conn.execute(sql).fetchone()[0]
    print(f"Database:          {args.db}")
    print(f"Meetings:          {q('SELECT COUNT(*) FROM meetings')}")
    print(f"First / last:      {q('SELECT MIN(meeting_date) FROM meetings')} / {q('SELECT MAX(meeting_date) FROM meetings')}")
    print(f"Sections:          {q('SELECT COUNT(*) FROM sections')}")
    print(f"Paragraphs:        {q('SELECT COUNT(*) FROM paragraphs')}")
    print(f"Words (discussion): {q('SELECT COALESCE(SUM(n_words), 0) FROM meetings')}")
    print(f"Decision found:    {q('SELECT COUNT(*) FROM meetings WHERE decision IS NOT NULL')}")
    print(f"Dictionary scored: {q('SELECT COUNT(DISTINCT meeting_id) FROM dictionary_sentiment')}")
    print(f"LLM classified:    {q('SELECT COUNT(DISTINCT meeting_date) FROM llm_sentiment')}")


def main(argv=None) -> None:
    parser = argparse.ArgumentParser(prog="python -m rba_minutes", description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--db", type=Path, default=config.DB_PATH, help="SQLite file (default: %(default)s)")
    parser.add_argument("-v", "--verbose", action="store_true")
    sub = parser.add_subparsers(dest="command", required=True)

    def add(name, func, help_text):
        p = sub.add_parser(name, help=help_text)
        p.set_defaults(func=func)
        return p

    for name, func, help_text in (("scrape", cmd_scrape, "download minutes pages"),
                                  ("all", cmd_all, "scrape, build, score and export")):
        p = add(name, func, help_text)
        p.add_argument("--first-year", type=int, default=config.FIRST_YEAR)
        p.add_argument("--last-year", type=int, default=None)
        p.add_argument("--refresh", action="store_true", help="re-download pages already cached")
    add("build", cmd_build, "parse cached pages into the database")
    add("score", cmd_score, "compute dictionary sentiment")
    p = add("llm", cmd_llm, "classify meetings with Claude")
    p.add_argument("--model", default="claude-opus-5-5")
    p.add_argument("--effort", default="medium", choices=["low", "medium", "high", "xhigh", "max"])
    p.add_argument("--scope", default="policy", choices=["policy", "full"],
                   help="send only the policy considerations and decision (default) or all discussion")
    p.add_argument("--limit", type=int, default=None, help="classify at most this many meetings")
    p = add("llm-export", cmd_llm_export, "write batch files for session labelling")
    p.add_argument("--out", type=Path, default=config.DATA_DIR / "llm_batches")
    p.add_argument("--scope", default="policy", choices=["policy", "full"])
    p.add_argument("--batch-size", type=int, default=10)
    p.add_argument("--model", default=SESSION_MODEL, help="leave out meetings this labeller has already done")
    p = add("llm-import", cmd_llm_import, "load session labels")
    p.add_argument("--dir", type=Path, default=config.DATA_DIR / "llm_batches")
    p.add_argument("--scope", default="policy", choices=["policy", "full"])
    p.add_argument("--model", default=SESSION_MODEL)
    add("check", cmd_check, "report data-quality problems")
    p = add("smp-scrape", cmd_smp_scrape, "download every SMP issue since 1997")
    p.add_argument("--refresh", action="store_true")
    add("smp-build", cmd_smp_build, "parse SMP issues into the database")
    add("smp-score", cmd_smp_score, "dictionary sentiment for SMP issues")
    p = add("smp-llm-export", cmd_smp_llm_export, "write SMP batch files for session labelling")
    p.add_argument("--out", type=Path, default=config.DATA_DIR / "llm_batches_smp")
    p.add_argument("--batch-size", type=int, default=10)
    p = add("smp-llm-import", cmd_smp_llm_import, "load SMP session labels")
    p.add_argument("--dir", type=Path, default=config.DATA_DIR / "llm_batches_smp")
    add("smp-forecasts", cmd_smp_forecasts, "download the RBA forecast archive")
    add("export", cmd_export, "write CSV exports")
    add("status", cmd_status, "summarise the database")

    args = parser.parse_args(argv)
    logging.basicConfig(level=logging.DEBUG if args.verbose else logging.INFO,
                        format="%(asctime)s %(levelname)s %(message)s")
    args.func(args)


if __name__ == "__main__":
    main()
