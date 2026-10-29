# RBA Minutes and Statements on Monetary Policy

Downloads, cleans, stores and scores two Reserve Bank of Australia publications, plus the RBA's own forecast history:

| Source | Coverage | What you get |
|---|---|---|
| Monetary policy meeting minutes | Every meeting since the first published minutes (3 October 2006) | Cleaned text by section and paragraph, the cash rate decision, dictionary sentiment, Claude stance labels |
| Statement on Monetary Policy (SMP) | Every issue since February 1997 (published in the RBA Bulletin until 2005) | Cleaned text by chapter and paragraph, every box and "In Depth" special study, **every table as clean data**, dictionary sentiment, Claude stance labels |
| RBA forecasts | Central forecasts since 1990 with outcomes and errors; detailed SMP forecast tables since November 2018 | Tidy forecast tables, cross-checked against the SMP tables |

Everything is in two SQLite files (both readable from R and Python):

| File | Contents |
|---|---|
| `data/rba_minutes.sqlite` | Minutes |
| `data/rba_smp.sqlite` | SMP text, SMP tables, RBA forecasts |

## Quick start

```bash
pip install -r requirements.txt
```

Minutes (scrape → build → score → export):

```bash
python -m rba_minutes all
```

Statements on Monetary Policy:

```bash
python -m rba_minutes smp-scrape
```

```bash
python -m rba_minutes smp-build
```

```bash
python -m rba_minutes smp-score
```

```bash
python -m rba_minutes smp-boxes
```

```bash
python -m rba_minutes smp-check
```

```bash
python -m rba_minutes smp-forecasts
```

```bash
python -m rba_minutes forecasts-load
```

```bash
python -m rba_minutes smp-tables
```

Downloads are cached (`data/raw/`, `data/raw_smp/`), pause one second between requests, and re-runs only fetch new pages. Run `smp-tables` after `forecasts-load` so the forecast cross-check can run.

### Commands

| Command | What it does |
|---|---|
| `scrape` / `build` / `score` | Minutes: download every page, parse into the database, compute dictionary sentiment |
| `check` | Minutes data-quality checks: every cash rate decision must chain from the previous meeting, every meeting needs policy and decision sections, no stray footnote markers or navigation text |
| `llm` | Claude stance labels through the Anthropic API (needs `ANTHROPIC_API_KEY`; `--model`, `--effort`, `--scope`, `--limit`) |
| `llm-export` / `llm-import` | The same labelling done by Claude inside a Claude Code session: export batch files with the inputs and instructions, then validate and load the JSON answers |
| `smp-scrape` | Download every SMP issue: the Bulletin page (1997–2005) or every chapter and box page (2006 onwards) |
| `smp-build` / `smp-score` | Parse SMP issues into chapters and paragraphs; compute dictionary sentiment per issue, overview, outlook and chapter |
| `smp-boxes` / `smp-check` | Download the RBA's yearly lists of SMP boxes and check every listed box is captured; check every issue has an overview |
| `smp-llm-export` / `smp-llm-import` | Claude stance labels for each SMP Overview, done in a Claude Code session |
| `smp-tables` | Download the "List of tables" pages (2006–2017) and graph-data workbooks (2024+), extract every table cell by cell, write CSVs, and cross-check the forecast tables against the RBA forecast archive |
| `smp-forecasts` / `forecasts-load` | Download the RBA forecast archive and historical forecast files, and load them into tables |
| `export` / `status` | Minutes CSV exports and a summary |

## Cleaning

| Step | Detail |
|---|---|
| Content only | Keeps the page's `#content` block; drops navigation, `<aside>` related-links panels, scripts, forms, footers, "Download" links and chart fragments (axis numbers, "Sources:" lines) |
| Footnotes | Markers are removed from sentences; footnote sections are skipped (minutes keep footnote text in a `footnotes` section) |
| Text | Unicode normalisation (non-breaking spaces etc.) **keeping vulgar fractions as typeset** ("2½", not "21⁄2"), invisible characters removed, whitespace collapsed, stray spaces before punctuation fixed |
| Structure | Split into sections/chapters and paragraphs, list items and subheadings |
| 1997–2005 SMP boxes | Boxes were embedded mid-chapter; they are lifted out as their own chapters so the chapter text after a box stays with its chapter |
| Categories | Minutes sections: `international`, `domestic`, `financial_markets`, `outlook`, `policy_considerations`, `decision`, `attendance`, `administrative`, `footnotes`, `other`. SMP chapters: `overview`, `international`, `domestic`, `financial`, `inflation`, `outlook`, `box`, `in_depth`, `forecast_table`, `administrative`, `other` |
| Cash rate decision | Read from the minutes' decision sentence (raise / lower / hold and the new target), including passive wording, the COVID-era packages and levels in basis points |

The original HTML is stored (SMP chapters zlib-compressed; read it with `rba_minutes.smp.chapter_html(conn, chapter_id)`), so text can be re-parsed without downloading again.

## SMP tables

Every `<table>` in every SMP issue is extracted:

| Era | Where the tables are |
|---|---|
| 1997 – Feb 2006 | Inline in the issue's Bulletin page |
| May 2006 – Aug 2017 | The issue's "List of tables" page |
| Nov 2017 onwards | Inline in the chapter pages (forecast appendix pages 2018–2023) |

| Field | Meaning |
|---|---|
| `smp_tables` | One row per table: issue, table number, title, `title_key` (to follow the same table across issues), subtitle/units, notes, sources, column labels, the published HTML |
| `smp_table_cells` | One row per cell: `row_label`, `row_group` (e.g. "Activity", or nested "Year-ended > …"), `row_level` (indented rows), `col_label` (stacked headers joined with " \| "), `value_text` exactly as published, `value_num`, `value_low` / `value_high` for ranges, `value_direction` for arrows, `footnotes`, `is_total` |
| `v_smp_table_cells` | Cells with their table's issue, number and title |
| `data/exports/smp_tables/` | `all_cells.csv` (every cell, long format) and one wide CSV per table, by issue |

Number parsing follows RBA typesetting: `2¾` → 2.75; `−1,504` and `–0.2` → negative; `+0.25` → 0.25; `2½–3` → range 2.5–3 (midpoint 2.75); `↓ 250` → −250 with direction −1; `25 bps` → 25; `2¾ (3¼)` (current and previous forecast) → 2.75; a lone `–`, `na`, `n.a.` → missing. Year ranges such as `2013–14` stay text.

## Databases

### `data/rba_minutes.sqlite`

| Table / view | One row per | Key columns |
|---|---|---|
| `meetings` | meeting | `meeting_date`, `url`, `decision`, `cash_rate_target`, `full_text`, `content_html` |
| `sections` | section of a meeting | `heading`, `category`, `text` |
| `paragraphs` | paragraph | `kind`, `text` (full-text search: `paragraphs_fts`) |
| `dictionary_sentiment` | meeting, policy block or section | `scope`, `lm_tone`, `lm_uncertainty_per_1k`, `hawkish`, `dovish`, `hawk_dove_index` |
| `llm_sentiment` | meeting × labeller × prompt version × input text | `stance`, `stance_score`, `confidence`, `policy_bias`, `decision`, `dominant_concern`, `key_quotes`, `quotes_verified` |
| `v_meeting_sentiment` | meeting | Decision, cash rate and all headline scores side by side |

### `data/rba_smp.sqlite`

| Table / view | One row per | Key columns |
|---|---|---|
| `smp_issues` | issue | `issue_date` (YYYY-MM), `era`, `published_date`, `full_text` (narrative chapters) |
| `smp_chapters` | chapter, box or In Depth study | `title`, `category`, `text`, `content_html_zlib` |
| `smp_paragraphs` | paragraph | `kind`, `text` (full-text search: `smp_paragraphs_fts`) |
| `v_smp_special_studies` | box or In Depth chapter | issue, title, text, word count and its dictionary scores |
| `smp_dictionary_sentiment` | issue, overview, outlook or chapter | as for the minutes |
| `smp_llm_sentiment` | issue × labeller × prompt version × input text | `stance`, `stance_score`, `confidence`, `policy_bias`, `inflation_outlook`, `growth_outlook`, `inflation_risks`, `dominant_concern`, `key_quotes`, `quotes_verified` |
| `v_smp_sentiment` | issue | All headline scores side by side |
| `smp_tables`, `smp_table_cells`, `v_smp_table_cells` | table / cell | See above |
| `forecasts_by_horizon` | variable × forecast quarter × horizon | `cpi_ye`, `underlying_ye`, `gdp_qtr`, `unemployment`; forecast, actual, error (= actual − forecast), since 1990 |
| `smp_forecast_archive` | SMP × variable × target period | 26 variables of the detailed forecast table since November 2018 (cash rate, TWI and oil assumptions from 2024) |

### Using both together

SQL:

```sql
ATTACH 'data/rba_smp.sqlite' AS smp;
SELECT m.meeting_date, m.decision, m.cash_rate_target, s.issue_date, s.llm_stance_score AS smp_stance
FROM v_meeting_sentiment m
JOIN smp.v_smp_sentiment s ON s.issue_date = substr(m.meeting_date, 1, 7);
```

R:

```r
library(DBI)
library(RSQLite)

con <- dbConnect(SQLite(), "data/rba_minutes.sqlite")
dbExecute(con, "ATTACH 'data/rba_smp.sqlite' AS smp")

minutes <- dbGetQuery(con, "SELECT * FROM v_meeting_sentiment")
smp <- dbGetQuery(con, "SELECT * FROM smp.v_smp_sentiment")
forecast_tables <- dbGetQuery(con, "
  SELECT issue_date, row_group, row_label, col_label, value_num
  FROM smp.v_smp_table_cells WHERE title LIKE '%Forecast%'")
studies <- dbGetQuery(con, "SELECT issue_date, title, n_words FROM smp.v_smp_special_studies")

dbDisconnect(con)
```

Full-text search across 30 years of SMPs:

```sql
SELECT i.issue_date, c.title, p.text
FROM smp_paragraphs_fts f
JOIN smp_paragraphs p ON p.paragraph_id = f.rowid
JOIN smp_chapters c USING (chapter_id)
JOIN smp_issues i ON i.issue_id = p.issue_id
WHERE smp_paragraphs_fts MATCH '"housing credit"'
ORDER BY i.issue_date;
```

## What is in the committed databases

Built on 29 October 2026.

| Item | Value |
|---|---|
| Minutes | 213 meetings, 3 Oct 2006 to 28–29 Sep 2026 (latest published); 1,825 sections, 8,574 paragraphs, ~670,000 words |
| Cash rate decisions | Read for all 213 meetings: 157 holds, 29 raises, 27 cuts; every rate chains from the previous meeting, and spot checks match the RBA record |
| SMP issues | 119, Feb 1997 to Aug 2026 (latest published); ~2.1 million words of narrative, 1,166 chapters, 29,262 paragraphs |
| SMP special studies | 332: 327 boxes and 5 In Depth chapters (~286,000 words). All 326 boxes on the RBA's yearly box lists are captured, plus one the lists omit (Aug 1998, "The Australian Dollar and the Yen") |
| SMP tables | 1,485 tables (423 distinct titles), 49,610 cells, 44,775 numeric |
| Forecasts | 4,718 historical forecasts with outcomes (1990–2025); 3,459 detailed SMP forecast values (Nov 2018 – Aug 2026) |
| Claude labels | All 213 meetings and all 119 SMP Overviews, each on the final cleaned text; every supporting quote verified word for word |

### Verification

| Check | Result |
|---|---|
| Every non-empty `<table>` in the downloaded HTML extracted | 1,485 of 1,485 (one empty placeholder table in Feb 2022 skipped) |
| RBA forecast archive workbook vs tables extracted from the SMP HTML (independent publications of the same numbers) | 3,459 of 3,459 values match exactly |
| Independent agent audit of sampled tables, cell by cell against the published HTML | TABLE_QA_RESULT |
| Every box on the RBA's own box lists captured | 326 of 326 |
| Cash rate decisions chain consistently | 213 of 213 |
| Claude quotes found verbatim in the source text | 213 of 213 minutes, 119 of 119 SMPs |

### How the measures relate

Minutes (correlation across meetings):

| Measure | With the next meeting's change in the cash rate | With the Claude stance score |
|---|---|---|
| Claude stance score | 0.51 | 1.00 |
| Hawkish/dovish index (whole minutes) | 0.51 | 0.61 |
| Hawkish/dovish index (policy sections) | 0.43 | 0.61 |
| Loughran-McDonald tone | 0.25 | 0.02 |

SMP (80 issues since October 2006, using the decisions in the minutes):

| Measure | With the change in the cash rate over the next 3 months | Next 6 months |
|---|---|---|
| Claude stance score | 0.51 | 0.40 |
| Hawkish/dovish index (whole issue) | 0.41 | 0.36 |
| Loughran-McDonald tone | 0.39 | 0.37 |

After a dovish SMP the cash rate never rose within three months (0 of 32); after a hawkish one it was cut once (1 of 30). These are in-sample descriptive statistics, not a tested forecasting model.

## Method notes and caveats

| Topic | Note |
|---|---|
| Loughran-McDonald | Word lists bundled with `pysentiment2` (MIT licence for the code; the dictionary is free for academic use, see [the authors' site](https://sraf.nd.edu/loughranmcdonald-master-dictionary/)). Built from US company filings, so "unemployment" or "decline" count as negative even when used descriptively |
| Hawkish/dovish | Follows Apel, Blix Grimaldi & Hull (2022, *Journal of Money, Credit and Banking*), with word lists adapted to RBA wording; not an exact replication. ±4-word window within a sentence, simple negation, plus phrases such as "increase the cash rate" |
| Claude labels | Made by Claude (`claude-opus-5-5`) inside a Claude Code session, by many parallel workers following identical written instructions, in batches of 5–10 documents (`model = 'claude-opus-5-5 (Claude Code session)'`). Minutes: the *Considerations for monetary policy* and *The Decision* sections. SMP: the Overview (Introduction before 2006). Small differences in convention between workers are possible (e.g. whether a hold with a tightening bias is 0 or +1, or whether too-*low* inflation is an `inflation` or `growth_employment` concern). The labeller knows RBA history, so later events may colour some calls despite the instruction to judge each document on its own text. Superseded labels (made on earlier versions of the text) are kept for audit; the views use the latest. `python -m rba_minutes llm` gives a single API labeller for comparison |
| Dates | `meeting_date` is the day the board met (the second day of two-day meetings since 2025); SMP `issue_date` is the issue month and `published_date` the page's publication metadata |
| Underlying inflation | The RBA's measure of underlying inflation changed several times since 1990 (see `forecasts_by_horizon.measure`); actuals are for the measure forecast at the time |
| Not in the SMP | Detailed borrower stress-testing (e.g. how many mortgage holders would face negative cash flow if unemployment rose) is published in the RBA's *Financial Stability Review*, not the SMP; it is not in these databases |
| Copyright | © Reserve Bank of Australia. Check the RBA's copyright and disclaimer page before sharing the text |

## Tests

```bash
python -m pytest -q
```

Most tests run offline against synthetic HTML in `tests/fixtures/` that copies the RBA layouts (not real RBA documents). `tests/test_forecasts.py` also checks the committed forecast files and the forecast cross-check against the built SMP database.

## Earlier R scraper

`minutes_scraper.Rmd` and `Code/minutes_scraper.R` are the original 2016 R scraper (2006–2016, `RCurl`/`XML`), kept for reference.
