# RBA Minutes, Statements on Monetary Policy and Financial Stability Reviews

Downloads, cleans, stores and scores three Reserve Bank of Australia publications, plus the RBA's own forecast history, and tests whether they help predict cash rate decisions:

| Source | Coverage | What you get |
|---|---|---|
| Monetary policy meeting minutes | Every meeting since the first published minutes (3 October 2006) | Cleaned text by section and paragraph, the cash rate decision, dictionary sentiment, Claude stance labels |
| Statement on Monetary Policy (SMP) | Every issue since February 1997 (published in the RBA Bulletin until 2005) | Cleaned text by chapter and paragraph, every box and "In Depth" special study, **every table as clean data**, **every graph** (image, title, SVG text, and underlying data where published), dictionary sentiment, Claude stance labels |
| Financial Stability Review (FSR) | Every issue since the first in March 2004 | Cleaned text, every box, Focus Topic, article and special chapter (household mortgage stress, housing, banks), every table and graph, dictionary sentiment, Claude risk assessments |
| RBA forecasts | Central forecasts since 1990 with outcomes and errors; detailed SMP forecast tables since November 2018 | Tidy forecast tables, cross-checked against the SMP tables |
| Prediction analysis | Every Board decision since 1990, with ABS inflation, unemployment, GDP and wages | Out-of-sample tests of what predicts the next decision: see [`analysis/README.md`](analysis/README.md) |

Everything is in three SQLite files (each under GitHub's 100 MB limit; all readable from R and Python):

| File | Contents |
|---|---|
| `data/rba_minutes.sqlite` | Minutes |
| `data/rba_smp.sqlite` | SMP text, tables, graphs, RBA forecasts |
| `data/rba_fsr.sqlite` | FSR text, tables, graphs, assessments |

## Quick start

```bash
pip install -r requirements.txt
```

Minutes (scrape → build → score → export):

```bash
python -m rba_minutes all
```

Statements on Monetary Policy, in this order (`smp-tables` after `forecasts-load` so the forecast cross-check can run):

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
python -m rba_minutes smp-forecasts
```

```bash
python -m rba_minutes forecasts-load
```

```bash
python -m rba_minutes smp-tables
```

```bash
python -m rba_minutes smp-graphs
```

Financial Stability Reviews:

```bash
python -m rba_minutes fsr-scrape
```

```bash
python -m rba_minutes fsr-build
```

```bash
python -m rba_minutes fsr-score
```

```bash
python -m rba_minutes fsr-tables
```

```bash
python -m rba_minutes fsr-graphs
```

Prediction backtests (and probabilities for the next decision; `--refresh-data` first downloads the RBA cash rate history, RBA tables G1 and H5 (ABS CPI and Labour Force), daily F1 money-market rates (OIS, bank bills), ABS real GDP and Wage Price Index from the ABS Data API, and today's ASX RBA Rate Tracker files into `data/macro/`):

```bash
python -m rba_minutes predict --refresh-data --next 2026-11-03
```

Downloads are cached (`data/raw/`, `data/raw_smp/`, `data/raw_fsr/`), pause one second between requests, and re-runs only fetch new pages. Graph images are saved under `data/smp_graphs/` and `data/fsr_graphs/` (not committed: several hundred MB; `smp-graphs` / `fsr-graphs` re-download them).

### Commands

| Command | What it does |
|---|---|
| `scrape` / `build` / `score` | Minutes: download every page, parse into the database, compute dictionary sentiment |
| `check` | Minutes data-quality checks: every cash rate decision must chain from the previous meeting, every meeting needs policy and decision sections, no stray footnote markers or navigation text |
| `llm` | Claude stance labels through the Anthropic API (needs `ANTHROPIC_API_KEY`; `--model`, `--effort`, `--scope`, `--limit`) |
| `llm-export` / `llm-import` | The same labelling done by Claude inside a Claude Code session: export batch files with the inputs and instructions, then validate and load the JSON answers |
| `smp-scrape` | Download every SMP issue: the Bulletin page (1997–2005) or every chapter and box page (2006 onwards) |
| `smp-build` / `smp-score` | Parse SMP issues into chapters and paragraphs; dictionary sentiment per issue, overview, outlook and chapter |
| `smp-boxes` / `smp-check` | Download the RBA's yearly lists of SMP boxes and check every listed box is captured; check every issue has an overview |
| `smp-llm-export` / `smp-llm-import` | Claude stance labels for each SMP Overview, done in a Claude Code session |
| `smp-tables` | Download the "List of tables" pages (2006–2017) and graph-data workbooks (2024+), extract every table cell by cell, write CSVs, and cross-check the forecast tables against the RBA forecast archive |
| `smp-graphs` | Find every graph in every issue, download its image (and its own page for 2006–2023), extract SVG text, store the details, and parse the graph-data workbooks into long format |
| `smp-forecasts` / `forecasts-load` | Download the RBA forecast archive and historical forecast files, and load them into tables |
| `fsr-scrape` / `fsr-build` / `fsr-score` / `fsr-tables` / `fsr-graphs` | The same steps for the FSR |
| `fsr-check` | Every issue on the RBA's FSR page stored; every box, Focus Topic and article on the RBA's own lists captured; every issue has an overview |
| `fsr-llm-export` / `fsr-llm-import` | Claude risk assessments for each FSR Overview (Financial Stability Assessment from 2023), done in a Claude Code session |
| `predict` | Backtest decision predictions and write `analysis/` (`--next YYYY-MM-DD` adds probabilities for that decision) |
| `export` / `status` | Minutes CSV exports and a summary |

## Cleaning

| Step | Detail |
|---|---|
| Content only | Keeps the page's `#content` block; drops navigation, `<aside>` related-links panels, scripts, forms, footers, "Download" links and chart fragments (axis numbers, "Sources:" lines) |
| Footnotes | Markers are removed from sentences; footnote sections are skipped (minutes keep footnote text in a `footnotes` section) |
| Text | Unicode normalisation (non-breaking spaces etc.) **keeping vulgar fractions as typeset** ("2½", not "21⁄2"), invisible characters removed, whitespace collapsed, stray spaces before punctuation fixed |
| Structure | Split into sections/chapters and paragraphs, list items and subheadings |
| 1997–2005 SMP boxes | Boxes were embedded mid-chapter; they are lifted out as their own chapters so the chapter text after a box stays with its chapter |
| Minutes sections | `international`, `domestic`, `financial_markets`, `outlook`, `policy_considerations`, `decision`, `attendance`, `administrative`, `footnotes`, `other` |
| SMP chapters | `overview`, `international`, `domestic`, `financial`, `inflation`, `outlook`, `box`, `in_depth`, `forecast_table`, `administrative`, `other` |
| FSR chapters | `overview` (Overview, Foreword, At a Glance, In Brief, Financial Stability Assessment), `global`, `household_business`, `financial_system`, `regulation`, `box`, `focus_topic`, `article`, `special_chapter`, `annex`, `administrative` |
| Cash rate decision | Read from the minutes' decision sentence (raise / lower / hold and the new target), including passive wording, the COVID-era packages and levels in basis points |

The original HTML is stored (SMP and FSR chapters zlib-compressed; read it with `rba_minutes.smp.chapter_html(conn, chapter_id)` or `rba_minutes.fsr.chapter_html(...)`), so text can be re-parsed without downloading again.

## Special studies

Boxes and similar studies are stored as chapters of their own and kept out of each issue's narrative text and sentiment scores, so they can be studied separately (`v_smp_special_studies`, `v_fsr_special_studies`).

| Publication | Kinds | Count |
|---|---|---|
| SMP | Boxes; "In Depth" chapters (2024+) | 327 boxes, 5 In Depth (~286,000 words) |
| FSR | Boxes; Focus Topics (2022+); articles (2004–2007); special chapters (e.g. "Assessing the Effects of Housing Lending Policy Measures", Oct 2018; "Mortgage Macroprudential Policies", Oct 2021) | 114 boxes, 14 Focus Topics, 9 articles, 2 special chapters (~203,000 words) |

The FSR is where the RBA's analysis of household mortgage stress lives, including how many borrowers would struggle if unemployment rose. Examples among the 32 studies whose titles mention households, mortgages, borrowers or arrears:

| Issue | Study |
|---|---|
| Sep 2012 | Box B: Households' Mortgage Prepayment Buffers |
| Oct 2018 | Box C: Vulnerable Households and Financial Stress |
| Oct 2022 | Box B: The Impact of Rising Interest Rates and Inflation on Indebted Households' Cash Flows |
| Apr 2023 | Box B: Scenario Analysis on Indebted Households' Spare Cash Flows and Prepayment Buffers |
| Oct 2023 | Focus Topic: Indicators of Household Financial Stress |
| Mar 2024 | Focus Topic: Scenario Analysis of the Resilience of Mortgagors and Businesses to Higher Inflation and Interest Rates |

```sql
SELECT issue_date, category, title, n_words
FROM v_fsr_special_studies
WHERE title LIKE '%mortgag%' OR title LIKE '%household%' OR title LIKE '%unemploy%'
ORDER BY issue_date;
```

## Tables

Every `<table>` in every SMP and FSR issue is extracted with the same code:

| Era | Where the tables are |
|---|---|
| SMP 1997 – Feb 2006 | Inline in the issue's Bulletin page |
| SMP May 2006 – Aug 2017; FSR to about 2019 | The issue's "List of tables" page |
| Later issues | Inline in the chapter pages |

| Field | Meaning |
|---|---|
| `smp_tables` / `fsr_tables` | One row per table: issue, table number, title, `title_key` (to follow the same table across issues), subtitle/units, notes, sources, column labels, the published HTML |
| `smp_table_cells` / `fsr_table_cells` | One row per cell: `row_label`, `row_group` (heading path, e.g. "Housing loans > Fixed rate", or a panel such as "Year-average"), `row_level` (indent depth), `col_label` (stacked headers joined with " \| "), `value_text` exactly as published, `value_num`, `value_low` / `value_high` for ranges, `value_direction` for arrows, `footnotes` (marks from the group, row, column and cell), `is_total` |
| `v_smp_table_cells` / `v_fsr_table_cells` | Cells with their table's issue, number and title |
| `data/exports/smp_tables/`, `data/exports/fsr_tables/` | `all_cells.csv` (every cell, long format) and one wide CSV per table, by issue |

Number parsing follows RBA typesetting: `2¾` → 2.75; `−1,504` and `–0.2` → negative; `+0.25` → 0.25; `2½–3` → range 2.5–3 (midpoint 2.75); `↓ 250` → −250 with direction −1; `25 bps` → 25; `2¾ (3¼)` (current and previous forecast) → 2.75; a lone `–`, `na`, `n.a.` → missing. Year ranges such as `2013–14` stay text.

Structure handled: multi-row and spanning headers, headers missing their stub cell, spacer columns, in-body header rows and panel headings ("Year-ended" / "Year-average" / "Assumptions"), nested and indented group headings (`tr-head` sections, bold and plain sub-headings, `sub`/`sub2` classes and leading dashes), side-by-side panels, wrapped row labels, continuation rows, "(previous)" forecast rows, unlabelled subtotals, and notes or sources written inside the table body or footer.

## Graphs

| Era | How graphs are published | What is stored |
|---|---|---|
| SMP 1997–2005 | GIF images in the Bulletin page, "Graph N: Title" | Number, title, chapter, image |
| SMP 2006–2023, FSR 2004–2023 | One page per graph linked from the chapter | Number, title, chapter, the graph page, image |
| 2024 onwards | SVG in a `<figure>` with a long text description | Number, title, description, **all SVG text** (title, axis labels, notes, sources; searchable), image |
| 2025 onwards (workbooks) | "Underlying data for selected graphs" | Every published value (`v_smp_graph_data`, `v_fsr_graph_data`): graph, panel, series, x (date or category), value; graphs whose data the RBA cannot publish are marked "Data restricted" |

| Item | SMP | FSR |
|---|---:|---:|
| Graphs | 10,773 in 119 issues | 3,015 in 46 issues |
| Images downloaded | 10,773 (7,554 GIF, 3,219 SVG; 500 MB) | 3,015 (2,275 GIF, 740 SVG; 138 MB) |
| With a title | 10,768 | 3,014 |
| SVG text extracted | 3,219 | 740 |
| Long text descriptions (2023 onwards) | 1,442 | 412 |
| Graph-data values (2025–2026 workbooks) | 148,086 in 895 series, 188 graphs | 14,570 in 167 series, 45 graphs |
| Graphs marked "Data restricted" in the workbooks | 239 | 40 |

Titles come from the image's alt text, the graph's own page, or the issue's "List of graphs" page. For 2023+ graphs whose title exists only inside the SVG, the title is the largest bold text in the image. The RBA's SVGs position each glyph and leave out the spaces ("HousingandBusinessCredit"), so spaces are rebuilt from the glyph positions, using character widths learned from all 3,959 SVGs (they match Arial's published metrics). 1997–2005 box graphs are numbered as printed ("Box 1 Graph 2") and placed in their box.

```sql
SELECT issue_date, chapter_title, graph_number, title, image_file
FROM smp_graphs WHERE title LIKE '%neutral rate%' ORDER BY issue_date;

SELECT x, value FROM v_smp_graph_data
WHERE issue_date = '2026-08' AND graph_number = '1.2' AND series = 'Nominal cash rate';
```

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
| `smp_tables`, `smp_table_cells`, `v_smp_table_cells` | table / cell | See Tables |
| `smp_graphs` | graph | `issue_date`, `chapter_title`, `graph_number`, `title`, `description`, `svg_text`, `image_url`, `image_file`, `graph_page` |
| `smp_graph_series`, `smp_graph_values`, `v_smp_graph_data` | graph series / published value (2025+ workbooks) | `graph_number`, `graph_title`, `source`, `panel`, `series`, `x_label`, `x`, `value` (the view joins them in long format) |
| `forecasts_by_horizon` | variable × forecast quarter × horizon | `cpi_ye`, `underlying_ye`, `gdp_qtr`, `unemployment`; forecast, actual, error (= actual − forecast), since 1990 |
| `smp_forecast_archive` | SMP × variable × target period | 26 variables of the detailed forecast table since November 2018 (cash rate, TWI and oil assumptions from 2024) |

### `data/rba_fsr.sqlite`

| Table / view | One row per | Key columns |
|---|---|---|
| `fsr_issues` | issue | `issue_date` (YYYY-MM), `published_date`, `full_text` (narrative chapters) |
| `fsr_chapters` | chapter, box, Focus Topic, article | `title`, `category`, `text`, `content_html_zlib` |
| `fsr_paragraphs` | paragraph | `kind`, `text` (full-text search: `fsr_paragraphs_fts`) |
| `v_fsr_special_studies` | box, Focus Topic, article or special chapter | issue, title, text, word count, tone |
| `fsr_dictionary_sentiment` | issue, overview or chapter | as for the minutes |
| `fsr_llm_assessment` | issue × labeller × prompt version × input text | `risk_level` (low/moderate/elevated/high), `risk_trend`, `system_resilience`, `household_stress`, `housing_market`, `credit_conditions`, `main_risk_source`, `policy_implication` (what the concerns imply for rates), `confidence`, `key_quotes`, `quotes_verified` |
| `v_fsr_sentiment` | issue | All headline scores; `llm_risk_level_ordinal` (1–4) |
| `fsr_tables`, `fsr_table_cells`, `v_fsr_table_cells`, `fsr_graphs`, `fsr_graph_series`, `fsr_graph_values`, `v_fsr_graph_data` | table / cell / graph / value | As for the SMP |

### Using them together

SQL:

```sql
ATTACH 'data/rba_smp.sqlite' AS smp;
ATTACH 'data/rba_fsr.sqlite' AS fsr;
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
dbExecute(con, "ATTACH 'data/rba_fsr.sqlite' AS fsr")

minutes <- dbGetQuery(con, "SELECT * FROM v_meeting_sentiment")
smp <- dbGetQuery(con, "SELECT * FROM smp.v_smp_sentiment")
fsr <- dbGetQuery(con, "SELECT * FROM fsr.v_fsr_sentiment")
forecast_tables <- dbGetQuery(con, "
  SELECT issue_date, row_group, row_label, col_label, value_num
  FROM smp.v_smp_table_cells WHERE title LIKE '%Forecast%'")
studies <- dbGetQuery(con, "SELECT issue_date, category, title, n_words FROM fsr.v_fsr_special_studies")

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
| Minutes | 213 meetings, 3 Oct 2006 to 28–29 Sep 2026 (latest published); 1,825 sections, 8,574 paragraphs, ~680,000 words |
| Cash rate decisions | Read for all 213 meetings: 157 holds, 29 raises, 27 cuts; every rate chains from the previous meeting, and spot checks match the RBA record |
| SMP issues | 119, Feb 1997 to Aug 2026 (latest published); ~2.1 million words of narrative, 1,166 chapters, 29,262 paragraphs |
| SMP special studies | 332: 327 boxes and 5 In Depth chapters. All 326 boxes on the RBA's yearly box lists are captured, plus one the lists omit (Aug 1998, "The Australian Dollar and the Yen") |
| SMP tables | 1,485 tables (422 distinct titles), 48,363 cells, 44,775 numeric |
| FSR issues | 46, Mar 2004 to Oct 2026; ~821,000 words of narrative, 392 chapters, 12,266 paragraphs |
| FSR special studies | 139 (~203,000 words); all 128 boxes/Focus Topics and 9 articles on the RBA's own lists captured |
| FSR tables | 242 tables (162 distinct titles), 7,201 cells, 5,634 numeric |
| Forecasts | 4,718 historical forecasts with outcomes (1990–2025); 3,459 detailed SMP forecast values (Nov 2018 – Aug 2026) |
| Claude labels | All 213 meetings, all 119 SMP Overviews and all 46 FSR Overviews, each on the final cleaned text; every supporting quote verified word for word |

### Verification

| Check | Result |
|---|---|
| Every non-empty `<table>` in the downloaded HTML extracted | SMP 1,485, FSR 242 (one empty placeholder table in the Feb 2022 SMP has no cells and is skipped) |
| RBA forecast archive workbook vs tables extracted from the SMP HTML (independent publications of the same numbers) | 3,459 of 3,459 values match exactly |
| Independent agent audits, cell by cell against the published HTML, on random samples (each round on tables not audited before) | See below |
| Every box on the RBA's own lists captured | SMP 326 of 326; FSR 137 of 137 (boxes, Focus Topics, articles) |
| Cash rate decisions chain consistently | 213 of 213 |
| Claude quotes found verbatim in the source text | 213 of 213 minutes, 119 of 119 SMPs, 46 of 46 FSRs |
| Every graph found in the pages has its image | SMP 10,773 of 10,773; FSR 3,015 of 3,015 |

Table audits (each by agents working only from the published HTML with their own code; every fault found was fixed, checked against the whole collection for side effects, and pinned by a regression test on the real table in `tests/fixtures/rba_tables/`):

| Round | Sample | Tables fully correct | Value errors in stored cells | Faults found |
|---|---|---|---|---|
| 1 | 6 agents, earlier extractor | — | 0 | Structure errors in about 6–10% of tables (headers, groups, panels); extractor rewritten |
| 2 | 120 random SMP tables | 98 (82%) | 0 (23 values lost in 2 tables) | 15 patterns: text columns read as panels, groups closed too early or too late, wrapped labels, footnotes on spanning headers, and others |
| 3 | 75 further random SMP tables | 73 (97%) | 0 (1 value lost) | Header row missing its stub cell; unlabelled subtotal in a one-column table |
| FSR | 40 random FSR tables | 37 (93%) | 0 (one row of 9 values lost) | Two-digit numbers read as years; multi-dot table numbers; footers split by line breaks; bold summary row |

Known remaining limitations: a bold summary row ending a section can lose its group heading (e.g. "Net profit after tax" in two 2005 FSR tables); a 1997 SMP table with two stacked sub-tables keeps the first heading; where the RBA's own markup is inconsistent (an unindented row inside an indented group, Mar 2011 SMP Table 3.2) the stored group follows the markup.

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

These are in-sample correlations. Whether the text adds anything *beyond* inflation and unemployment, out of sample, is tested in [`analysis/README.md`](analysis/README.md).

## Prediction: what helps predict the next decision?

Summary of [`analysis/README.md`](analysis/README.md) (every decision since 1990; for each meeting, models are fitted only on earlier meetings and documents count only if published before the decision):

| Added to inflation, unemployment and the last move | Sample | Better probabilities? | Better calls? |
|---|---|---|---|
| SMP tone (Claude or dictionary) | 2001–2026, 273 meetings | No (gain ≈ 0) | No |
| RBA's year-ahead inflation forecast | 2004–2026, 233 meetings | Slightly, not significant | No |
| Inflation surprise (outcome minus the RBA's nowcast) | 2004–2026, 233 meetings | No | No |
| **Market pricing** (1-month OIS minus the cash rate: the ASX Rate Tracker's information) | 2005–2022, 189 meetings | **Yes, by far the strongest** (log loss 0.629 → 0.216; 29 of 46 moves called, 2 false alarms) | **Yes** (accuracy 78.8% → 89.9%) |
| Minutes, on top of market pricing | 2011–2022, 131 meetings | Yes, small but significant (+0.020 per meeting) | Slightly (14 against 13 of 26 moves, no false alarms) |
| ABS real GDP growth | 2003–2026, 251 meetings | No (slightly worse) | No |
| ABS Wage Price Index growth | 2003–2026, 251 meetings | No (gain ≈ 0) | No |
| FSR risk assessment | 2008–2026, 193 meetings | No | No |
| **Minutes** (Claude, or dictionary only) | 2010–2026, 172 meetings | **Yes, significant** (log loss 0.616 → 0.500; dictionary-only 0.550) | No: 15 against 12 of 39 moves caught, but 14 against 6 false alarms |

Macro data are today's revised figures rather than the first estimates available at each meeting, which flatters the macro models slightly (see the caveat in `analysis/README.md`).

Once market pricing is known, inflation, unemployment and the last move add nothing. The RBA stopped publishing OIS in December 2022; the 1-month bank bill rate stands in to 2026 but is a noisier signal.

For the 3 November 2026 decision, the ASX futures price an 80–82% chance of a 25 bp rise to 4.85% (ASX Rate Tracker 80% on 29 October, saved in `data/macro/asx/`; IBKR close 82% on 30 October); the backtested models that include market pricing give 76–87% (87% with the September minutes). On 2001–2022 history a raise priced this strongly happened about four times in five, and over the last 10 decisions (August 2025 to September 2026, IBKR futures data) the futures priced the right direction every time.

## Method notes and caveats

| Topic | Note |
|---|---|
| Loughran-McDonald | Word lists bundled with `pysentiment2` (MIT licence for the code; the dictionary is free for academic use, see [the authors' site](https://sraf.nd.edu/loughranmcdonald-master-dictionary/)). Built from US company filings, so "unemployment" or "decline" count as negative even when used descriptively |
| Hawkish/dovish | Follows Apel, Blix Grimaldi & Hull (2022, *Journal of Money, Credit and Banking*), with word lists adapted to RBA wording; not an exact replication. ±4-word window within a sentence, simple negation, plus phrases such as "increase the cash rate" |
| Claude labels | Made by Claude (`claude-opus-5-5`) inside a Claude Code session, by parallel workers following identical written instructions, in batches of 5–10 documents (`model = 'claude-opus-5-5 (Claude Code session)'`). Minutes: the *Considerations for monetary policy* and *The Decision* sections. SMP: the Overview (Introduction before 2006). FSR: the Overview (Financial Stability Assessment from late 2023). Small differences in convention between workers are possible. The labeller knows RBA history, so later events may colour some calls despite the instruction to judge each document on its own text (the prediction analysis checks this with dictionary-only measures). Superseded labels are kept for audit; the views use the latest. `python -m rba_minutes llm` gives a single API labeller for comparison |
| FSR risk score | The FSR prompt asked for four risk levels and a 1–5 score; batches mapped the score onto the levels slightly differently, so use `llm_risk_level_ordinal` (1–4), not `llm_risk_score` |
| Dates | `meeting_date` is the day the board met (the second day of two-day meetings since 2025); `issue_date` is the issue month and `published_date` the page's publication metadata |
| Underlying inflation | The RBA's measure of underlying inflation changed several times since 1990 (see `forecasts_by_horizon.measure`); actuals are for the measure forecast at the time |
| Graph images | Not committed (several hundred MB); `image_file` gives the local path after `smp-graphs` / `fsr-graphs` |
| Copyright | © Reserve Bank of Australia. Check the RBA's copyright and disclaimer page before sharing the text, tables or graphs |

## Tests

```bash
python -m pytest -q
```

Parser tests run offline against synthetic HTML in `tests/fixtures/` that copies the RBA layouts, and against 46 real RBA tables in `tests/fixtures/rba_tables/` that the audits flagged (excerpts used as test fixtures). `tests/test_forecasts.py` also checks the committed forecast files and the forecast cross-check against the built SMP database.

## Earlier R scraper

`minutes_scraper.Rmd` and `Code/minutes_scraper.R` are the original 2016 R scraper (2006–2016, `RCurl`/`XML`), kept for reference.
