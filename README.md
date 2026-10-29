# RBA Minutes

Downloads the minutes of every Reserve Bank of Australia monetary policy meeting (the RBA has published them since the October 2006 meeting), cleans the text, stores it in a SQLite database, and scores sentiment in two ways:

| Method | What it measures | Cost | Reproducible |
|---|---|---|---|
| Loughran-McDonald dictionary | Positive, negative, uncertainty and constraining word counts; tone = (pos − neg) / (pos + neg) | Free | Exactly |
| Hawkish/dovish dictionary | Economic terms paired with direction words ("inflation … rising" = hawkish), plus direct policy phrases | Free | Exactly |
| Claude (LLM) | Hawkish/neutral/dovish label, −2…+2 score, confidence, policy bias, decision, main concern, supporting quotes | A few dollars for every meeting (needs an Anthropic API key) | Stored with model, prompt version and input hash |

## Quick start

```bash
pip install -r requirements.txt
```

```bash
python -m rba_minutes all
```

`all` runs `scrape` → `build` → `score` → `export`. To add the LLM classification:

```bash
export ANTHROPIC_API_KEY=...
```

```bash
python -m rba_minutes llm --limit 5
```

Check the first five results, then run it again without `--limit`. Without an API key, use `llm-export`, have Claude Code label each batch file, then `llm-import`. Meetings that already have a result are skipped, so re-running only pays for new or changed meetings.

| Command | What it does |
|---|---|
| `scrape` | Reads each year's index page from 2006 onwards and downloads every minutes page to `data/raw/` (one-second pause between requests; cached pages are not re-downloaded unless you pass `--refresh`) |
| `build` | Parses the cached pages into `data/rba_minutes.sqlite` |
| `score` | Recomputes the dictionary sentiment for every meeting, policy block and section |
| `llm` | Classifies each meeting with Claude (`--model`, `--effort`, `--scope policy\|full`, `--limit`) |
| `llm-export` | Writes the same model inputs and instructions to Markdown batch files in `data/llm_batches/`, for labelling by Claude inside a Claude Code session instead of through the API |
| `llm-import` | Validates the `*.labels.json` answers for those batches against the same schema and stores them (model recorded as `claude-opus-5-5 (Claude Code session)`) |
| `export` | Writes CSV copies of the main tables to `data/exports/` |
| `status` | Prints counts and the date range in the database |

## Cleaning

| Step | Detail |
|---|---|
| Content only | Keeps the page's `#content` block; drops navigation, `<aside>` related-links panels, scripts, forms and the footer |
| Footnotes | Markers are removed from sentences; footnote text is kept in its own `footnotes` section and left out of sentiment |
| Text | Unicode NFKC normalisation (non-breaking spaces etc.), invisible characters removed, whitespace collapsed, stray spaces before punctuation fixed |
| Structure | Split by `<h2>` heading into sections, and into paragraphs, list items and subheadings |
| Section categories | Each heading is mapped to `international`, `domestic`, `financial_markets`, `outlook`, `policy_considerations`, `decision`, `attendance`, `footnotes` or `other`, because headings were reworded over the years |
| Decision | A regex reads the decision sentence to get `raise` / `lower` / `hold` and the cash rate target. It is a heuristic, so check it before relying on it |

The page's original content HTML is stored too, so the text can be re-parsed later without downloading again.

## Database

`data/rba_minutes.sqlite`

| Table / view | One row per | Key columns |
|---|---|---|
| `meetings` | meeting | `meeting_date`, `url`, `decision`, `cash_rate_target`, `full_text`, `content_html` |
| `sections` | section of a meeting | `heading`, `category`, `text` |
| `paragraphs` | paragraph | `kind`, `text` (with full-text search through `paragraphs_fts`) |
| `dictionary_sentiment` | meeting, policy block or section | `scope`, `lm_tone`, `lm_uncertainty_per_1k`, `hawkish`, `dovish`, `hawk_dove_index` |
| `llm_sentiment` | meeting × model × prompt version | `stance`, `stance_score`, `confidence`, `policy_bias`, `key_quotes`, `quotes_verified` |
| `v_meeting_sentiment` | meeting | All the headline scores side by side |

Full-text search example:

```sql
SELECT m.meeting_date, p.text
FROM paragraphs_fts f
JOIN paragraphs p ON p.paragraph_id = f.rowid
JOIN meetings m USING (meeting_id)
WHERE paragraphs_fts MATCH '"neutral rate"'
ORDER BY m.meeting_date;
```

### Reading it from R

```r
library(DBI)
library(RSQLite)

con <- dbConnect(SQLite(), "data/rba_minutes.sqlite")
sentiment <- dbGetQuery(con, "SELECT * FROM v_meeting_sentiment")
sentiment$meeting_date <- as.Date(sentiment$meeting_date)
plot(sentiment$meeting_date, sentiment$hawk_dove_index, type = "l",
     xlab = "Meeting", ylab = "Hawkish-dovish index")
dbDisconnect(con)
```

## What is in the committed database

Built on 29 October 2026.

| Item | Value |
|---|---|
| Meetings | 213, from 3 October 2006 (the first published minutes) to 28–29 September 2026 (the latest published) |
| Text | 1,825 sections, 8,574 paragraphs, about 680,000 words of discussion |
| Decisions | Read for all 213 meetings: 157 holds, 29 raises, 27 cuts; `check` confirms every rate chains from the previous meeting |
| Dictionary sentiment | All 213 meetings, plus each policy block and section |
| LLM labels | All 213 meetings, labelled by Claude (`claude-opus-5-5`) inside a Claude Code session from the policy sections (`model = 'claude-opus-5-5 (Claude Code session)'`); every supporting quote was verified word for word against the minutes |

How the measures relate (correlation across meetings):

| Measure | With the next meeting's change in the cash rate | With the LLM stance score |
|---|---|---|
| LLM stance score | 0.52 | 1.00 |
| Hawkish/dovish index (whole minutes) | 0.51 | 0.61 |
| Hawkish/dovish index (policy sections) | 0.43 | 0.60 |
| Loughran-McDonald tone | 0.25 | 0.01 |

The LLM labels were made in 22 batches of about 10 meetings by six parallel workers following identical written instructions, rather than by one API call per meeting. Small differences in convention between workers are possible, e.g. whether a "modest" cut scores −1 or −2, or whether too-*low* inflation is recorded as an `inflation` or `growth_employment` concern. The labeller also knows RBA history, so despite the instruction to judge each meeting on its own text, later events may colour some calls. Re-running with `python -m rba_minutes llm` (API key needed) gives one consistent labeller for comparison; both sets can sit side by side in `llm_sentiment`.

## Method notes and caveats

| Topic | Note |
|---|---|
| Loughran-McDonald | The word lists are bundled with the `pysentiment2` package (MIT licence for the code). The dictionary itself is free for academic use; see [the authors' site](https://sraf.nd.edu/loughranmcdonald-master-dictionary/) for terms. It was built from US company filings, so words such as "unemployment" or "decline" count as negative even when used neutrally in economic description |
| Hawkish/dovish | Follows the approach of Apel, Blix Grimaldi & Hull (2022, *Journal of Money, Credit and Banking*), with word lists adapted to RBA wording. It is not an exact replication. It uses a ±4-word window within each sentence, simple negation ("not weak" counts as hawkish), and counts phrases such as "increase the cash rate". Discussing the case for a cut counts as dovish even if the Board held |
| LLM | Default model `claude-opus-5-5` at `medium` effort, reading only the *Considerations for monetary policy* and *The Decision* sections (`--scope full` sends the whole discussion). Requests opt into Anthropic's server-side refusal fallback; `served_model` records which model answered. `quotes_verified` checks that every quote the model gives appears word for word in the minutes |
| Dates | `meeting_date` is the day the board met; the decision is announced separately, usually the same or next day, so match to announcement dates with care |
| Board change | From 2025 the Monetary Policy Board replaced the Reserve Bank Board for monetary policy. The scraper expects their minutes under the same `/monetary-policy/rba-board-minutes/` path |
| Copyright | Minutes are © Reserve Bank of Australia. Check the RBA's copyright and disclaimer page for reuse terms before sharing the text |

## Tests

```bash
python -m pytest -q
```

The tests run offline against synthetic HTML pages in `tests/fixtures/` that copy the layout of an RBA minutes page. They are not real RBA documents.

## Earlier R scraper

`minutes_scraper.Rmd` and `Code/minutes_scraper.R` are the original 2016 R scraper (2006–2016, `RCurl`/`XML`). They are kept for reference; the Python pipeline above replaces them.
