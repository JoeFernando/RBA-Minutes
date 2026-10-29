# Do the RBA's own words help predict its next decision?

Built by `python -m rba_minutes predict --next 2026-11-03` (code: `rba_minutes/predict.py`).
Data to 29 October 2026. All figures below are **out of sample**: for each meeting, every model is
fitted only on earlier meetings and then predicts that meeting.

## What was tested

| Item | Detail |
|---|---|
| Outcome | Every Board decision since January 1990 (402 decisions, from the RBA's cash rate page, which lists unchanged decisions too): cut, hold or raise |
| Macro model | Trimmed mean inflation minus 2.5 (ABS CPI, from RBA table G1; used from 28 days after the quarter), six-month change in the unemployment rate (ABS Labour Force, from RBA table H5; from 18 days after the month), direction of the last move |
| Market pricing | What the ASX RBA Rate Tracker measures, rebuilt from RBA table F1 (daily): the 1-month overnight indexed swap (OIS) rate minus the overnight cash rate at the last close before the decision (+0.25 = a 25 bp rise fully priced). OIS is published from July 2001 to December 2022 only; the 1-month bank bill rate (available to date, but carrying a credit premium) is tested as a stand-in. The ASX's own Rate Tracker history is not freely available |
| Activity and wages | Real GDP growth over the year (ABS national accounts via the ABS Data API, chain volume, seasonally adjusted; from 68 days after the quarter) and Wage Price Index growth over the year (ABS Data API, total hourly rates excluding bonuses, seasonally adjusted; from 50 days after the quarter, data from September 1998) |
| RBA forecast | The year-ahead trimmed mean forecast published in the latest SMP (from May 2000; internal forecasts not published at the time are excluded) |
| Inflation surprise | The latest trimmed mean outcome minus the RBA's forecast for that quarter in the SMP of the same quarter (its nowcast; interpolated between the previous outcome and the next forecast where the archive has only half-yearly targets) |
| SMP text | Latest SMP overview: Claude stance score (-2 to +2) and hawk/dove dictionary index; or dictionary only (hawk/dove index and Loughran-McDonald tone) |
| Minutes text | Latest minutes (published two weeks after each meeting, so the previous meeting's minutes): Claude stance score and hawk/dove index; or the two dictionary hawk/dove indexes only |
| FSR | Latest FSR: Claude risk level (low/moderate/elevated/high) and what its concerns imply for rates (tightening/neutral/easing) |
| Timing rule | A document counts only if published strictly before the decision day, so an SMP released with a decision is not used for it |
| Model | Multinomial logistic regression, standardised inputs, L2 penalty C = 1; refitted at every meeting on all earlier meetings |
| Baselines | Past class frequencies; always hold; repeat the last decision (90% weight) |
| Scores | Log loss and Brier score (lower is better; they reward good probabilities), accuracy, moves called correctly, false alarms (a move predicted at a hold) |
| Significance | Mean per-meeting log-loss gain over the macro model, with a 90% block-bootstrap interval (blocks of 8 meetings) |

No model ever predicted a move in the wrong direction; the hard part is timing.

## Results

### Since 2003 (ABS GDP and wages available; 251 meetings, 61 moves)

| Model | Log loss | Brier | Accuracy | Moves called | False alarms |
|---|---:|---:|---:|---:|---:|
| macro + wages | 0.610 | 0.348 | 77.7% | 16 of 61 | 11 |
| macro | 0.610 | 0.354 | 77.3% | 16 of 61 | 12 |
| macro + GDP + wages | 0.617 | 0.352 | 75.7% | 16 of 61 | 16 |
| macro + GDP | 0.624 | 0.359 | 77.7% | 16 of 61 | 11 |
| past class frequencies | 0.730 | 0.401 | 75.7% | 0 of 61 | 0 |
| repeat last decision | 0.900 | 0.482 | 72.5% | 26 of 61 | 34 |

| Added to macro | Log-loss gain per meeting | 90% interval | Meetings better |
|---|---:|---:|---:|
| wages (WPI) | +0.000 | -0.023 to +0.019 | 76% |
| GDP | -0.014 | -0.027 to -0.000 | 39% |
| GDP + wages | -0.007 | -0.041 to +0.020 | 69% |

**Neither GDP nor wages growth helps.** Once trimmed mean inflation, the trend in unemployment and
the last move are known, GDP growth makes the probabilities slightly worse and wages growth changes
nothing. The same holds since 2010 (below). The RBA's decisions follow inflation and the labour market
much more closely than output, and wages largely move with unemployment.

### Since 2001 (SMP era; 273 meetings tested, 66 moves)

| Model | Log loss | Brier | Accuracy | Moves called | False alarms |
|---|---:|---:|---:|---:|---:|
| macro + SMP dictionary | 0.619 | 0.365 | 74.7% | 17 of 66 | 20 |
| macro | 0.623 | 0.366 | 75.8% | 17 of 66 | 17 |
| macro + SMP text (Claude) | 0.623 | 0.366 | 75.1% | 17 of 66 | 19 |
| past class frequencies | 0.727 | 0.398 | 75.8% | 0 of 66 | 0 |
| repeat last decision | 0.910 | 0.488 | 72.2% | 28 of 66 | 38 |

| Added to macro | Log-loss gain per meeting | 90% interval | Meetings better |
|---|---:|---:|---:|
| SMP text (Claude) | -0.000 | -0.027 to +0.026 | 55% |
| SMP dictionary | +0.004 | -0.017 to +0.039 | 62% |

The macro model is clearly better than the naive baselines (gain over class frequencies +0.104,
90% interval +0.027 to +0.213). **The SMP's tone adds nothing once inflation, unemployment and the
last move are known.** The SMP stance is correlated with the next move (0.51, as reported earlier),
but that information is already in the data.

### Since 2004 (RBA forecasts published; 233 meetings, 59 moves)

| Model | Log loss | Brier | Accuracy | Moves called | False alarms |
|---|---:|---:|---:|---:|---:|
| macro + RBA forecast + SMP text | 0.620 | 0.366 | 75.5% | 20 of 59 | 18 |
| macro + RBA forecast | 0.630 | 0.367 | 77.3% | 19 of 59 | 13 |
| macro + SMP text (Claude) | 0.647 | 0.372 | 76.4% | 19 of 59 | 15 |
| macro | 0.651 | 0.373 | 76.8% | 17 of 59 | 12 |
| macro + inflation surprise | 0.664 | 0.377 | 76.4% | 17 of 59 | 13 |
| past class frequencies | 0.749 | 0.413 | 74.7% | 0 of 59 | 0 |

| Added to macro | Log-loss gain per meeting | 90% interval | Meetings better |
|---|---:|---:|---:|
| RBA forecast | +0.021 | -0.015 to +0.061 | 67% |
| SMP text (Claude) | +0.004 | -0.021 to +0.027 | 60% |
| RBA forecast + SMP text | +0.031 | -0.002 to +0.063 | 67% |
| inflation surprise | -0.013 | -0.025 to +0.000 | 42% |

The RBA's own inflation forecast helps a little, but the gain is not statistically reliable. The
inflation surprise (outcome against the RBA's nowcast) adds nothing: once the level of inflation is
known, whether it beat the RBA's forecast has not helped predict the decision.

### Since 2008 (FSR era; 193 meetings, 51 moves)

| Model | Log loss | Brier | Accuracy | Moves called | False alarms |
|---|---:|---:|---:|---:|---:|
| macro + RBA forecast + SMP text | 0.707 | 0.365 | 76.7% | 15 of 51 | 9 |
| macro + FSR | 0.733 | 0.375 | 75.6% | 15 of 51 | 11 |
| macro | 0.735 | 0.372 | 77.7% | 15 of 51 | 7 |

| Added to macro | Log-loss gain per meeting | 90% interval | Meetings better |
|---|---:|---:|---:|
| RBA forecast + SMP text | +0.028 | -0.030 to +0.072 | 73% |
| FSR | +0.002 | -0.049 to +0.054 | 59% |

**The FSR does not help predict rate decisions.** That fits its purpose: it assesses financial
stability, and the RBA sets the cash rate for inflation and employment, using macroprudential tools
for financial-stability risks.

### Since 2010 (minutes era, every source available; 172 meetings, 39 moves)

| Model | Log loss | Brier | Accuracy | Moves called | False alarms |
|---|---:|---:|---:|---:|---:|
| macro + minutes text (Claude) | **0.500** | 0.312 | 77.9% | 15 of 39 | 14 |
| macro + inflation surprise + minutes text | 0.502 | **0.311** | **80.8%** | **16 of 39** | 10 |
| text only (SMP + minutes) | 0.510 | 0.326 | 74.4% | 9 of 39 | 14 |
| macro + all text, forecast and FSR | 0.535 | 0.319 | 78.5% | 15 of 39 | 13 |
| macro + minutes dictionary | 0.550 | 0.324 | 79.7% | 13 of 39 | 9 |
| macro + SMP text (Claude) | 0.596 | 0.339 | 78.5% | 12 of 39 | 10 |
| macro + FSR | 0.601 | 0.329 | 79.7% | 12 of 39 | 8 |
| macro + GDP + wages + minutes text | 0.515 | 0.318 | 76.7% | 15 of 39 | 16 |
| macro + GDP + wages | 0.608 | 0.331 | 77.9% | 11 of 39 | 10 |
| macro + wages | 0.609 | 0.326 | 79.7% | 12 of 39 | 8 |
| macro + inflation surprise | 0.611 | 0.336 | 80.8% | 12 of 39 | 6 |
| macro | 0.616 | 0.335 | 80.8% | 12 of 39 | 6 |
| macro + RBA forecast | 0.621 | 0.336 | 79.7% | 10 of 39 | 6 |
| macro + GDP | 0.630 | 0.337 | 80.8% | 12 of 39 | 6 |
| past class frequencies | 0.726 | 0.395 | 77.3% | 0 of 39 | 0 |
| repeat last decision | 0.862 | 0.460 | 73.8% | 16 of 39 | 22 |

| Added to macro | Log-loss gain per meeting | 90% interval | Meetings better |
|---|---:|---:|---:|
| minutes text (Claude) | **+0.116** | **+0.050 to +0.177** | 71% |
| inflation surprise + minutes text | **+0.114** | **+0.045 to +0.185** | 73% |
| minutes dictionary | **+0.066** | **+0.029 to +0.101** | 71% |
| all text, forecast and FSR | +0.082 | +0.009 to +0.149 | 73% |
| text only (SMP + minutes) | +0.107 | +0.008 to +0.201 | 60% |
| SMP text (Claude) | +0.021 | -0.014 to +0.055 | 59% |
| FSR | +0.015 | -0.039 to +0.065 | 69% |
| inflation surprise | +0.005 | -0.015 to +0.033 | 58% |
| GDP + wages + minutes text | +0.102 | +0.038 to +0.170 | 74% |
| GDP + wages | +0.009 | -0.017 to +0.046 | 72% |
| wages | +0.008 | -0.012 to +0.039 | 78% |
| GDP | -0.013 | -0.035 to +0.005 | 58% |
| RBA forecast | -0.004 | -0.047 to +0.040 | 74% |

**The minutes are the one source that adds real predictive information.** Across the actual moves,
the macro model gave the move that happened an average probability of 36%; macro + minutes gave
47%. At meetings that held, it was also slightly less likely to expect a move (22% against 24%).

Two checks that the minutes result is genuine:

| Check | Result |
|---|---|
| Hindsight in the LLM labels (Claude knows what the RBA did next) | The dictionary-only minutes measures, which cannot know the future, still improve log loss significantly (+0.066, interval +0.029 to +0.101) |
| Sensitivity to the penalty C (0.1, 0.3, 1, 3, 10) | Minutes text gain +0.091 to +0.118, dictionary gain +0.057 to +0.065, all intervals above zero (`sensitivity_minutes_era.csv`) |

**Better probabilities are mostly not better calls.** The macro model's accuracy (80.8%) is matched
but not beaten. The minutes model catches a few more moves (15 against 12 of 39) at the cost of more
false alarms (14 against 6); adding the inflation surprise to it gives the best balance (16 moves
caught, 10 false alarms, 80.8%). Turning points stay hard. For example, the 29 September 2026 raise followed two
holds, and every model gave it 13-17%.

## Market pricing (the ASX Rate Tracker's information)

### OIS, 2005-2022 (189 meetings, 46 moves)

| Model | Log loss | Brier | Accuracy | Moves called | False alarms |
|---|---:|---:|---:|---:|---:|
| market (OIS) | **0.216** | **0.126** | **89.9%** | 29 of 46 | 2 |
| macro + market | 0.250 | 0.145 | 89.4% | 31 of 46 | 5 |
| macro | 0.629 | 0.353 | 78.8% | 13 of 46 | 7 |
| past class frequencies | 0.735 | 0.402 | 75.7% | 0 of 46 | 0 |

### OIS with the minutes, 2011-2022 (131 meetings, 26 moves)

| Model | Log loss | Brier | Accuracy | Moves called | False alarms |
|---|---:|---:|---:|---:|---:|
| market + minutes text | **0.224** | **0.124** | 90.8% | 14 of 26 | 0 |
| macro + market + minutes text | 0.239 | 0.131 | 90.1% | 15 of 26 | 2 |
| market (OIS) | 0.244 | 0.130 | 90.1% | 13 of 26 | 0 |
| macro + market | 0.249 | 0.128 | 90.8% | 14 of 26 | 0 |
| macro + minutes text | 0.430 | 0.268 | 80.2% | 9 of 26 | 9 |
| macro | 0.539 | 0.280 | 85.5% | 7 of 26 | 0 |

| Compared with the market alone | Log-loss gain per meeting | 90% interval | Meetings better |
|---|---:|---:|---:|
| + minutes text | **+0.020** | **+0.005 to +0.032** | 75% |
| + macro | -0.005 | -0.073 to +0.024 | 79% |
| + macro + minutes text | +0.006 | -0.040 to +0.025 | 76% |
| + macro (2005-2022 sample) | -0.034 | -0.093 to +0.004 | 63% |

**Market pricing beats everything else by a wide margin.** The OIS-implied move correlates 0.92 with
the decision; alone it cuts the macro model's log loss by two-thirds and calls 29 of 46 moves with
only 2 false alarms. Inflation, unemployment and the last move add nothing once market pricing is
known: the market already prices them. **The minutes still add a small, statistically significant
edge on top of the market** (+0.020 per meeting): the market does not fully absorb what the minutes
say.

### Bank bills, 2011-2026 (164 meetings, 38 moves)

| Model | Log loss | Brier | Accuracy | Moves called | False alarms |
|---|---:|---:|---:|---:|---:|
| macro + bank bills + minutes text | **0.448** | **0.266** | 82.3% | 22 of 38 | 13 |
| macro + bank bills | 0.493 | 0.268 | **85.4%** | 20 of 38 | 6 |
| macro + minutes text | 0.499 | 0.311 | 77.4% | 15 of 38 | 14 |
| bank bills | 0.587 | 0.348 | 79.3% | 12 of 38 | 8 |
| macro | 0.620 | 0.335 | 80.5% | 12 of 38 | 6 |

| Added to macro | Log-loss gain per meeting | 90% interval | Meetings better |
|---|---:|---:|---:|
| bank bills | +0.127 | +0.059 to +0.206 | 68% |
| bank bills + minutes text | +0.172 | +0.092 to +0.255 | 70% |
| minutes text | +0.121 | +0.049 to +0.187 | 71% |

The 1-month bank bill rate is a much weaker signal than OIS (correlation with the decision 0.64
against 0.92), because its credit premium moves around. Still, it and the minutes each add
information, and together they score best on this sample, which runs to 2026. Exact ASX futures
data for 2023-2026 would show how close the market alone gets.

## The RBA's forecasts against outcomes (1990-2025)

From the RBA's forecast-by-horizon files (`forecasts_by_horizon`); error = outcome minus forecast,
percentage points.

| Variable | Horizon (quarters) | Forecasts | Mean error | Mean absolute error | RMSE |
|---|---:|---:|---:|---:|---:|
| Trimmed mean inflation (year-ended) | 0 | 135 | 0.00 | 0.19 | 0.25 |
| | 4 | 134 | +0.03 | 0.61 | 0.98 |
| | 8 | 77 | +0.16 | 0.89 | 1.40 |
| Headline CPI (year-ended) | 4 | 138 | -0.22 | 1.11 | 1.53 |
| Unemployment rate | 4 | 136 | -0.22 | 0.57 | 0.85 |
| GDP growth (quarterly) | 4 | 135 | -0.09 | 0.53 | 0.98 |

Year-ahead trimmed mean errors by period: about -0.4 to -0.6 in the 1990s and 2015-19 (inflation
lower than forecast); +1.3 for forecasts made in 2020-24 (the post-COVID surge was under-predicted,
RMSE 2.0).

## The next decision: 3 November 2026

Inputs as of 2 November 2026:

| Input | Value |
|---|---|
| Last decision | Raise, 29 September 2026, to 4.60% (fourth raise of 2026) |
| Trimmed mean inflation, September quarter (released 28 October) | 3.7% year-ended (June 3.6%, March 3.5%); 1.0% in the quarter, the fastest this year |
| Headline CPI, September quarter | 3.9% year-ended |
| Unemployment rate, September | 4.72%, up 0.43 points in six months |
| Real GDP, June quarter (September quarter due 2 December) | 2.1% over the year |
| Wage Price Index, June quarter (September quarter due mid-November) | 3.2% over the year |
| RBA trimmed mean forecasts (August SMP) | 3.3% for December 2026, 3.0% for June 2027, 2.6% for December 2027 |
| September minutes (Claude stance) | +2, strongly hawkish |
| August SMP (Claude stance) | +1, mildly hawkish |
| October FSR | Elevated risk, rising; no clear implication for rates |
| 1-month bank bill rate minus the overnight cash rate (28 October) | +0.14 percentage points (it was +0.10 to +0.18 before each 2026 raise and -0.03 before the holds) |

| Model | Cut | Hold | Raise | Backtest log loss (2010-26) |
|---|---:|---:|---:|---:|
| macro + minutes text (best log loss) | 0% | 49% | 51% | 0.500 |
| macro + inflation surprise + minutes text (best balance of calls) | 0% | 46% | 54% | 0.502 |
| macro + minutes dictionary (no LLM) | 1% | 68% | 32% | 0.550 |
| macro + all text, forecast and FSR | 0% | 36% | 64% | 0.535 |
| macro + inflation surprise | 5% | 83% | 12% | 0.611 |
| macro + wages | 3% | 81% | 17% | 0.609 |
| macro + GDP + wages | 2% | 82% | 16% | 0.608 |
| macro | 6% | 84% | 10% | 0.616 |
| macro + RBA forecast + SMP text | 3% | 87% | 10% | 0.616 (2004-26) |
| macro + bank bills + minutes text (best on the 2011-26 market sample) | 0% | 59% | 41% | 0.448 (2011-26) |
| macro + bank bills | 2% | 87% | 11% | 0.493 (2011-26) |
| bank bills alone | 2% | 88% | 10% | 0.587 (2011-26) |

**Reading:** the models disagree. On data alone (high but stable trimmed mean, rising unemployment) a
hold is likely. The September minutes were strongly hawkish, and the minutes-based models put a
raise at roughly one in three to one in two. The best-scoring model in the backtest calls it close to
a coin flip, with **a hold slightly more likely than not on balance across models**.

The September quarter trimmed mean (3.7%) came in about 0.25 points above the RBA's implied August
nowcast, and above the 3.3% it forecast for December 2026. That looks like news for a raise, but
historically the surprise has added nothing beyond the level of inflation (see above). Including it
moves the probabilities only slightly (raise 10% → 12% on data alone, 51% → 54% with the minutes).

Market pricing is the strongest predictor in the backtests, but the clean measure (OIS) stops in
2022. On the bank bill stand-in, the best-scoring model gives a raise 41%. The bank bill spread of
+0.14 points sits within the +0.10 to +0.18 range seen before each 2026 raise, but bank bills have
over-signalled moves before. The ASX Rate Tracker itself (not reachable from this environment) is the
best single number for this call.

This cannot be called "confidently": in the backtests no specification predicted turning points
reliably, and the models do not see the RBA's November SMP (released with the decision) or speeches
since the minutes.

## Caveat: revised data, not real-time data

The macro inputs are today's revised figures, not the first estimates the Board saw at each meeting.
The publication lags make sure no figure is used before it was released, but the value used is the
latest revision. Unemployment is revised as seasonal factors are re-estimated, GDP growth is often
revised by several tenths of a percentage point, and trimmed mean and wages are revised a little.

| Effect | Direction |
|---|---|
| Macro models are tested with slightly cleaner data than existed in real time | Flatters the macro models |
| Text measures come from documents as published | Unaffected |
| So the gain from the minutes over macro data | If anything understated |
| GDP and wages adding nothing | Unlikely to be caused by revisions: first estimates are noisier, so they would help even less |

Real-time data would remove this: GDP and unemployment as first published, rebuilt from past ABS
releases (the ABS does not publish a vintage database as the US does).

## Files

| File | Contents |
|---|---|
| `decision_panel.csv` | One row per decision since 1990: outcome and every input as known before it |
| `backtest_<sample>.csv` | Out-of-sample probabilities of each model at each tested meeting |
| `scores_<sample>.csv` | The score tables above |
| `sensitivity_minutes_era.csv` | Minutes-era gains for other penalty strengths |
| `next_decision.csv` | Probabilities for 3 November 2026 |
| `results.json` | Everything above in one file |
