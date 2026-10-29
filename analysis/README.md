# Do the RBA's own words help predict its next decision?

Built by `python -m rba_minutes predict --next 2026-11-03` (code: `rba_minutes/predict.py`).
Data to 29 October 2026. All figures below are **out of sample**: for each meeting, every model is
fitted only on earlier meetings and then predicts that meeting.

## What was tested

| Item | Detail |
|---|---|
| Outcome | Every Board decision since January 1990 (402 decisions, from the RBA's cash rate page, which lists unchanged decisions too): cut, hold or raise |
| Macro model | Trimmed mean inflation minus 2.5 (RBA table G1, used from 28 days after the quarter), six-month change in the unemployment rate (H5, from 18 days after the month), direction of the last move |
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
| macro + inflation surprise | 0.611 | 0.336 | 80.8% | 12 of 39 | 6 |
| macro | 0.616 | 0.335 | 80.8% | 12 of 39 | 6 |
| macro + RBA forecast | 0.621 | 0.336 | 79.7% | 10 of 39 | 6 |
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
| RBA trimmed mean forecasts (August SMP) | 3.3% for December 2026, 3.0% for June 2027, 2.6% for December 2027 |
| September minutes (Claude stance) | +2, strongly hawkish |
| August SMP (Claude stance) | +1, mildly hawkish |
| October FSR | Elevated risk, rising; no clear implication for rates |

| Model | Cut | Hold | Raise | Backtest log loss (2010-26) |
|---|---:|---:|---:|---:|
| macro + minutes text (best log loss) | 0% | 49% | 51% | 0.500 |
| macro + inflation surprise + minutes text (best balance of calls) | 0% | 46% | 54% | 0.502 |
| macro + minutes dictionary (no LLM) | 1% | 68% | 32% | 0.550 |
| macro + all text, forecast and FSR | 0% | 36% | 64% | 0.535 |
| macro + inflation surprise | 5% | 83% | 12% | 0.611 |
| macro | 6% | 84% | 10% | 0.616 |
| macro + RBA forecast + SMP text | 3% | 87% | 10% | 0.616 (2004-26) |

**Reading:** the models disagree. On data alone (high but stable trimmed mean, rising unemployment) a
hold is likely. The September minutes were strongly hawkish, and the minutes-based models put a
raise at roughly one in three to one in two. The best-scoring model in the backtest calls it close to
a coin flip, with **a hold slightly more likely than not on balance across models**.

The September quarter trimmed mean (3.7%) came in about 0.25 points above the RBA's implied August
nowcast, and above the 3.3% it forecast for December 2026. That looks like news for a raise, but
historically the surprise has added nothing beyond the level of inflation (see above). Including it
moves the probabilities only slightly (raise 10% → 12% on data alone, 51% → 54% with the minutes).

This cannot be called "confidently": in the backtests no specification predicted turning points
reliably. The models do not see market pricing, the RBA's November SMP (released with the decision),
or speeches since the minutes. Those are the next things to add.

## Files

| File | Contents |
|---|---|
| `decision_panel.csv` | One row per decision since 1990: outcome and every input as known before it |
| `backtest_<sample>.csv` | Out-of-sample probabilities of each model at each tested meeting |
| `scores_<sample>.csv` | The score tables above |
| `sensitivity_minutes_era.csv` | Minutes-era gains for other penalty strengths |
| `next_decision.csv` | Probabilities for 3 November 2026 |
| `results.json` | Everything above in one file |
