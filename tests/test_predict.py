"""Prediction helpers: decision parsing, as-of data lookups and scoring."""

import datetime as dt
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from rba_minutes import predict


def test_num_handles_signs_and_ranges():
    assert predict._num("+0.25") == 0.25
    assert predict._num("-1.00 to -1.50") == -1.25
    assert predict._num("15.00 to 15.50") == 15.25
    assert predict._num("0.00") == 0


def test_load_decisions(tmp_path: Path):
    html = """<table><tr><th>Effective Date</th><th>Change % points</th><th>Cash rate target %</th></tr>
    <tr><td>30 Sep 2026</td><td>+0.25</td><td>4.60</td></tr>
    <tr><td>12 Aug 2026</td><td>0.00</td><td>4.35</td></tr>
    <tr><td>23 Jan 1990</td><td>-0.50 to -1.00</td><td>17.00 to 17.50</td></tr>
    <tr><td colspan="3">Legend:</td></tr></table>"""
    path = tmp_path / "cash-rate.html"
    path.write_text(html)
    d = predict.load_decisions(path)
    assert d["effective_date"].tolist() == [dt.date(1990, 1, 23), dt.date(2026, 8, 12), dt.date(2026, 9, 30)]
    assert d["decision_date"].iloc[-1] == dt.date(2026, 9, 29)  # the day before it takes effect
    assert d["direction"].tolist() == [-1, 0, 1]
    assert d["cash_rate"].iloc[0] == 17.25


def test_latest_before_respects_publication_lag():
    series = pd.Series([3.5, 3.7], index=[dt.date(2026, 6, 30), dt.date(2026, 9, 30)])
    lag = predict.CPI_PUBLICATION_LAG
    assert predict._latest_before(series, dt.date(2026, 10, 27), lag)[1] == 3.5  # September CPI not out yet
    assert predict._latest_before(series, dt.date(2026, 11, 3), lag)[1] == 3.7


def test_latest_doc_is_strictly_before_the_decision():
    docs = pd.DataFrame({"issue_date": ["2026-05", "2026-08"], "available": [dt.date(2026, 5, 5), dt.date(2026, 8, 11)]})
    assert predict._latest_doc(docs, dt.date(2026, 8, 11))["issue_date"] == "2026-05"  # released with the decision
    assert predict._latest_doc(docs, dt.date(2026, 8, 12))["issue_date"] == "2026-08"


def test_scores():
    y = np.array([0, 1, -1, 0])
    proba = np.array([[0.1, 0.8, 0.1], [0.1, 0.2, 0.7], [0.2, 0.7, 0.1], [0.3, 0.6, 0.1]])
    s = predict._scores(y, proba)
    assert s["accuracy"] == 0.75
    assert s["moves"] == 2 and s["moves_called"] == 1 and s["false_alarms"] == 0 and s["wrong_direction"] == 0
    assert s["log_loss"] == pytest.approx(-np.mean(np.log([0.8, 0.7, 0.2, 0.6])))


def test_backtest_uses_only_earlier_meetings():
    rng = np.random.default_rng(0)
    n = 80
    x = rng.normal(size=n)
    direction = np.where(x > 0.8, 1, np.where(x < -0.8, -1, 0))
    panel = pd.DataFrame({"decision_date": [dt.date(2000, 1, 1) + dt.timedelta(days=30 * i) for i in range(n)],
                          "direction": direction, "previous_direction": np.r_[0, direction[:-1]],
                          "cash_rate": 5.0, "x": x})
    table, preds = predict.backtest(panel, "2000-01-01", {"x": ["x"]}, min_train=40)
    assert len(preds) == n - 40
    assert preds["decision_date"].min() == panel["decision_date"].iloc[40]
    scores = table.set_index("model")
    assert scores.loc["x", "log_loss"] < scores.loc["class_frequencies", "log_loss"]


def test_abs_series_and_growth(tmp_path, monkeypatch):
    gdp = "DATAFLOW,MEASURE,TIME_PERIOD,OBS_VALUE\n" + "\n".join(
        f"ABS:ANA_AGG,M1,{y}-Q{q},{100 + (y - 2024) * 4 + q}" for y in (2024, 2025) for q in (1, 2, 3, 4))
    wpi = "DATAFLOW,MEASURE,TIME_PERIOD,OBS_VALUE\nABS:WPI,3,1997-Q3,\nABS:WPI,3,2025-Q4,3.4\n"
    (tmp_path / "abs_gdp.csv").write_text(gdp)
    (tmp_path / "abs_wpi.csv").write_text(wpi)
    monkeypatch.setattr(predict, "MACRO_DIR", tmp_path)
    growth, wages = predict.load_abs()
    assert growth.index[0] == dt.date(2025, 3, 31)
    assert growth.iloc[0] == pytest.approx((105 / 101 - 1) * 100)  # 2025-Q1 level over 2024-Q1 level
    assert wages.to_dict() == {dt.date(2025, 12, 31): 3.4}  # empty observations dropped
    assert predict._latest_before(growth, dt.date(2025, 6, 8), predict.GDP_PUBLICATION_LAG)[0] == dt.date(2025, 3, 31)
    assert predict._latest_before(growth, dt.date(2025, 6, 7), predict.GDP_PUBLICATION_LAG)[0] is None  # released 7 June: not before that day


def test_market_implied_uses_the_last_close_before_the_decision():
    market = pd.DataFrame({"overnight": [4.35, 4.35, 4.35], "bab_1m": [4.40, 4.45, 4.60],
                           "ois_1m": [4.36, 4.48, np.nan]},
                          index=[dt.date(2026, 9, 25), dt.date(2026, 9, 28), dt.date(2026, 9, 29)])
    out = predict.market_implied(market, dt.date(2026, 9, 29))  # the decision day's own close is not used
    assert out["mkt_ois_change"] == pytest.approx(0.13)
    assert out["mkt_bab_change"] == pytest.approx(0.10)
    stale = predict.market_implied(market, dt.date(2026, 10, 20))  # more than a week after the last data
    assert np.isnan(stale["mkt_ois_change"]) and np.isnan(stale["mkt_bab_change"])


def test_contract_month_from_columns_and_symbols():
    assert predict._contract_month({"contract_month": "2026-11"}) == "2026-11"
    assert predict._contract_month({"contract_month": "202611"}) == "2026-11"
    assert predict._contract_month({"lastTradeDateOrContractMonth": "20261130"}) == "2026-11"
    assert predict._contract_month({"local_symbol": "IBX6"}) == "2026-11"
    assert predict._contract_month({"local_symbol": "IB H8"}) == "2028-03"


def test_futures_implied_follows_the_rate_tracker_method(tmp_path, monkeypatch):
    # Meeting on 3 November (30 days): 3 days at 4.60, then 27 days at the new rate.
    # A fully priced 25 bp rise gives a November average of (3*4.60 + 27*4.85)/30 = 4.825.
    csv = ("trade_date,contract_month,settlement,close\n"
           "2026-11-02,2026-11,95.175,95.17\n2026-11-02,2026-12,95.15,95.15\n"
           "2026-11-03,2026-11,95.10,95.10\n2026-11-03,2026-12,95.15,95.15\n")
    path = tmp_path / "ib.csv"
    path.write_text(csv)
    futures = predict.load_futures(path)
    assert futures["rate"].iloc[0] == pytest.approx(4.825)  # the settlement price is preferred
    overnight = pd.Series([4.60], index=[dt.date(2026, 10, 30)])
    out = predict.futures_implied(futures, overnight, dt.date(2026, 11, 3))
    assert out["price_date"] == dt.date(2026, 11, 2)  # not the decision day's own close
    assert out["futures_method"] == "meeting month"
    assert out["post_meeting_rate"] == pytest.approx(4.85)
    assert out["futures_prob_25bp"] == pytest.approx(1.0)
    # A meeting in the last week of the month reads the next month's contract.
    late = predict.futures_implied(futures, overnight, dt.date(2026, 11, 28))
    assert late["futures_method"] == "next month" and late["post_meeting_rate"] == pytest.approx(4.85)
