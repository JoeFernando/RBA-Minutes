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
