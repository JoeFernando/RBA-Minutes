"""Do the RBA's own words help predict its next cash rate decision?

Every Board decision since 1990 (the RBA's cash rate page lists unchanged
decisions too) is matched with what was public before it:

- macro data: latest trimmed mean inflation (RBA table G1, available about
  four weeks after the quarter) and the unemployment rate (H5, about 18 days
  after the month), and the direction of the last move
- the RBA's year-ahead trimmed mean forecast from the latest SMP
- the latest SMP's stance (Claude label, -2..+2) and hawk/dove dictionary index
- the latest minutes' stance and hawk/dove index (minutes are published two
  weeks after each meeting, so the previous meeting's minutes are known)
- the latest FSR's risk level and what its concerns imply for rates

A document counts only if it was published strictly before the decision date
(the day before the 'effective date' on the RBA's page), so an SMP released
with a decision is not used for that decision.

Models are compared out of sample: for each meeting, a model is fitted on all
earlier meetings and predicts this one (an expanding window). Scores are
accuracy, multiclass log loss and Brier score against simple baselines
(always hold; repeat the last decision; past class frequencies).
"""

import datetime as dt
import json
import re
import sqlite3
from pathlib import Path

import numpy as np
import pandas as pd
from bs4 import BeautifulSoup

from . import config

MACRO_DIR = config.DATA_DIR / "macro"
OUT_DIR = config.PROJECT_ROOT / "analysis"
CLASSES = np.array([-1, 0, 1])  # cut, hold, raise
TARGET_MIDPOINT = 2.5

MINUTES_PUBLICATION_LAG = dt.timedelta(days=14)
CPI_PUBLICATION_LAG = dt.timedelta(days=28)
LABOUR_PUBLICATION_LAG = dt.timedelta(days=18)


# --- Data ---------------------------------------------------------------------------

def _num(text: str) -> float:
    """'0.25', '+0.25', '-1.00 to -1.50' (a range: its midpoint), '15.00 to 15.50'."""
    parts = [float(x.replace("+", "")) for x in re.findall(r"[-+]?\d+(?:\.\d+)?", text)]
    return sum(parts) / len(parts)


def load_decisions(path: Path = MACRO_DIR / "cash-rate.html") -> pd.DataFrame:
    """Every Board decision since 1990 from the RBA's cash rate page, oldest first."""
    soup = BeautifulSoup(path.read_bytes(), "lxml")
    rows = []
    for tr in soup.find("table").find_all("tr"):
        cells = [c.get_text(" ", strip=True) for c in tr.find_all(["th", "td"])]
        if len(cells) < 3 or not re.match(r"\d{1,2} \w{3} \d{4}$", cells[0]):
            continue
        effective = dt.datetime.strptime(cells[0], "%d %b %Y").date()
        change = _num(cells[1])
        rows.append({"effective_date": effective, "decision_date": effective - dt.timedelta(days=1),
                     "change": change, "cash_rate": _num(cells[2]),
                     "direction": int(np.sign(round(change, 4)))})
    return pd.DataFrame(rows).sort_values("effective_date").reset_index(drop=True)


def _rba_table(path: Path) -> tuple[pd.DataFrame, dict]:
    raw = pd.read_excel(path, header=None)
    ids = raw.iloc[10].tolist()
    titles = dict(zip(ids, raw.iloc[1].tolist()))
    data = raw.iloc[11:].copy()
    data.columns = ids
    data = data.rename(columns={ids[0]: "date"})
    data["date"] = pd.to_datetime(data["date"]).dt.date
    return data.set_index("date").apply(pd.to_numeric, errors="coerce"), titles


def load_macro() -> tuple[pd.Series, pd.Series]:
    """(trimmed mean year-ended inflation by quarter end, unemployment rate by month end)."""
    g1, _ = _rba_table(MACRO_DIR / "g01hist.xlsx")
    h5, _ = _rba_table(MACRO_DIR / "h05hist.xlsx")
    return g1["GCPIOCPMTMYP"].dropna(), h5["GLFSURSA"].dropna()


def _latest_before(series: pd.Series, when: dt.date, lag: dt.timedelta):
    known = series[[d + lag < when for d in series.index]]
    return (known.index[-1], known.iloc[-1]) if len(known) else (None, np.nan)


def load_rba_forecasts(smp_db: Path = config.SMP_DB_PATH) -> pd.DataFrame:
    """The RBA's year-ahead trimmed mean forecast at each SMP: (forecast_month, value)."""
    conn = sqlite3.connect(smp_db)
    hist = pd.read_sql("SELECT forecast_month, horizon, forecast FROM forecasts_by_horizon "
                       "WHERE variable = 'underlying_ye' AND horizon BETWEEN 3 AND 5", conn)
    arch = pd.read_sql("SELECT smp_date AS forecast_month, horizon, value AS forecast FROM smp_forecast_archive "
                       "WHERE variable_code = 'underlying_ye' AND horizon BETWEEN 3 AND 5", conn)
    out = {}
    for frame in (hist, arch):  # the archive (from 2018-11) fills the one-year lag in the historical file
        for month, group in frame.groupby("forecast_month"):
            exact = group[group["horizon"] == 4]["forecast"]
            out.setdefault(month, float(exact.iloc[0]) if len(exact) else float(group["forecast"].mean()))
    return pd.Series(out).sort_index()


def load_documents() -> dict[str, pd.DataFrame]:
    """Text features with the date each document became public."""
    minutes = pd.read_sql(
        "SELECT meeting_date, llm_stance_score, hawk_dove_index, policy_hawk_dove_index, lm_tone "
        "FROM v_meeting_sentiment", sqlite3.connect(config.DB_PATH))
    minutes["available"] = pd.to_datetime(minutes["meeting_date"]).dt.date + MINUTES_PUBLICATION_LAG
    smp = pd.read_sql(
        "SELECT issue_date, published_date, llm_stance_score, hawk_dove_index, lm_tone FROM v_smp_sentiment",
        sqlite3.connect(config.SMP_DB_PATH))
    smp["available"] = pd.to_datetime(smp["published_date"]).dt.date
    fsr = pd.read_sql(
        "SELECT issue_date, published_date, llm_risk_level_ordinal, llm_risk_trend, llm_policy_implication, "
        "lm_tone FROM v_fsr_sentiment", sqlite3.connect(config.FSR_DB_PATH))
    fsr["available"] = pd.to_datetime(fsr["published_date"]).dt.date
    fsr["fsr_policy"] = fsr["llm_policy_implication"].map({"tightening": 1, "neutral": 0, "easing": -1})
    fsr["fsr_trend"] = fsr["llm_risk_trend"].map({"rising": 1, "stable": 0, "easing": -1})
    return {"minutes": minutes, "smp": smp, "fsr": fsr}


def _latest_doc(frame: pd.DataFrame, when: dt.date):
    known = frame[frame["available"] < when]
    return known.iloc[-1] if len(known) else None


def build_panel() -> pd.DataFrame:
    """One row per decision: the outcome and everything public before it."""
    decisions = load_decisions()
    tm, ur = load_macro()
    forecasts = load_rba_forecasts()
    smp_dates = load_documents()["smp"].set_index("issue_date")["available"]
    docs = load_documents()
    rows = []
    for i, d in decisions.iterrows():
        when = d["decision_date"]
        past = decisions.iloc[:i]
        moves = past[past["direction"] != 0]
        recent = past[past["decision_date"] > when - dt.timedelta(days=183)]
        tm_date, tm_value = _latest_before(tm, when, CPI_PUBLICATION_LAG)
        ur_date, ur_value = _latest_before(ur, when, LABOUR_PUBLICATION_LAG)
        ur_6m = ur[[x <= ur_date - dt.timedelta(days=170) for x in ur.index]] if ur_date else pd.Series(dtype=float)
        row = {
            "decision_date": when, "change": d["change"], "cash_rate": d["cash_rate"], "direction": d["direction"],
            "previous_direction": past["direction"].iloc[-1] if len(past) else 0,
            "last_move": moves["direction"].iloc[-1] if len(moves) else 0,
            "moves_6m": int(recent["direction"].sum()),
            "tm_yoy": tm_value, "tm_quarter": tm_date,
            "tm_gap": tm_value - TARGET_MIDPOINT,
            "unemployment": ur_value,
            "unemployment_change_6m": ur_value - ur_6m.iloc[-1] if len(ur_6m) else np.nan,
        }
        # the RBA's forecast from the latest SMP published before the decision
        known_smp = [k for k, v in smp_dates.items() if v < when]
        month = known_smp[-1] if known_smp else None
        row["rba_tm_forecast_gap"] = forecasts.get(month, np.nan) - TARGET_MIDPOINT if month else np.nan
        for name, cols in (("smp", ["llm_stance_score", "hawk_dove_index", "lm_tone"]),
                           ("minutes", ["llm_stance_score", "hawk_dove_index", "policy_hawk_dove_index", "lm_tone"]),
                           ("fsr", ["llm_risk_level_ordinal", "fsr_policy", "fsr_trend", "lm_tone"])):
            doc = _latest_doc(docs[name], when)
            for col in cols:
                row[f"{name}_{col}".replace("fsr_fsr_", "fsr_")] = doc[col] if doc is not None else np.nan
            row[f"{name}_source"] = (doc.get("issue_date") or doc.get("meeting_date")) if doc is not None else None
        rows.append(row)
    return pd.DataFrame(rows)


# --- Out-of-sample evaluation --------------------------------------------------------------

MODELS = {
    "macro": ["tm_gap", "unemployment_change_6m", "last_move"],
    "macro+rba_forecast": ["tm_gap", "unemployment_change_6m", "last_move", "rba_tm_forecast_gap"],
    "macro+smp_text": ["tm_gap", "unemployment_change_6m", "last_move", "smp_llm_stance_score", "smp_hawk_dove_index"],
    "macro+rba_forecast+smp_text": ["tm_gap", "unemployment_change_6m", "last_move", "rba_tm_forecast_gap",
                                    "smp_llm_stance_score", "smp_hawk_dove_index"],
    "macro+minutes_text": ["tm_gap", "unemployment_change_6m", "last_move", "minutes_llm_stance_score",
                           "minutes_hawk_dove_index"],
    "macro+fsr": ["tm_gap", "unemployment_change_6m", "last_move", "fsr_llm_risk_level_ordinal", "fsr_policy"],
    "macro+all_text": ["tm_gap", "unemployment_change_6m", "last_move", "rba_tm_forecast_gap",
                       "smp_llm_stance_score", "smp_hawk_dove_index", "minutes_llm_stance_score",
                       "minutes_hawk_dove_index", "fsr_llm_risk_level_ordinal", "fsr_policy"],
    "text_only": ["smp_llm_stance_score", "minutes_llm_stance_score", "minutes_hawk_dove_index"],
}


def _fit_predict(train: pd.DataFrame, test: pd.DataFrame, features: list[str], C: float = 1.0) -> np.ndarray:
    from sklearn.linear_model import LogisticRegression
    from sklearn.pipeline import make_pipeline
    from sklearn.preprocessing import StandardScaler

    y = train["direction"].to_numpy()
    model = make_pipeline(StandardScaler(), LogisticRegression(C=C, max_iter=2000))
    model.fit(train[features].to_numpy(dtype=float), y)
    proba = np.zeros((len(test), 3))
    classes = model.classes_
    p = model.predict_proba(test[features].to_numpy(dtype=float))
    for j, c in enumerate(classes):
        proba[:, list(CLASSES).index(c)] = p[:, j]
    return proba


def _scores(y: np.ndarray, proba: np.ndarray) -> dict:
    idx = np.searchsorted(CLASSES, y)
    eps = 1e-6
    p_true = np.clip(proba[np.arange(len(y)), idx], eps, 1)
    pred = CLASSES[proba.argmax(axis=1)]
    onehot = np.eye(3)[idx]
    moves = y != 0
    return {
        "n": int(len(y)),
        "accuracy": float((pred == y).mean()),
        "log_loss": float(-np.log(p_true).mean()),
        "brier": float(((proba - onehot) ** 2).sum(axis=1).mean()),
        "moves": int(moves.sum()),
        "moves_called": int(((pred == y) & moves).sum()),
        "false_alarms": int(((pred != 0) & ~moves).sum()),
        "wrong_direction": int(((pred != 0) & moves & (pred != y)).sum()),
    }


def backtest(panel: pd.DataFrame, start: str, models: dict = MODELS, min_train: int = 48,
             C: float = 1.0) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Expanding-window predictions for every meeting from `start` whose features are all known.

    The sample is the same for every model (rows where all features of all
    listed models are present), so the scores are comparable.
    """
    needed = sorted({f for feats in models.values() for f in feats})
    data = panel[panel["decision_date"] >= dt.date.fromisoformat(start)].dropna(subset=needed).reset_index(drop=True)
    preds = {name: np.full((len(data), 3), np.nan) for name in models}
    preds["always_hold"] = np.tile([0.0, 1.0, 0.0], (len(data), 1))
    preds["repeat_last_decision"] = np.zeros((len(data), 3))
    preds["class_frequencies"] = np.full((len(data), 3), np.nan)
    for t in range(len(data)):
        prev = data["previous_direction"].iloc[t]
        smooth = np.full(3, 0.05)
        smooth[list(CLASSES).index(prev)] = 0.90
        preds["repeat_last_decision"][t] = smooth
        train = data.iloc[:t]
        if t < min_train:
            continue
        freq = np.array([(train["direction"] == c).sum() + 1 for c in CLASSES], dtype=float)
        preds["class_frequencies"][t] = freq / freq.sum()
        if train["direction"].nunique() < 2:
            continue
        for name, feats in models.items():
            preds[name][t] = _fit_predict(train, data.iloc[[t]], feats, C)[0]
    tested = ~np.isnan(preds["class_frequencies"]).any(axis=1)
    y = data["direction"].to_numpy()[tested]
    table = []
    for name, proba in preds.items():
        p = proba[tested]
        if np.isnan(p).any():
            continue
        table.append({"model": name, **_scores(y, p)})
    out = data.loc[tested, ["decision_date", "direction", "cash_rate"]].copy()
    for name, proba in preds.items():
        p = proba[tested]
        out[f"{name}_p_cut"], out[f"{name}_p_hold"], out[f"{name}_p_raise"] = p[:, 0], p[:, 1], p[:, 2]
    return pd.DataFrame(table), out


def paired_log_loss(preds: pd.DataFrame, a: str, b: str, block: int = 8, reps: int = 4000, seed: int = 0) -> dict:
    """Mean per-meeting log-loss improvement of model b over model a, with a block-bootstrap 90% interval."""
    y = preds["direction"].to_numpy()
    idx = np.searchsorted(CLASSES, y)

    def loss(name):
        p = preds[[f"{name}_p_cut", f"{name}_p_hold", f"{name}_p_raise"]].to_numpy()
        return -np.log(np.clip(p[np.arange(len(y)), idx], 1e-6, 1))
    diff = loss(a) - loss(b)
    rng = np.random.default_rng(seed)
    n = len(diff)
    starts = np.arange(0, n - block + 1)
    means = []
    for _ in range(reps):
        picks = rng.choice(starts, size=int(np.ceil(n / block)))
        sample = np.concatenate([diff[s:s + block] for s in picks])[:n]
        means.append(sample.mean())
    lo, hi = np.percentile(means, [5, 95])
    return {"baseline": a, "model": b, "mean_improvement": float(diff.mean()), "ci90_low": float(lo),
            "ci90_high": float(hi), "share_better": float((diff > 0).mean())}


# --- The next decision ------------------------------------------------------------------------

def features_as_of(panel: pd.DataFrame, when: dt.date) -> pd.Series:
    """The feature row for a decision that has not happened yet."""
    decisions = load_decisions()
    tm, ur = load_macro()
    forecasts = load_rba_forecasts()
    docs = load_documents()
    past = decisions[decisions["decision_date"] < when]
    moves = past[past["direction"] != 0]
    recent = past[past["decision_date"] > when - dt.timedelta(days=183)]
    tm_date, tm_value = _latest_before(tm, when, CPI_PUBLICATION_LAG)
    ur_date, ur_value = _latest_before(ur, when, LABOUR_PUBLICATION_LAG)
    ur_6m = ur[[x <= ur_date - dt.timedelta(days=170) for x in ur.index]]
    row = {"decision_date": when, "previous_direction": past["direction"].iloc[-1],
           "last_move": moves["direction"].iloc[-1], "moves_6m": int(recent["direction"].sum()),
           "tm_yoy": tm_value, "tm_quarter": tm_date, "tm_gap": tm_value - TARGET_MIDPOINT,
           "unemployment": ur_value, "unemployment_change_6m": ur_value - ur_6m.iloc[-1]}
    smp = _latest_doc(docs["smp"], when)
    row["rba_tm_forecast_gap"] = forecasts.get(smp["issue_date"], np.nan) - TARGET_MIDPOINT
    for name, cols in (("smp", ["llm_stance_score", "hawk_dove_index", "lm_tone"]),
                       ("minutes", ["llm_stance_score", "hawk_dove_index", "policy_hawk_dove_index", "lm_tone"]),
                       ("fsr", ["llm_risk_level_ordinal", "fsr_policy", "fsr_trend", "lm_tone"])):
        doc = _latest_doc(docs[name], when)
        for col in cols:
            row[f"{name}_{col}".replace("fsr_fsr_", "fsr_")] = doc[col]
        row[f"{name}_source"] = doc.get("issue_date") or doc.get("meeting_date")
    return pd.Series(row)


def forecast_next(panel: pd.DataFrame, when: dt.date, models: dict = MODELS, C: float = 1.0) -> pd.DataFrame:
    """Probabilities of a cut / hold / raise at a future decision, from each model fitted on all past decisions."""
    row = features_as_of(panel, when)
    out = []
    for name, feats in models.items():
        train = panel.dropna(subset=feats)
        p = _fit_predict(train, pd.DataFrame([row]), feats, C)[0]
        out.append({"model": name, "trained_on": len(train), "p_cut": p[0], "p_hold": p[1], "p_raise": p[2]})
    return pd.DataFrame(out), row


def run(next_decision: dt.date | None = None) -> dict:
    """Build the panel, run the backtests for each sample period and write the outputs to analysis/."""
    OUT_DIR.mkdir(exist_ok=True)
    panel = build_panel()
    panel.to_csv(OUT_DIR / "decision_panel.csv", index=False)
    results = {}
    samples = {
        # inflation targeting, SMP text and RBA forecasts available (first SMP Feb 1997)
        "smp_era": ("1997-03-01", {k: MODELS[k] for k in ("macro", "macro+rba_forecast", "macro+smp_text",
                                                          "macro+rba_forecast+smp_text")}),
        # FSR from March 2004
        "fsr_era": ("2004-04-01", {k: MODELS[k] for k in ("macro", "macro+rba_forecast+smp_text", "macro+fsr")}),
        # minutes from October 2006: every source available
        "minutes_era": ("2006-11-01", {k: MODELS[k] for k in ("macro", "macro+rba_forecast", "macro+smp_text",
                                                              "macro+minutes_text", "macro+fsr", "macro+all_text",
                                                              "text_only")}),
    }
    for name, (start, models) in samples.items():
        min_train = 48 if name != "minutes_era" else 40
        table, preds = backtest(panel, start, models, min_train=min_train)
        preds.to_csv(OUT_DIR / f"backtest_{name}.csv", index=False)
        table.to_csv(OUT_DIR / f"scores_{name}.csv", index=False)
        tests = [paired_log_loss(preds, "macro", m) for m in models if m != "macro"]
        tests += [paired_log_loss(preds, "class_frequencies", "macro")]
        results[name] = {"start": start, "first_test": str(preds["decision_date"].min()),
                         "last_test": str(preds["decision_date"].max()),
                         "scores": table.to_dict("records"), "paired": tests}
    if next_decision:
        forecast, row = forecast_next(panel, next_decision)
        forecast.to_csv(OUT_DIR / "next_decision.csv", index=False)
        results["next_decision"] = {"date": str(next_decision), "features": {k: (str(v) if isinstance(v, (dt.date,)) else v)
                                                                              for k, v in row.items()},
                                    "probabilities": forecast.to_dict("records")}
    (OUT_DIR / "results.json").write_text(json.dumps(results, indent=2, default=str))
    return results
