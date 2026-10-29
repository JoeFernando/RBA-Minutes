"""Do the RBA's own words help predict its next cash rate decision?

Every Board decision since 1990 (the RBA's cash rate page lists unchanged
decisions too) is matched with what was public before it:

- macro data: latest trimmed mean inflation (ABS, via RBA table G1; available
  about four weeks after the quarter), the unemployment rate (ABS Labour Force,
  via RBA table H5; about 18 days after the month), and the direction of the
  last move; for testing, also real GDP growth (ABS national accounts, about
  68 days after the quarter) and Wage Price Index growth (ABS, about 50 days)
- the RBA's year-ahead trimmed mean forecast from the latest SMP
- the latest SMP's stance (Claude label, -2..+2) and hawk/dove dictionary index
- the latest minutes' stance and hawk/dove index (minutes are published two
  weeks after each meeting, so the previous meeting's minutes are known)
- the latest FSR's risk level and what its concerns imply for rates

Macro data are today's revised figures, not the first estimates the Board saw
at the time (see analysis/README.md).

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
GDP_PUBLICATION_LAG = dt.timedelta(days=68)   # national accounts: first week of the third month
WPI_PUBLICATION_LAG = dt.timedelta(days=50)   # Wage Price Index: middle of the second month

ABS_API = "https://data.api.abs.gov.au/rest/data"
MACRO_SOURCES = {
    "cash-rate.html": f"{config.BASE_URL}/statistics/cash-rate/",
    "g01hist.xlsx": f"{config.BASE_URL}/statistics/tables/xls/g01hist.xlsx",   # CPI (ABS)
    "h05hist.xlsx": f"{config.BASE_URL}/statistics/tables/xls/h05hist.xlsx",   # Labour force (ABS)
    # ABS Data API: real GDP, chain volume, seasonally adjusted ($m)
    "abs_gdp.csv": f"{ABS_API}/ABS,ANA_AGG/M1.GPM.20.AUS.Q",
    # ABS Data API: Wage Price Index, total hourly rates of pay excluding bonuses, private and public,
    # all industries, seasonally adjusted, % change from a year earlier
    "abs_wpi.csv": f"{ABS_API}/ABS,WPI/3.THRPEB.7.TOT.20.AUS.Q",
    # RBA table F1, daily: cash rate, overnight interbank rate, 1-month bank bills and OIS
    "f01dhist.xls": f"{config.BASE_URL}/statistics/tables/xls-hist/f01dhist.xls",   # to 2010
    "f01d.xlsx": f"{config.BASE_URL}/statistics/tables/xls/f01d.xlsx",              # 2011 onwards
}


def fetch_macro(out_dir: Path = MACRO_DIR) -> list[Path]:
    """Download the cash rate history, RBA tables G1, H5 and F1, ABS GDP and WPI, and today's ASX Rate Tracker."""
    import time

    from .scrape import make_session

    session = make_session()
    out_dir.mkdir(parents=True, exist_ok=True)
    saved = []
    for name, url in MACRO_SOURCES.items():
        headers = {"Accept": "application/vnd.sdmx.data+csv"} if url.startswith(ABS_API) else {}
        response = session.get(url, headers=headers, timeout=60)
        response.raise_for_status()
        (out_dir / name).write_bytes(response.content)
        saved.append(out_dir / name)
        time.sleep(config.REQUEST_DELAY_SECONDS)
    (out_dir / "downloaded_at.txt").write_text(dt.datetime.now(dt.timezone.utc).isoformat() + "\n")
    saved.append(fetch_asx(out_dir / "asx"))
    return saved


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


def _abs_series(path: Path) -> pd.Series:
    """An ABS Data API CSV (one series) as values by quarter-end date."""
    frame = pd.read_csv(path)
    frame = frame[frame["OBS_VALUE"].notna()]
    dates = [(pd.Period(t.replace("-Q", "Q"), freq="Q").end_time.date()) for t in frame["TIME_PERIOD"]]
    return pd.Series(frame["OBS_VALUE"].astype(float).to_numpy(), index=dates).sort_index()


def load_abs() -> tuple[pd.Series, pd.Series]:
    """(real GDP growth over the year, Wage Price Index growth over the year), by quarter end."""
    gdp = _abs_series(MACRO_DIR / "abs_gdp.csv")
    gdp_growth = (gdp / gdp.shift(4) - 1).dropna() * 100
    return gdp_growth, _abs_series(MACRO_DIR / "abs_wpi.csv")


def load_market() -> pd.DataFrame:
    """Daily money-market rates (RBA table F1): overnight interbank rate, 1-month bank bills, 1-month OIS.

    OIS rates were published from July 2001 to December 2022; bank bill rates throughout.
    """
    frames = []
    for name in ("f01dhist.xls", "f01d.xlsx"):
        raw = pd.read_excel(MACRO_DIR / name, header=None)
        ids = raw.iloc[10].tolist()
        data = raw.iloc[11:].copy()
        data.columns = ids
        data = data.rename(columns={ids[0]: "date"})
        data["date"] = pd.to_datetime(data["date"]).dt.date
        frames.append(data.set_index("date")[["FIRMMCRID", "FIRMMBAB30D", "FIRMMOIS1D"]].apply(pd.to_numeric, errors="coerce"))
    market = pd.concat(frames)
    market = market[~market.index.duplicated(keep="last")].sort_index()
    return market.rename(columns={"FIRMMCRID": "overnight", "FIRMMBAB30D": "bab_1m", "FIRMMOIS1D": "ois_1m"})


def market_implied(market: pd.DataFrame, when: dt.date) -> dict:
    """The market's priced move for a decision, like the ASX Rate Tracker, from the last close before it.

    1-month OIS (or bank bill) rate minus the overnight cash rate that day: about +0.25 when a 25 basis
    point rise is fully priced.
    """
    before = market[market.index < when]
    ois = before.dropna(subset=["ois_1m", "overnight"])
    bab = before.dropna(subset=["bab_1m", "overnight"])
    out = {"mkt_ois_change": np.nan, "mkt_bab_change": np.nan}
    if len(ois) and (when - ois.index[-1]).days <= 7:
        out["mkt_ois_change"] = ois["ois_1m"].iloc[-1] - ois["overnight"].iloc[-1]
    if len(bab) and (when - bab.index[-1]).days <= 7:
        out["mkt_bab_change"] = bab["bab_1m"].iloc[-1] - bab["overnight"].iloc[-1]
    return out


# --- ASX 30 Day Interbank Cash Rate Futures (the ASX RBA Rate Tracker's inputs) ----------------

FUTURES_FILE = MACRO_DIR / "ib_futures_ibkr.csv"
MONTH_CODES = dict(zip("FGHJKMNQUVXZ", range(1, 13)))
LAST_WEEK = 7  # a meeting this close to month end: read the new rate from the next month's contract


def _contract_month(row) -> str | None:
    """'YYYY-MM' from a contract_month column ('2026-11', '202611', 'Nov 2026') or an ASX/IBKR
    local symbol ('IBX6', 'IB X6', 'IBX26')."""
    for key in ("contract_month", "expiry", "last_trade_date", "lastTradeDateOrContractMonth"):
        value = str(row.get(key, "") or "").strip()
        if not value or value == "nan":
            continue
        match = re.match(r"^(\d{4})-?(\d{2})", value)
        if match:
            return f"{match.group(1)}-{match.group(2)}"
        try:
            return pd.Timestamp(value).strftime("%Y-%m")
        except (ValueError, TypeError):
            pass
    symbol = str(row.get("local_symbol", "") or "").replace(" ", "").upper()
    match = re.match(r"^IB([FGHJKMNQUVXZ])(\d{1,2})$", symbol)
    if match:
        year = int(match.group(2))
        year = 2020 + year if year < 10 else 2000 + year
        return f"{year}-{MONTH_CODES[match.group(1)]:02d}"
    return None


def load_futures(path: Path = FUTURES_FILE) -> pd.DataFrame:
    """Daily implied average cash rate for each contract month: (trade_date, contract_month, rate).

    The price is 100 minus the month's average overnight cash rate; a settlement price is used where
    given, else the close (or an implied_rate column).
    """
    raw = pd.read_csv(path)
    raw.columns = [c.strip().lower() for c in raw.columns]
    date_col = next(c for c in ("trade_date", "date", "datetime", "time") if c in raw.columns)
    out = pd.DataFrame({"trade_date": pd.to_datetime(raw[date_col]).dt.date,
                        "contract_month": [_contract_month(r) for r in raw.to_dict("records")]})
    price = pd.Series(np.nan, index=raw.index)
    for col in ("settlement", "settle", "close", "last"):
        if col in raw.columns:
            price = price.fillna(pd.to_numeric(raw[col], errors="coerce"))
    rate = 100 - price
    if "implied_rate" in raw.columns:
        rate = rate.fillna(pd.to_numeric(raw["implied_rate"], errors="coerce"))
    out["rate"] = rate
    # Where a day has both a traded price and a bid/ask midpoint, the traded price wins.
    out["source"] = raw["what_to_show"].astype(str).str.upper() if "what_to_show" in raw.columns else "TRADES"
    out["_rank"] = out["source"].map({"TRADES": 0}).fillna(1)
    out = out.dropna(subset=["contract_month", "rate"])
    out = out[(out["rate"] > -1) & (out["rate"] < 20)]  # drop obviously bad prices
    out = out.sort_values(["trade_date", "contract_month", "_rank"]).drop_duplicates(["trade_date", "contract_month"])
    return out.drop(columns="_rank").reset_index(drop=True)


def futures_implied(futures: pd.DataFrame, overnight: pd.Series, decision: dt.date) -> dict:
    """The move priced for a decision at the last close before it, the way the ASX Rate Tracker works it out."""
    before = futures[futures["trade_date"] < decision]
    if before.empty:
        return {}
    day = before["trade_date"].max()
    curve = before[before["trade_date"] == day].set_index("contract_month")["rate"]
    known = overnight[[d <= day for d in overnight.index]].dropna()
    if known.empty:
        return {}
    current = float(known.iloc[-1])
    month = f"{decision.year}-{decision.month:02d}"
    following = (pd.Timestamp(decision) + pd.offsets.MonthBegin(1)).strftime("%Y-%m")
    days = pd.Timestamp(decision).days_in_month
    old_days = decision.day  # the new rate takes effect the day after the decision
    if days - old_days >= LAST_WEEK and month in curve:
        post = (days * curve[month] - old_days * current) / (days - old_days)
        method = "meeting month"
    elif following in curve:
        post = float(curve[following])
        method = "next month"
    else:
        return {}
    change = post - current
    used = [m for m in (month, following) if m in curve.index][:1 if method == "meeting month" else 2][-1:]
    source = before[(before["trade_date"] == day) & before["contract_month"].isin(used)]["source"]
    return {"price_date": day, "current_rate": current, "post_meeting_rate": post, "futures_change": change,
            "futures_prob_25bp": change / 0.25, "futures_method": method,
            "price_source": source.iloc[0] if len(source) else ""}


ASX_DATA = "https://www.asx.com.au/content/dam/asx/data/"
ASX_FILES = ("dynamic_text.csv", "market_exp.csv", "yield_curve.csv")  # the files behind the RBA Rate Tracker page
ASX_DIR = MACRO_DIR / "asx"


def fetch_asx(out_dir: Path = ASX_DIR) -> Path:
    """Save today's ASX RBA Rate Tracker files under asx/<settlement date>/ (ASX replaces them daily)."""
    from .scrape import make_session

    session = make_session()
    contents = {}
    for name in ASX_FILES:
        response = session.get(ASX_DATA + name, timeout=60)
        response.raise_for_status()
        contents[name] = response.content
    date = json.loads(contents["dynamic_text.csv"].decode("utf-8").strip())["Crnt_Stlmnt_Dt"]
    folder = out_dir / date
    folder.mkdir(parents=True, exist_ok=True)
    for name, content in contents.items():
        (folder / name).write_bytes(content)
    return folder


def load_asx(folder: Path | None = None) -> dict | None:
    """The latest saved ASX RBA Rate Tracker: next meeting, probability of a move, recent history, futures curve."""
    folders = sorted(p for p in ASX_DIR.glob("*") if (p / "dynamic_text.csv").exists()) if folder is None else [folder]
    if not folders:
        return None
    folder = folders[-1]
    read = lambda name: json.loads((folder / name).read_text().strip())
    text, expectations, curve = read("dynamic_text.csv"), read("market_exp.csv"), read("yield_curve.csv")
    settlement = dt.date.fromisoformat(curve["Crnt_Stlmnt_Dt"])
    futures = pd.DataFrame([{"trade_date": settlement, "contract_month": pd.Timestamp("01-" + m["Expiry_Month"]).strftime("%Y-%m"),
                             "rate": m["Implied_Yield"], "source": "SETTLEMENT"} for m in curve["months"]])
    history = pd.DataFrame(expectations["days"]).rename(columns={"Stlmnt_Dt": "settlement_date"})
    return {
        "folder": str(folder.relative_to(config.PROJECT_ROOT)), "settlement_date": settlement,
        "next_meeting": dt.date.fromisoformat(text["Nxt_RBA_Mtng_Dt"]), "cash_rate_target": text["RBA_Trgt_Cash_Rate"],
        "expected_rate": text["Ftre_Cash_Rate"], "expected_change": text["Ftre_Cash_Rate_Change"],
        "probability": text["Mkt_Exptn_Rate"] / 100, "history": history.sort_values("settlement_date"),
        "futures": futures,
    }


def futures_check(next_decision: dt.date | None = None) -> pd.DataFrame:
    """Each decision the futures file covers: the move priced the day before, the bank bill stand-in and the outcome."""
    futures = load_futures()
    market = load_market()
    overnight = market["overnight"].dropna()
    decisions = load_decisions()
    rows = []
    first = futures["trade_date"].min()
    for d in decisions.itertuples():
        if d.decision_date <= first:
            continue
        implied = futures_implied(futures, overnight, d.decision_date)
        if implied:
            rows.append({"decision_date": d.decision_date, "actual_change": d.change, **implied,
                         **market_implied(market, d.decision_date)})
    if next_decision:
        implied = futures_implied(futures, overnight, next_decision)
        if implied:
            rows.append({"decision_date": next_decision, "actual_change": np.nan, **implied,
                         **market_implied(market, next_decision)})
    return pd.DataFrame(rows)


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


def _quarter(label: str) -> dt.date:
    """'2026-Q3' -> 2026-09-30, the quarter-end date used in table G1."""
    year, q = int(label[:4]), int(label[-1])
    return (pd.Timestamp(year=year, month=3 * q, day=1) + pd.offsets.MonthEnd(0)).date()


def load_tm_nowcasts(smp_db: Path = config.SMP_DB_PATH) -> dict[dt.date, float]:
    """The RBA's forecast of trimmed mean inflation for the quarter of each SMP (its nowcast), by quarter end.

    From the historical file where it has the SMP's own quarter (horizon 0, SMP months only, so only
    published forecasts); then from the archive, which has horizon 0 for May and November SMPs. For
    February and August SMPs the nowcast is interpolated between the previous quarter's outcome and
    the forecast for the next quarter.
    """
    conn = sqlite3.connect(smp_db)
    smp_months = {r[0] for r in conn.execute("SELECT issue_date FROM smp_issues")}
    out = {}
    for month, target, value in conn.execute(
            "SELECT forecast_month, target_quarter, forecast FROM forecasts_by_horizon "
            "WHERE variable = 'underlying_ye' AND horizon = 0"):
        if month in smp_months:
            out[_quarter(target)] = value
    tm, _ = load_macro()
    archive = pd.read_sql("SELECT smp_date, target_period, horizon, value FROM smp_forecast_archive "
                          "WHERE variable_code = 'underlying_ye'", conn)
    for month, group in archive.groupby("smp_date"):
        year, mon = int(month[:4]), int(month[5:])
        own = _quarter(f"{year}-Q{(mon - 1) // 3 + 1}")
        if own in out:
            continue
        h0 = group[group["horizon"] == 0]["value"]
        h1 = group[group["horizon"] == 1]["value"]
        previous = (pd.Timestamp(own) - pd.offsets.QuarterEnd(1)).date()
        if len(h0):
            out[own] = float(h0.iloc[0])
        elif len(h1) and previous in tm.index:
            out[own] = (float(tm[previous]) + float(h1.iloc[0])) / 2
    return out


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
    gdp, wpi = load_abs()
    market = load_market()
    forecasts = load_rba_forecasts()
    nowcasts = load_tm_nowcasts()
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
            # the latest outcome against the RBA's own forecast for that quarter
            "tm_surprise": tm_value - nowcasts[tm_date] if tm_date in nowcasts else np.nan,
            "unemployment": ur_value,
            "unemployment_change_6m": ur_value - ur_6m.iloc[-1] if len(ur_6m) else np.nan,
            "gdp_growth": _latest_before(gdp, when, GDP_PUBLICATION_LAG)[1],
            "wpi_growth": _latest_before(wpi, when, WPI_PUBLICATION_LAG)[1],
            **market_implied(market, when),
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
    # ABS activity and wages data
    "macro+gdp": ["tm_gap", "unemployment_change_6m", "last_move", "gdp_growth"],
    "macro+wages": ["tm_gap", "unemployment_change_6m", "last_move", "wpi_growth"],
    "macro+gdp+wages": ["tm_gap", "unemployment_change_6m", "last_move", "gdp_growth", "wpi_growth"],
    # Market pricing (the ASX Rate Tracker's information): OIS 2001-2022, bank bills 2001 onwards
    "market_ois": ["mkt_ois_change"],
    "macro+market_ois": ["tm_gap", "unemployment_change_6m", "last_move", "mkt_ois_change"],
    "market_ois+minutes_text": ["mkt_ois_change", "minutes_llm_stance_score", "minutes_hawk_dove_index"],
    "macro+market_ois+minutes_text": ["tm_gap", "unemployment_change_6m", "last_move", "mkt_ois_change",
                                      "minutes_llm_stance_score", "minutes_hawk_dove_index"],
    "market_bab": ["mkt_bab_change"],
    "macro+market_bab": ["tm_gap", "unemployment_change_6m", "last_move", "mkt_bab_change"],
    "macro+market_bab+minutes_text": ["tm_gap", "unemployment_change_6m", "last_move", "mkt_bab_change",
                                      "minutes_llm_stance_score", "minutes_hawk_dove_index"],
    "macro+gdp+wages+minutes_text": ["tm_gap", "unemployment_change_6m", "last_move", "gdp_growth", "wpi_growth",
                                     "minutes_llm_stance_score", "minutes_hawk_dove_index"],
    "macro+surprise": ["tm_gap", "unemployment_change_6m", "last_move", "tm_surprise"],
    "macro+surprise+minutes_text": ["tm_gap", "unemployment_change_6m", "last_move", "tm_surprise",
                                    "minutes_llm_stance_score", "minutes_hawk_dove_index"],
    # Dictionary measures only: no LLM, so no chance of hindsight from a labeller that knows what came next.
    "macro+smp_dictionary": ["tm_gap", "unemployment_change_6m", "last_move", "smp_hawk_dove_index", "smp_lm_tone"],
    "macro+minutes_dictionary": ["tm_gap", "unemployment_change_6m", "last_move", "minutes_hawk_dove_index",
                                 "minutes_policy_hawk_dove_index"],
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
    nowcasts = load_tm_nowcasts()
    gdp, wpi = load_abs()
    row = {"decision_date": when, "previous_direction": past["direction"].iloc[-1],
           "last_move": moves["direction"].iloc[-1], "moves_6m": int(recent["direction"].sum()),
           "tm_yoy": tm_value, "tm_quarter": tm_date, "tm_gap": tm_value - TARGET_MIDPOINT,
           "tm_surprise": tm_value - nowcasts[tm_date] if tm_date in nowcasts else np.nan,
           "unemployment": ur_value, "unemployment_change_6m": ur_value - ur_6m.iloc[-1],
           "gdp_growth": _latest_before(gdp, when, GDP_PUBLICATION_LAG)[1],
           "wpi_growth": _latest_before(wpi, when, WPI_PUBLICATION_LAG)[1],
           **market_implied(load_market(), when)}
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
        if any(pd.isna(row.get(f)) for f in feats):
            continue  # an input not available for this decision (e.g. OIS, published only to 2022)
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
        # ABS GDP and Wage Price Index growth (WPI year-ended growth from September 1998)
        "abs_era": ("1998-11-01", {k: MODELS[k] for k in ("macro", "macro+gdp", "macro+wages", "macro+gdp+wages")}),
        # Market pricing: OIS from July 2001 to December 2022 (the RBA stopped publishing it then)
        "market_ois_era": ("2001-08-01", {k: MODELS[k] for k in ("macro", "market_ois", "macro+market_ois")}),
        "market_ois_minutes_era": ("2006-11-01", {k: MODELS[k] for k in (
            "macro", "macro+minutes_text", "market_ois", "macro+market_ois", "market_ois+minutes_text",
            "macro+market_ois+minutes_text")}),
        # Bank bills: noisier (they carry a credit premium) but available to date
        "market_bab_minutes_era": ("2006-11-01", {k: MODELS[k] for k in (
            "macro", "macro+minutes_text", "market_bab", "macro+market_bab", "macro+market_bab+minutes_text")}),
        "smp_era": ("1997-03-01", {k: MODELS[k] for k in ("macro", "macro+smp_text", "macro+smp_dictionary")}),
        # the RBA's year-ahead trimmed mean forecast is published in the SMP from May 2000
        "smp_forecast_era": ("2000-06-01", {k: MODELS[k] for k in ("macro", "macro+rba_forecast", "macro+smp_text",
                                                                   "macro+rba_forecast+smp_text", "macro+surprise")}),
        # FSR from March 2004
        "fsr_era": ("2004-04-01", {k: MODELS[k] for k in ("macro", "macro+rba_forecast+smp_text", "macro+fsr")}),
        # minutes from October 2006: every source available
        "minutes_era": ("2006-11-01", {k: MODELS[k] for k in ("macro", "macro+rba_forecast", "macro+smp_text",
                                                              "macro+minutes_text", "macro+minutes_dictionary",
                                                              "macro+surprise", "macro+surprise+minutes_text",
                                                              "macro+fsr", "macro+all_text", "text_only",
                                                              "macro+gdp", "macro+wages", "macro+gdp+wages",
                                                              "macro+gdp+wages+minutes_text")}),
    }
    for name, (start, models) in samples.items():
        min_train = 48 if name != "minutes_era" else 40
        table, preds = backtest(panel, start, models, min_train=min_train)
        preds.to_csv(OUT_DIR / f"backtest_{name}.csv", index=False)
        table.to_csv(OUT_DIR / f"scores_{name}.csv", index=False)
        tests = [paired_log_loss(preds, "macro", m) for m in models if m != "macro"]
        tests += [paired_log_loss(preds, "class_frequencies", "macro")]
        if "market_ois" in models:  # what adds to market pricing
            tests += [paired_log_loss(preds, "market_ois", m) for m in models if m != "market_ois" and (m.endswith("market_ois") or "market_ois+" in m)]
        results[name] = {"start": start, "first_test": str(preds["decision_date"].min()),
                         "last_test": str(preds["decision_date"].max()),
                         "scores": table.to_dict("records"), "paired": tests}
    # Sensitivity of the minutes-era comparison to the amount of regularisation.
    sensitivity = []
    for C in (0.1, 0.3, 3.0, 10.0):
        models = {k: MODELS[k] for k in ("macro", "macro+minutes_text", "macro+minutes_dictionary", "macro+all_text")}
        table, preds = backtest(panel, "2006-11-01", models, min_train=40, C=C)
        for m in models:
            if m != "macro":
                sensitivity.append({"C": C, **paired_log_loss(preds, "macro", m),
                                    **{f"accuracy_{k}": v for k, v in table.set_index("model").loc[m, ["accuracy"]].items()}})
    pd.DataFrame(sensitivity).to_csv(OUT_DIR / "sensitivity_minutes_era.csv", index=False)
    results["sensitivity_minutes_era"] = sensitivity
    if next_decision:
        forecast, row = forecast_next(panel, next_decision)
        forecast.to_csv(OUT_DIR / "next_decision.csv", index=False)
        results["next_decision"] = {"date": str(next_decision), "features": {k: (str(v) if isinstance(v, (dt.date,)) else v)
                                                                              for k, v in row.items()},
                                    "probabilities": forecast.to_dict("records")}
    asx = load_asx()
    if asx and next_decision == asx["next_meeting"]:
        # The ASX Rate Tracker's priced move, reproduced from its futures curve, as the market input
        # (futures and OIS price the same expected cash rate; the models were fitted on OIS, 2001-2022).
        overnight = pd.Series([asx["cash_rate_target"]], index=[asx["settlement_date"]])
        implied = futures_implied(asx["futures"], overnight, next_decision)
        row = features_as_of(panel, next_decision)
        row["mkt_ois_change"] = implied["futures_change"]
        market_rows = []
        for name in ("market_ois", "macro+market_ois", "market_ois+minutes_text", "macro+market_ois+minutes_text"):
            feats = MODELS[name]
            p = _fit_predict(panel.dropna(subset=feats), pd.DataFrame([row]), feats)[0]
            market_rows.append({"model": f"{name} (ASX futures)", "trained_on": int(panel.dropna(subset=feats).shape[0]),
                                "p_cut": p[0], "p_hold": p[1], "p_raise": p[2]})
        results["asx_rate_tracker"] = {
            "settlement_date": str(asx["settlement_date"]), "next_meeting": str(asx["next_meeting"]),
            "probability_of_move": asx["probability"], "expected_rate": asx["expected_rate"],
            "reproduced_change": implied["futures_change"], "history": asx["history"].to_dict("records"),
            "model_probabilities": market_rows,
        }
        pd.DataFrame(market_rows).to_csv(OUT_DIR / "next_decision_market.csv", index=False)
    if FUTURES_FILE.exists():
        check = futures_check(next_decision)
        check.to_csv(OUT_DIR / "futures_check.csv", index=False)
        results["futures_check"] = check.to_dict("records")
    (OUT_DIR / "results.json").write_text(json.dumps(results, indent=2, default=str))
    return results
