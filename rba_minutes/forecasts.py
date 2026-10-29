"""RBA forecast files: load the published forecast workbooks into tidy tables.

Two tables, both rebuilt in full on every run:

- forecasts_by_horizon: central forecasts since 1990 for year-ended CPI and
  underlying inflation, quarterly GDP growth and the unemployment rate, with the
  outcome and error the RBA publishes alongside each one. Built from the four
  "forecast date by horizon" workbooks, which are released with a one-year lag.
- smp_forecast_archive: every number in the detailed SMP forecast tables from
  November 2018, one row per (SMP, variable, target period).

The workbooks are downloaded by `smp.download_forecasts()` into data/forecasts.

Layout of the by-horizon workbooks (Forecasts, Actuals and Errors sheets):
column 0 holds the forecast date (first of a month: the last month of the
quarter for JEFG / board paper rounds, the middle month for SMP / PDG rounds);
one header row labels the horizon columns t-12 ... t, t+1 ...; the Forecasts
sheet also has Source (and, for underlying inflation, Measure) columns before
the horizons. Horizon t is the quarter the forecast date falls in. The sheets
do not share row offsets, so they are aligned on the forecast date. Negative
horizons (the history as known at the time) are not loaded. Errors are
published as actual minus forecast.
"""

import datetime as dt
import logging
import re
import sqlite3
from pathlib import Path

import pandas as pd

from . import config, db

log = logging.getLogger(__name__)

FORECAST_DIR = config.DATA_DIR / "forecasts"

HORIZON_FILES = {
    "cpi_ye": "cpi-by-horizon.xls",
    "underlying_ye": "underlying-inflation-by-horizon.xls",
    "gdp_qtr": "gdp-by-horizon.xls",
    "unemployment": "unemployment-rate-by-horizon.xls",
}
EVENT_DATE_FILE = "forecast-date-by-event-date.xls"
EVENT_DATE_SHEETS = {
    "cpi_ye": "CPI - 4 quarter change",
    "underlying_ye": "Underlying - 4 quarter change",
    "gdp_qtr": "GDP - 1 quarter change",
    "unemployment": "Unemployment rate - Level",
}
ARCHIVE_FILE = "smp-forecast-archive.xlsx"

# Archive sheet -> short code. Codes shared with forecasts_by_horizon mean the
# same series (the archive's underlying measure is trimmed mean throughout).
ARCHIVE_CODES = {
    "RGDP": "gdp_ye",
    "RHFCE": "consumption_ye",
    "RDI": "dwelling_investment_ye",
    "RBI": "business_investment_ye",
    "RPD": "public_demand_ye",
    "RGNE": "gne_ye",
    "RMTPGDP": "trading_partner_gdp_ye",
    "Imp": "imports_ye",
    "Exp": "exports_ye",
    "ToT": "terms_of_trade_ye",
    "Emp": "employment_ye",
    "UR": "unemployment",
    "HUR": "hours_underutilisation",
    "NWPI": "wpi_ye",
    "NAENA": "avg_earnings_hour_ye",
    "RHDI": "real_disposable_income_ye",
    "CPI": "cpi_ye",
    "TMI": "underlying_ye",
    "CR": "cash_rate",
    "TWI": "twi",
    "Crude": "brent_oil_usd",
    "ERP": "population_ye",
    "GDPpHW": "labour_productivity_ye",
    "HSR": "household_saving_ratio",
    "RWPI": "real_wpi_ye",
    "RAENA": "real_avg_earnings_hour_ye",
}

SCHEMA = """
CREATE TABLE IF NOT EXISTS forecasts_by_horizon (
    variable            TEXT NOT NULL,      -- cpi_ye / underlying_ye / gdp_qtr / unemployment
    forecast_quarter    TEXT NOT NULL,      -- YYYY-Qn, the quarter (t) the forecast was made in
    horizon             INTEGER NOT NULL,   -- quarters ahead of forecast_quarter, 0 = current quarter
    target_quarter      TEXT NOT NULL,      -- YYYY-Qn, forecast_quarter + horizon
    forecast            REAL NOT NULL,      -- per cent
    actual              REAL,               -- outcome as published with this vintage, per cent
    error               REAL,               -- published error: actual - forecast
    forecast_month      TEXT NOT NULL,      -- YYYY-MM, the workbook's 'Forecast date'
    forecast_source     TEXT,               -- JEFG / PDG / SMP / BP / IR, '/'-joined if several
    measure             TEXT,               -- underlying inflation only: the measure forecast
    source_file         TEXT NOT NULL,
    loaded_at           TEXT NOT NULL,
    PRIMARY KEY (variable, forecast_quarter, horizon)
);

CREATE TABLE IF NOT EXISTS smp_forecast_archive (
    smp_date            TEXT NOT NULL,      -- YYYY-MM of the SMP
    variable            TEXT NOT NULL,      -- series name as the archive labels it
    variable_code       TEXT,               -- normalised short code (NULL for sheets not in ARCHIVE_CODES)
    target_period       TEXT NOT NULL,      -- YYYY-Qn
    target_label        TEXT NOT NULL,      -- as written in the archive, e.g. 'Dec-2018'
    horizon             INTEGER NOT NULL,   -- quarters from the SMP's quarter to target_period
    value               REAL NOT NULL,      -- per cent, except TWI (index) and Brent oil (US$/bbl)
    source_sheet        TEXT NOT NULL,
    loaded_at           TEXT NOT NULL,
    PRIMARY KEY (smp_date, source_sheet, target_period)
);
"""

HORIZON_LABEL = re.compile(r"^t(?:\s*([+-])\s*(\d+))?$")
QUARTER_LABEL = re.compile(r"^(\d{4})\s*-?Q([1-4])$")
MONTHS = {m: i for i, m in enumerate(
    ["jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"], 1)}


# --- Helpers --------------------------------------------------------------------

def quarter_index(year: int, month: int) -> int:
    return year * 4 + (month - 1) // 3


def quarter_label(index: int) -> str:
    return f"{index // 4}-Q{index % 4 + 1}"


def _number(value) -> float | None:
    """A numeric cell as float; blanks and '--' (not available) as None."""
    if value is None or isinstance(value, str) or pd.isna(value):
        return None
    return float(value)


def _text(value) -> str | None:
    if value is None or (not isinstance(value, str) and pd.isna(value)):
        return None
    return str(value).strip() or None


def _month(value) -> tuple[int, int] | None:
    """(year, month) from a date cell or a 'Nov-2018' label."""
    if isinstance(value, (dt.date, pd.Timestamp)):
        return value.year, value.month
    match = re.match(r"^([A-Za-z]{3})[a-z]*[-\s](\d{4})$", str(value).strip())
    if match and match.group(1).lower() in MONTHS:
        return int(match.group(2)), MONTHS[match.group(1).lower()]
    return None


# --- Forecast date by horizon -----------------------------------------------------

def read_horizon_sheet(sheet: pd.DataFrame) -> dict[pd.Timestamp, tuple[dict, dict]]:
    """{forecast date: ({horizon: cell}, {label column: cell})} for one sheet."""
    for header_row in range(min(10, len(sheet))):
        if any(isinstance(v, str) and v.strip() == "t" for v in sheet.iloc[header_row]):
            break
    else:
        raise ValueError("no horizon header row (a cell reading 't')")
    horizons, labels = {}, {}
    for col, value in enumerate(sheet.iloc[header_row]):
        match = HORIZON_LABEL.match(value.strip()) if isinstance(value, str) else None
        if match:
            horizons[col] = int(match.group(1) + match.group(2)) if match.group(1) else 0
    for col in range(1, min(horizons)):
        name = _text(sheet.iloc[header_row, col]) or "Source"  # unlabelled in the unemployment file
        labels[col] = "measure" if name.lower().startswith("measure") else "source"
    rows = {}
    for row in range(header_row + 1, len(sheet)):
        date = sheet.iloc[row, 0]
        if not isinstance(date, (dt.date, pd.Timestamp)):
            continue
        rows[pd.Timestamp(date)] = (
            {h: sheet.iloc[row, col] for col, h in horizons.items()},
            {name: _text(sheet.iloc[row, col]) for col, name in labels.items()},
        )
    return rows


def read_horizon_file(path: Path, variable: str) -> list[dict]:
    """One dict per (forecast quarter, horizon >= 0) with a numeric forecast."""
    book = pd.read_excel(path, sheet_name=None, header=None)
    forecasts = read_horizon_sheet(book["Forecasts"])
    actuals = read_horizon_sheet(book["Actuals"])
    errors = read_horizon_sheet(book["Errors"])
    records, seen = [], set()
    for date, (cells, labels) in sorted(forecasts.items()):
        t = quarter_index(date.year, date.month)
        if t in seen:
            raise ValueError(f"{path.name}: two forecast dates in {quarter_label(t)}")
        seen.add(t)
        for horizon, cell in sorted(cells.items()):
            forecast = _number(cell)
            if horizon < 0 or forecast is None:
                continue
            records.append({
                "variable": variable,
                "forecast_quarter": quarter_label(t),
                "horizon": horizon,
                "target_quarter": quarter_label(t + horizon),
                "forecast": forecast,
                "actual": _number(actuals.get(date, ({}, {}))[0].get(horizon)),
                "error": _number(errors.get(date, ({}, {}))[0].get(horizon)),
                "forecast_month": f"{date.year}-{date.month:02d}",
                "forecast_source": labels.get("source"),
                "measure": labels.get("measure"),
                "source_file": path.name,
            })
    return records


def read_event_date_file(path: Path = FORECAST_DIR / EVENT_DATE_FILE) -> dict[tuple[str, str, str], float]:
    """{(variable, forecast_quarter, target_quarter): forecast} from the event-date workbook.

    Not stored: it holds the same forecasts as the by-horizon files (plus the
    long history as known at each date) and serves as a cross-check.
    """
    book = pd.read_excel(path, sheet_name=None, header=None)
    out = {}
    for variable, sheet_name in EVENT_DATE_SHEETS.items():
        sheet = book[sheet_name]
        date_row = next(r for r in range(10) if str(sheet.iloc[r, 0]).startswith("Forecast date"))
        targets = {}
        for row in range(date_row + 1, len(sheet)):
            match = QUARTER_LABEL.match(str(sheet.iloc[row, 0]).strip())
            if match:
                targets[row] = quarter_index(int(match.group(1)), 3 * int(match.group(2)))
        for col in range(1, sheet.shape[1]):
            date = sheet.iloc[date_row, col]
            if not isinstance(date, (dt.date, pd.Timestamp)):
                continue
            t = quarter_label(quarter_index(date.year, date.month))
            for row, target in targets.items():
                value = _number(sheet.iloc[row, col])
                if value is not None:
                    out[(variable, t, quarter_label(target))] = value
    return out


# --- SMP forecast archive ---------------------------------------------------------

def read_archive(path: Path) -> list[dict]:
    """One dict per (SMP, sheet, target period) with a value."""
    book = pd.read_excel(path, sheet_name=None, header=None)
    records = []
    for sheet_name, sheet in book.items():
        if sheet_name.lower() == "contents":
            continue
        header_row = next((r for r in range(min(10, len(sheet)))
                           if str(sheet.iloc[r, 0]).strip().lower().startswith("publication date")), None)
        if header_row is None:
            log.warning("%s: sheet %s has no 'Publication date' row; skipped", path.name, sheet_name)
            continue
        label = _text(sheet.iloc[0, 0]) or sheet_name
        smps = {col: _month(sheet.iloc[header_row, col]) for col in range(1, sheet.shape[1])}
        for row in range(header_row + 1, len(sheet)):
            target_label = _text(sheet.iloc[row, 0])
            target = _month(target_label) if target_label else None
            if target is None:
                continue
            target_q = quarter_index(*target)
            for col, smp in smps.items():
                value = _number(sheet.iloc[row, col])
                if smp is None or value is None:
                    continue
                records.append({
                    "smp_date": f"{smp[0]}-{smp[1]:02d}",
                    "variable": label,
                    "variable_code": ARCHIVE_CODES.get(sheet_name),
                    "target_period": quarter_label(target_q),
                    "target_label": target_label,
                    "horizon": target_q - quarter_index(*smp),
                    "value": value,
                    "source_sheet": sheet_name,
                })
    return records


# --- Loading ----------------------------------------------------------------------

def _insert(conn: sqlite3.Connection, table: str, records: list[dict], loaded_at: str) -> None:
    if not records:
        return
    columns = list(records[0]) + ["loaded_at"]
    conn.executemany(
        f"INSERT INTO {table} ({', '.join(columns)}) VALUES ({', '.join('?' * len(columns))})",
        [tuple(r.values()) + (loaded_at,) for r in records],
    )


def load_forecasts(conn: sqlite3.Connection, forecast_dir: Path = FORECAST_DIR) -> dict[str, int]:
    """Rebuild forecasts_by_horizon and smp_forecast_archive; return row counts."""
    conn.executescript(SCHEMA)
    horizon_rows = []
    for variable, name in HORIZON_FILES.items():
        horizon_rows += read_horizon_file(forecast_dir / name, variable)
    archive_rows = read_archive(forecast_dir / ARCHIVE_FILE)
    loaded_at = db.now()
    with conn:
        conn.execute("DELETE FROM forecasts_by_horizon")
        conn.execute("DELETE FROM smp_forecast_archive")
        _insert(conn, "forecasts_by_horizon", horizon_rows, loaded_at)
        _insert(conn, "smp_forecast_archive", archive_rows, loaded_at)
    return {"forecasts_by_horizon": len(horizon_rows), "smp_forecast_archive": len(archive_rows)}
