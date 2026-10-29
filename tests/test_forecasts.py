"""Tests for the forecast loader, run against the real workbooks in data/forecasts."""

import re
import sqlite3

import pytest

pytest.importorskip("pandas")
pytest.importorskip("xlrd")      # .xls workbooks
pytest.importorskip("openpyxl")  # .xlsx archive

from rba_minutes import forecasts

QUARTER = re.compile(r"^\d{4}-Q[1-4]$")
VARIABLES = {"cpi_ye", "underlying_ye", "gdp_qtr", "unemployment"}


def qindex(label: str) -> int:
    return int(label[:4]) * 4 + int(label[-1]) - 1


@pytest.fixture(scope="module")
def conn():
    conn = sqlite3.connect(":memory:")
    forecasts.load_forecasts(conn)
    yield conn
    conn.close()


def rows(conn, sql, *params):
    return conn.execute(sql, params).fetchall()


def test_reload_replaces_rows(conn):
    before = rows(conn, "SELECT COUNT(*) FROM forecasts_by_horizon")[0][0]
    counts = forecasts.load_forecasts(conn)
    assert counts["forecasts_by_horizon"] == before > 4000
    assert rows(conn, "SELECT COUNT(*) FROM smp_forecast_archive")[0][0] == counts["smp_forecast_archive"] > 3000


def test_every_variable_loaded_since_1990s(conn):
    found = dict(rows(conn, "SELECT variable, MIN(forecast_quarter) FROM forecasts_by_horizon GROUP BY 1"))
    assert set(found) == VARIABLES
    assert all(first <= "1991-Q1" for first in found.values())
    # released with a one-year lag: the files run to the February 2025 SMP
    assert rows(conn, "SELECT MAX(forecast_quarter) FROM forecasts_by_horizon")[0][0] >= "2025-Q1"


def test_quarters_well_formed_and_consistent(conn):
    for forecast_q, horizon, target_q, month in rows(
            conn, "SELECT forecast_quarter, horizon, target_quarter, forecast_month FROM forecasts_by_horizon"):
        assert QUARTER.match(forecast_q) and QUARTER.match(target_q)
        assert qindex(target_q) == qindex(forecast_q) + horizon
        assert forecasts.quarter_index(int(month[:4]), int(month[5:])) == qindex(forecast_q)


def test_horizons_start_at_zero(conn):
    assert rows(conn, "SELECT MIN(horizon) FROM forecasts_by_horizon")[0][0] == 0
    # every vintage has a current-quarter (t) forecast
    assert rows(conn, """SELECT COUNT(*) FROM (SELECT variable, forecast_quarter FROM forecasts_by_horizon
                         GROUP BY 1, 2 HAVING MIN(horizon) > 0)""")[0][0] == 0
    assert rows(conn, "SELECT MAX(horizon) FROM forecasts_by_horizon")[0][0] <= 12


def test_error_is_actual_minus_forecast(conn):
    # sign convention found in the Errors sheets: error = actual - forecast
    with_actual = rows(conn, "SELECT forecast, actual, error FROM forecasts_by_horizon WHERE actual IS NOT NULL")
    assert len(with_actual) > 4000
    for forecast, actual, error in with_actual:
        assert error is not None
        assert error == pytest.approx(actual - forecast, abs=1e-6)
    assert rows(conn, "SELECT COUNT(*) FROM forecasts_by_horizon WHERE actual IS NULL AND error IS NOT NULL")[0][0] == 0


def test_values_plausible(conn):
    ranges = {"cpi_ye": (-2, 12), "underlying_ye": (-1, 10), "gdp_qtr": (-10, 5), "unemployment": (3, 12)}
    for variable, (low, high) in ranges.items():
        lo, hi = rows(conn, "SELECT MIN(forecast), MAX(forecast) FROM forecasts_by_horizon WHERE variable = ?",
                      variable)[0]
        assert low <= lo and hi <= high, variable
    latest = rows(conn, """SELECT forecast FROM forecasts_by_horizon WHERE variable = 'underlying_ye'
                           AND forecast_quarter = (SELECT MAX(forecast_quarter) FROM forecasts_by_horizon)""")
    assert latest and all(0 < f < 10 for (f,) in latest)


def test_known_spot_values(conn):
    def one(sql, *params):
        return rows(conn, sql, *params)[0][0]
    # Feb 2025 SMP: trimmed mean 2.9 per cent in the March quarter, then 2.7
    assert one("""SELECT forecast FROM forecasts_by_horizon WHERE variable = 'underlying_ye'
                  AND forecast_quarter = '2025-Q1' AND horizon = 0""") == pytest.approx(2.9)
    # headline CPI peaked at 7.8 per cent year-ended in December quarter 2022
    assert one("""SELECT MAX(actual) FROM forecasts_by_horizon WHERE variable = 'cpi_ye'
                  AND target_quarter = '2022-Q4'""") == pytest.approx(7.8)
    # the COVID contraction in June quarter 2020
    assert one("""SELECT MIN(actual) FROM forecasts_by_horizon WHERE variable = 'gdp_qtr'
                  AND target_quarter = '2020-Q2'""") < -6
    assert one("""SELECT measure FROM forecasts_by_horizon WHERE variable = 'underlying_ye'
                  AND forecast_quarter = '2025-Q1' LIMIT 1""").startswith("TM")


def test_matches_event_date_workbook(conn):
    event = forecasts.read_event_date_file()
    loaded = rows(conn, "SELECT variable, forecast_quarter, target_quarter, forecast FROM forecasts_by_horizon")
    for variable, forecast_q, target_q, value in loaded:
        assert event[(variable, forecast_q, target_q)] == pytest.approx(value), (variable, forecast_q, target_q)


def test_smp_archive(conn):
    first, last = rows(conn, "SELECT MIN(smp_date), MAX(smp_date) FROM smp_forecast_archive")[0]
    assert first == "2018-11" and last >= "2025-02"
    for smp_date, target, label, horizon in rows(
            conn, "SELECT smp_date, target_period, target_label, horizon FROM smp_forecast_archive"):
        assert re.match(r"^\d{4}-(0[1-9]|1[0-2])$", smp_date)
        assert QUARTER.match(target) and label.endswith(target[:4])
        assert -4 <= horizon <= 12
    codes = {c for (c,) in rows(conn, "SELECT DISTINCT variable_code FROM smp_forecast_archive")}
    assert {"gdp_ye", "cpi_ye", "underlying_ye", "unemployment", "cash_rate"} <= codes
    assert rows(conn, """SELECT value FROM smp_forecast_archive WHERE source_sheet = 'RGDP'
                         AND smp_date = '2018-11' AND target_period = '2018-Q4'""")[0][0] == pytest.approx(3.6)
    assert rows(conn, "SELECT variable FROM smp_forecast_archive WHERE source_sheet = 'TMI' LIMIT 1")[0][0] \
        == "Trimmed mean inflation"
    lo, hi = rows(conn, "SELECT MIN(value), MAX(value) FROM smp_forecast_archive WHERE variable_code = 'cash_rate'")[0]
    assert 0 <= lo and hi < 10
    # one row per (SMP, sheet, target): the primary key would reject duplicates
    assert rows(conn, """SELECT COUNT(*) FROM (SELECT 1 FROM smp_forecast_archive
                         GROUP BY smp_date, source_sheet, target_period HAVING COUNT(*) > 1)""")[0][0] == 0


def test_forecast_archive_matches_extracted_smp_tables():
    """Every value in the RBA forecast archive appears in the forecast table extracted from that SMP's HTML."""
    import sqlite3
    from pathlib import Path

    from rba_minutes import smp_tables

    path = Path(__file__).resolve().parent.parent / "data" / "rba_smp.sqlite"
    if not path.exists():
        pytest.skip("SMP database not built")
    conn = sqlite3.connect(path)
    if not conn.execute("SELECT name FROM sqlite_master WHERE name = 'smp_table_cells'").fetchone():
        pytest.skip("SMP tables not built")
    result = smp_tables.cross_check_forecast_archive(conn)
    if "skipped" in result:
        pytest.skip(result["skipped"])
    assert result["matched"] > 3000
    assert result["different"] == [] and result["not_found"] == []
