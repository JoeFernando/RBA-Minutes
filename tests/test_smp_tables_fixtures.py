"""Regression tests on real RBA tables that independent audits flagged (tests/fixtures/rba_tables).

Each test pins down the structure the published table actually has, as
checked by hand against the HTML (and the PDF where noted in the audits).
"""

from pathlib import Path

import pytest
from bs4 import BeautifulSoup

from rba_minutes.smp_tables import parse_table

FIXTURES = Path(__file__).parent / "fixtures" / "rba_tables"


def table(table_id: int) -> dict:
    path = next(FIXTURES.glob(f"*_{table_id}.html"))
    return parse_table(BeautifulSoup(path.read_text(), "lxml").find("table"))


def rows(t: dict) -> list[tuple]:
    """(row_group, row_label, row_level) per row, in order."""
    seen, out = set(), []
    for c in t["cells"]:
        if c["row_idx"] not in seen:
            seen.add(c["row_idx"])
            out.append((c["row_group"], c["row_label"], c["row_level"]))
    return out


def cell(t: dict, row_label: str, col_label: str, group: str | None = None) -> dict:
    hits = [c for c in t["cells"] if c["row_label"] == row_label and c["col_label"] == col_label
            and (group is None or c["row_group"] == group)]
    assert len(hits) == 1, (row_label, col_label, group, len(hits))
    return hits[0]


def test_nested_groups_and_double_total_2006_bond_issuance():
    t = table(651)
    assert t["column_labels"] == ["2003", "2004", "2005", "2006 | June half", "2006 | September quarter"]
    r = rows(t)
    assert r[0] == ("Bond issues by Australian entities > Onshore", "Financial institutions", 0)
    assert r[4] == ("Bond issues by Australian entities > Offshore", "Financial institutions", 0)
    assert r[8] == ("Bond issues by Australian entities", "Total", 0)  # the grand total of both
    assert r[9][0] == "A$ bond issues by non-resident entities"
    assert t["sources"] == "RBA"  # 'Source: RBA' was a body row, not a group


def test_group_hierarchy_with_indented_children_2002_lending_rates():
    r = rows(table(348))
    assert r[0] == ("", "Cash rate", 0)
    assert r[1] == ("Households > Standard variable mortgages", "Banks", 1)
    assert r[3] == ("Households", "Banks' basic", 0)
    assert r[4] == ("Households > Personal lending", "Residential secured", 1)
    assert r[8] == ("Small business > Other security:", "Overdraft", 1)
    assert r[10] == ("Large business", "Overdraft", 1)


def test_of_which_group_ends_after_its_rows():
    r = rows(table(349))
    assert r[4][:2] == ("of which:", "Credit-wrapped")
    assert r[6][:2] == ("", "Asset-backed issuance")
    assert r[7][:2] == ("", "Total new issuance")


def test_memo_item_label_row_keeps_its_group():
    assert rows(table(643))[-1][:2] == ("Memo item:", "Real GDP adjusted for changes in the terms of trade")


def test_spacer_columns_dropped():
    t = table(532)
    assert t["n_cols"] == 8 and all(t["column_labels"])


def test_side_by_side_lists_become_panels():
    t = table(304)
    tot = [c for c in t["cells"] if c["col_label"] == "Terms of trade"]
    assert [c["row_label"] for c in tot][:2] == ["March 1965", "March 1971"]
    assert tot[0]["value_num"] == -1.0
    assert not [c for c in t["cells"] if c["row_label"] == ""]


def test_mid_row_header_cells_start_a_panel_1998_loans():
    t = table(78)
    assert t["column_labels"] == ["variable-rate loans", "Fixed-rate loans"]  # the label column is not a value column
    common = "Movement between June quarter 1996 and September quarter 1998 Percentage points"
    assert cell(t, "Housing", "Fixed-rate loans")["value_num"] == -2.8
    assert cell(t, "Indicator rate", "Fixed-rate loans", f"{common} > Small business")["value_num"] == -2.9
    assert cell(t, "Indicator rate", "Fixed-rate loans", f"{common} > Large business")["value_num"] == -3.0
    assert cell(t, "Overdraft", "variable-rate loans", f"{common} > Small business > Indicator rates")["value_num"] == -3.6
    assert cell(t, "Three-year swap rate", "Fixed-rate loans", common)["value_num"] == -2.9


def test_bold_category_row_closes_group_2010_lending_rates():
    r = rows(table(896))
    assert r[1] == ("Housing loans", "Prime low-doc", 0)
    assert r[2] == ("", "Personal loans", 0)
    assert r[3] == ("Small business > Term loans", "Residentially secured", 1)
    assert r[7] == ("Small business", "Average actual rate", 0)


def test_label_row_with_blank_cells_groups_unindented_rows_2012_cpi():
    r = rows(table(994))
    assert r[5] == ("Selected underlying measures", "Trimmed mean", 0)
    assert cell(table(994), "CPI excl volatile items", "Quarterly | December quarter 2011")["footnotes"].count("(c)") == 1


def test_split_arrow_and_amount_cells_2008_policy_changes():
    t = table(770)
    us = [c for c in t["cells"] if c["row_label"] == "United States"]
    amount = next(c for c in us if c["value_text"] == "25 bps")
    assert (amount["value_num"], amount["value_direction"]) == (-25.0, -1)
    date = next(c for c in us if c["value_text"] == "Oct 08")
    assert date["value_num"] is None


def test_year_rows_typed_as_data_are_headers_2016_forecasts():
    t = table(1236)
    assert t["n_rows"] == 4
    assert rows(t)[3] == ("Year-average", "GDP growth", 0)
    assert cell(t, "GDP growth", "2015/16", "Year-average")["value_num"] == 2.8
    assert cell(t, "GDP growth", "Jun 2016", "Year-ended")["value_num"] == 3.3
    assert not [c for c in t["cells"] if c["value_num"] in (2016.0, 2017.0, 2018.0)]


def test_rows_with_values_are_not_headings_and_deep_indents_2021_aggregates():
    t = table(1394)
    labels = [r[1] for r in rows(t)]
    assert labels[0] == "Total credit" and labels[-1] == "Broad money"
    assert ("", "Owner-occupier", 2) in rows(t)


def test_colspan_wider_than_table_2022_housing_rates():
    t = table(1408)
    assert t["n_cols"] == 3 and t["n_rows"] == 8
    assert rows(t)[6] == ("By repayment type", "Principal-and-interest", 1)


def test_label_carried_down_and_second_label_column():
    assert [r[1] for r in rows(table(53))][:2] == ["Secured by residential property – range",
                                                   "Secured by residential property – average"]
    assert [r[1] for r in rows(table(200))][:2] == ["US – Wilshire", "US – Dow Jones"]


def test_colspan_error_heading_1999_producer_prices():
    t = table(114)
    assert t["n_cols"] == 2 and t["n_rows"] == 8
    assert rows(t)[4][0] == "Construction > Input prices"


def test_group_closed_for_following_rows_2007_share_prices():
    assert rows(table(689))[8] == ("", "MSCI Emerging Asia", 0)


@pytest.mark.parametrize("table_id, expected", [
    (1282, ("4.1", "Financial Aggregates", "Percentage change")),   # caption typo class="sup"
    (1395, ("C.1", "Top 10 Largest Drawdowns of the Term Funding Facility by Bank", "")),
])
def test_caption_variants(table_id, expected):
    t = table(table_id)
    assert (t["table_number"], t["title"], t["subtitle"]) == expected


def test_bare_footer_text_is_kept_as_notes():
    assert table(1200)["notes"].startswith("(a)")


def test_phantom_column_from_wide_colspan_2013_forecasts():
    t = table(1068)
    assert t["n_cols"] == 6
    assert t["column_labels"] == ["June 2013", "Dec 2013", "June 2014", "Dec 2014", "Jun 2015", "Dec 2015"]
    assert cell(t, "CPI inflation", "Dec 2015", "Year-ended")["value_text"] == "1¾–2¾"
    assert cell(t, "GDP growth", "2012/13", "Year-average")["value_num"] == 2.75
