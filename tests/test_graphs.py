"""Graph references in RBA pages and the graph-data workbooks."""

import datetime as dt

import openpyxl

from rba_minutes import graphs


def test_find_graphs_in_each_layout():
    bulletin = b'<div id="content"><img src="images/graph-0297-1.gif" alt="Graph 1: Real GDP Growth"></div>'
    g = graphs.find_graphs(bulletin, "https://www.rba.gov.au/publications/bulletin/1997/feb/1.html")
    assert (g[0]["number"], g[0]["title"]) == ("1", "Real GDP Growth")
    assert g[0]["image_url"] == "https://www.rba.gov.au/publications/bulletin/1997/feb/images/graph-0297-1.gif"
    chapter = b'<div id="content"><a href="graphs/graph-2.1.html">Graph 2.1</a><img src="/icons/print.png"></div>'
    g = graphs.find_graphs(chapter, "https://www.rba.gov.au/publications/smp/2012/feb/eco-dev.html")
    assert len(g) == 1 and g[0]["graph_page"] == "https://www.rba.gov.au/publications/smp/2012/feb/graphs/graph-2.1.html"
    modern = (b'<div id="content"><figure><figcaption>Graph 2.1 Oil Prices</figcaption>'
              b'<img src="images/graph-2-1.svg" alt="A line chart showing oil prices &amp;amp; fuel."></figure></div>')
    g = graphs.find_graphs(modern, "https://www.rba.gov.au/publications/smp/2026/aug/economic-conditions.html")
    assert g[0]["number"] == "2.1" and g[0]["title"] == "Oil Prices"
    assert g[0]["description"] == "A line chart showing oil prices & fuel."


def test_svg_text_title_is_largest_text():
    svg = (b'<svg xmlns="http://www.w3.org/2000/svg"><text font-size="9">ABS;RBA</text>'
           b'<text font-size="14" font-weight="bold">BrentOilPrice</text><text font-size="9">US$/bbl</text></svg>')
    title, text = graphs._svg_text(svg)
    assert title == "Brent Oil Price"
    assert "US$/bbl" in text


def test_parse_graph_data(tmp_path):
    wb = openpyxl.Workbook()
    wb.active.title = "Disclaimer"
    ws = wb.create_sheet("Chapter 1")
    ws.append(["Publication", None, "Statement on Monetary Policy, August 2026"])
    ws.append(["Graph number", None, 1.1, None, 1.2, None, None])
    ws.append(["Graph title", None, "Restricted Graph", None, "Neutral Rate", None, None])
    ws.append(["Source", None, "LSEG", None, "RBA", None, None])
    ws.append(["Panel", None, None, None, None, None, None])
    ws.append(["Data", None, "Data restricted", None, "Date", "Cash rate", "Model"])
    ws.append([None, None, None, None, dt.datetime(2026, 1, 31), 3.85, 3.1])
    ws.append([None, None, None, None, dt.datetime(2026, 2, 28), 3.85, None])
    path = tmp_path / "smp-graph-data-2026-08.xlsx"
    wb.save(path)
    values, restricted = graphs.parse_graph_data(path, "2026-08")
    assert [r["graph_title"] for r in restricted] == ["Restricted Graph"]
    assert [(v["graph_number"], v["series"], v["x"], v["value"]) for v in values] == [
        ("1.2", "Cash rate", "2026-01-31", 3.85), ("1.2", "Cash rate", "2026-02-28", 3.85),
        ("1.2", "Model", "2026-01-31", 3.1)]
    assert values[0]["x_label"] == "Date" and values[0]["source"] == "RBA"
