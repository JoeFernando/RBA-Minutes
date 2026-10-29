"""Financial Stability Review parsing: issue listing, chapter categories and titles."""

from rba_minutes import fsr


def test_list_issues_from_index():
    html = """<a href="/publications/fsr/2004/mar/">March 2004</a>
              <a href="/publications/fsr/2015/oct/">October 2015</a>
              <a href="/publications/fsr/2015/boxes.html">Boxes</a>
              <a href="/publications/fsr/2004/mar/">again</a>"""
    issues = fsr.list_issues(html)
    assert [i["issue_date"] for i in issues] == ["2004-03", "2015-10"]
    assert issues[0]["url"] == "https://www.rba.gov.au/publications/fsr/2004/mar/"
    assert issues[0]["era"] == "fsr"


def test_categories():
    base = "https://www.rba.gov.au/publications/fsr/"
    articles = {base + "2004/sep/asset-sec-in-aus.html"}
    cases = [
        ("Box B: Households' Mortgage Prepayment Buffers", "2012/sep/box-b.html", "box"),
        ("4.1 Focus Topic: Financial Stability Implications of AI", "2024/sep/focus-topic-ai.html", "focus_topic"),
        ("Asset Securitisation in Australia", "2004/sep/asset-sec-in-aus.html", "article"),
        ("Overview", "2010/mar/overview.html", "overview"),
        ("Financial Stability Assessment", "2024/mar/financial-stability-assessment.html", "overview"),
        ("The Macroeconomic and Financial Environment", "2005/mar/mac-fin-env.html", "global"),
        ("The Global and Macro-financial Environment", "2024/mar/x.html", "global"),
        ("Household and Business Balance Sheets", "2012/sep/house-bus-bal-sheet.html", "household_business"),
        ("Resilience of Australian Households and Businesses", "2024/mar/x.html", "household_business"),
        ("The Australian Financial System", "2012/sep/aus-fin-sys.html", "financial_system"),
        ("Developments in the Financial System Infrastructure", "2004/mar/dev-sys-infra.html", "regulation"),
        ("Regulatory Developments", "2020/oct/regulatory-developments.html", "regulation"),
        ("Copyright and Disclaimer Notices", "2020/oct/copyright.html", "administrative"),
        ("Annex: Selected Policy Responses to the COVID-19 Pandemic", "2020/apr/annex.html", "annex"),
    ]
    for title, path, expected in cases:
        assert fsr.categorise_chapter(title, base + path, articles) == expected, title


def test_clean_title():
    assert fsr._clean_title("Financial Stability Review – March 2024 2. Resilience of Australian Households") == \
        "Resilience of Australian Households"
    assert fsr._clean_title("3.1 Banks") == "Banks"


def test_overview_input_prefers_overview_over_summary_pages():
    chapters = [
        {"title": "At a Glance", "category": "overview", "text": "a"},
        {"title": "Overview", "category": "overview", "text": "b"},
        {"title": "Box A: Something", "category": "box", "text": "c"},
    ]
    assert [c["title"] for c in fsr.issue_input_rows(chapters)] == ["Overview"]
    chapters = [{"title": "In Brief", "category": "overview", "text": "a"},
                {"title": "Financial Stability Assessment", "category": "overview", "text": "b"}]
    assert [c["title"] for c in fsr.issue_input_rows(chapters)] == ["Financial Stability Assessment"]
