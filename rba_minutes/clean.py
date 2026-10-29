"""Parse a raw RBA minutes page into clean, structured text."""

import datetime as dt
import re
import unicodedata
from dataclasses import dataclass, field

from bs4 import BeautifulSoup, Tag

PARSER_VERSION = "1"

# Sections that are not the board's discussion: kept in the database but
# left out of sentiment scoring.
NON_DISCUSSION = {"header", "attendance", "footnotes"}

# Checked in order; the first match wins. "International Financial Markets"
# is a markets section, so markets are tested before international.
SECTION_CATEGORIES = [
    ("decision", r"\bdecision\b"),
    ("policy_considerations", r"consideration"),
    ("attendance", r"\b(members?|present|attend|participants|apologies)\b"),
    ("footnotes", r"\b(footnotes?|references|endnotes?)\b"),
    ("outlook", r"\b(outlook|forecasts?)\b"),
    ("financial_markets", r"financial|markets?\b|bank's operations|balance sheet|liquidity"),
    ("international", r"international|global|world"),
    ("domestic", r"domestic|australia|economic|inflation|prices|wages|labour|housing|fiscal"),
]

MONTHS = "January|February|March|April|May|June|July|August|September|October|November|December"
DATE_IN_TEXT = re.compile(rf"\b(\d{{1,2}})\s+({MONTHS})\s+(\d{{4}})\b")

STRIP_TAGS = ("aside", "script", "style", "nav", "noscript", "form", "button", "svg")


@dataclass
class Paragraph:
    kind: str  # "text", "list_item" or "subheading"
    text: str


@dataclass
class Section:
    heading: str
    category: str
    paragraphs: list[Paragraph] = field(default_factory=list)

    @property
    def text(self) -> str:
        return "\n\n".join(p.text for p in self.paragraphs)


@dataclass
class Minutes:
    url: str
    meeting_date: str  # ISO date
    title: str
    location_date_line: str | None
    sections: list[Section]
    content_html: str

    @property
    def attendance_text(self) -> str:
        return "\n".join(s.text for s in self.sections if s.category in ("header", "attendance"))

    def discussion_sections(self) -> list[Section]:
        return [s for s in self.sections if s.category not in NON_DISCUSSION]

    @property
    def full_text(self) -> str:
        """Cleaned discussion text with section headings, ready for reading or NLP."""
        return "\n\n".join(f"{s.heading}\n\n{s.text}" for s in self.discussion_sections() if s.paragraphs)


def normalise_text(text: str) -> str:
    """Unicode-normalise, drop invisible characters and tidy whitespace and spacing."""
    text = unicodedata.normalize("NFKC", text)
    text = re.sub("[­​‌‍⁠﻿]", "", text)
    text = re.sub(r"\s+", " ", text).strip()
    # get_text(" ") puts a space between inline tags: "per cent ," -> "per cent,"
    text = re.sub(r"\s+([,.;:!?%)\]])", r"\1", text)
    text = re.sub(r"([(\[])\s+", r"\1", text)
    return text


def categorise(heading: str) -> str:
    lowered = heading.lower()
    for category, pattern in SECTION_CATEGORIES:
        if re.search(pattern, lowered):
            return category
    return "other"


def date_from_url(url: str) -> dt.date | None:
    stem = url.rstrip("/").rsplit("/", 1)[-1].removesuffix(".html")
    for fmt in ("%d%m%Y", "%Y-%m-%d"):
        try:
            return dt.datetime.strptime(stem, fmt).date()
        except ValueError:
            pass
    return None


def date_from_text(text: str) -> dt.date | None:
    match = DATE_IN_TEXT.search(text)
    if not match:
        return None
    return dt.datetime.strptime(" ".join(match.groups()), "%d %B %Y").date()


def _find_content(soup: BeautifulSoup) -> Tag:
    for candidate in (soup.find(id="content"), soup.find("main"), soup.body):
        if candidate is not None:
            return candidate
    raise ValueError("Page has no content element")


def _remove_noise(content: Tag) -> list[Paragraph]:
    """Strip navigation and footnote markup in place; return the footnote text."""
    for tag in content.find_all(STRIP_TAGS):
        tag.decompose()
    footnotes = []
    for el in content.select('[class*="footnote"], [id*="footnote"]'):
        if el.decomposed:
            continue
        if el.name in ("div", "section", "ol", "ul", "p", "li", "dl"):
            for item in el.find_all(["li", "p", "dd"]) or [el]:
                text = normalise_text(item.get_text(" "))
                if len(text) > 20:
                    footnotes.append(Paragraph("text", text))
        # Inline markers (e.g. <sup class="footnote-ref">) go too, so "rate1" doesn't appear.
        el.decompose()
    for sup in content.find_all("sup"):
        if sup.find("a", href=re.compile(r"^#")):
            sup.decompose()
    return footnotes


def _blocks(content: Tag):
    """Yield (kind, element) for headings and text blocks in document order, without double counting."""
    for el in content.find_all(["h1", "h2", "h3", "h4", "p", "li"]):
        if el.name == "li" and el.find(["p", "ul", "ol"]):
            continue  # its inner <p>/<li> elements are visited on their own
        yield el.name, el


def parse_minutes(html: str | bytes, url: str) -> Minutes:
    soup = BeautifulSoup(html, "lxml")
    content = _find_content(soup)
    content_html = str(content)
    footnotes = _remove_noise(content)

    title = ""
    sections = [Section("Header", "header")]
    for name, el in _blocks(content):
        text = normalise_text(el.get_text(" "))
        if not text:
            continue
        if name == "h1":
            title = title or text
        elif name == "h2":
            sections.append(Section(text, categorise(text)))
        elif name in ("h3", "h4"):
            sections[-1].paragraphs.append(Paragraph("subheading", text))
        else:
            kind = "list_item" if name == "li" else "text"
            sections[-1].paragraphs.append(Paragraph(kind, text))
    if footnotes and not any(s.category == "footnotes" for s in sections):
        sections.append(Section("Footnotes", "footnotes", footnotes))
    sections = [s for s in sections if s.paragraphs]

    header_text = " ".join(p.text for p in sections[0].paragraphs) if sections and sections[0].category == "header" else ""
    location_line = next(
        (p.text for s in sections if s.category in ("header", "attendance")
         for p in s.paragraphs if DATE_IN_TEXT.search(p.text) and len(p.text) < 200),
        None,
    )
    meeting_date = date_from_url(url) or date_from_text(location_line or header_text or title)
    if meeting_date is None:
        raise ValueError(f"Could not determine the meeting date for {url}")
    if not any(s.category not in NON_DISCUSSION for s in sections):
        raise ValueError(f"No discussion sections found in {url}")

    return Minutes(
        url=url,
        meeting_date=meeting_date.isoformat(),
        title=title,
        location_date_line=location_line,
        sections=sections,
        content_html=content_html,
    )


# --- Cash rate decision (heuristic) -------------------------------------------

_DECISION_VERB = re.compile(
    r"\b(increas\w*|rais\w*|lift\w*|lower\w*|reduc\w*|cut\w*|decreas\w*|"
    r"leav\w*|left|maintain\w*|keep\w*|kept|hold\w*)\s+(?:the\s+)?cash\s+rate",
    re.IGNORECASE,
)
_UNCHANGED = re.compile(r"cash\s+rate(?:\s+target)?\s+(?:\w+\s+){0,3}unchanged", re.IGNORECASE)
_RATE_LEVEL = re.compile(r"\b(?:to|at)\s+(\d+(?:\.\d+)?)\s*(?:per\s*cent|%)", re.IGNORECASE)
_VERB_TO_DECISION = [
    ("raise", ("increas", "rais", "lift")),
    ("lower", ("lower", "reduc", "cut", "decreas")),
    ("hold", ("leav", "left", "maintain", "keep", "kept", "hold")),
]


def split_sentences(text: str) -> list[str]:
    return [s for s in re.split(r"(?<=[.!?])\s+(?=[A-Z\"'‘“(])", text) if s.strip()]


def extract_decision(minutes: Minutes) -> tuple[str | None, float | None, str | None]:
    """Best-effort (decision, cash rate target, sentence) from the decision section.

    A regex heuristic, not ground truth: it looks for the first sentence in
    "The Decision" that says what the board decided about the cash rate.
    """
    decision_sections = [s for s in minutes.sections if s.category == "decision"]
    if not decision_sections:
        return None, None, None
    for sentence in split_sentences(decision_sections[-1].text):
        if "cash rate" not in sentence.lower():
            continue
        decision = None
        verb = _DECISION_VERB.search(sentence)
        if verb:
            word = verb.group(1).lower()
            decision = next(d for d, stems in _VERB_TO_DECISION if word.startswith(stems))
        elif _UNCHANGED.search(sentence):
            decision = "hold"
        if decision:
            level = _RATE_LEVEL.search(sentence[verb.start():] if verb else sentence)
            return decision, float(level.group(1)) if level else None, sentence
    return None, None, None
