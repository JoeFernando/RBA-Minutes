"""Find and download every RBA minutes page, caching raw HTML on disk."""

import datetime as dt
import logging
import re
import time
from pathlib import Path
from urllib.parse import urljoin, urlparse

import requests
from bs4 import BeautifulSoup
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry

from . import config

log = logging.getLogger(__name__)

# e.g. /monetary-policy/rba-board-minutes/2006/03102006.html (older pages)
#      /monetary-policy/rba-board-minutes/2024/2024-02-06.html (newer pages)
MINUTES_PATH = re.compile(
    r"^/monetary-policy/rba-board-minutes/(\d{4})/([^/]+)\.html$"
)


def make_session() -> requests.Session:
    session = requests.Session()
    session.headers["User-Agent"] = config.USER_AGENT
    retry = Retry(
        total=5,
        backoff_factor=2,
        status_forcelist=(429, 500, 502, 503, 504),
        allowed_methods=("GET",),
    )
    session.mount("https://", HTTPAdapter(max_retries=retry))
    session.mount("http://", HTTPAdapter(max_retries=retry))
    return session


def year_index_url(year: int) -> str:
    return f"{config.BASE_URL}{config.INDEX_PATH}{year}/"


def find_minutes_links(html: str, page_url: str) -> list[str]:
    """Return absolute URLs of minutes pages linked from an index page, in page order."""
    soup = BeautifulSoup(html, "lxml")
    urls = []
    for anchor in soup.find_all("a", href=True):
        url = urljoin(page_url, anchor["href"]).split("#")[0].split("?")[0]
        parsed = urlparse(url)
        if parsed.netloc and not parsed.netloc.endswith("rba.gov.au"):
            continue
        if MINUTES_PATH.match(parsed.path):
            url = f"{config.BASE_URL}{parsed.path}"
            if url not in urls:
                urls.append(url)
    return urls


def list_minutes_urls(session: requests.Session, first_year: int = config.FIRST_YEAR,
                      last_year: int | None = None) -> list[str]:
    last_year = last_year or dt.date.today().year
    found: list[str] = []
    for year in range(first_year, last_year + 1):
        index_url = year_index_url(year)
        response = session.get(index_url, timeout=config.REQUEST_TIMEOUT_SECONDS)
        if response.status_code == 404:
            log.warning("No index page for %s (%s)", year, index_url)
            continue
        response.raise_for_status()
        links = find_minutes_links(response.text, index_url)
        # Year pages can link to minutes from other years (e.g. "latest
        # minutes" panels); keep only this year's so each URL is listed once.
        links = [u for u in links if MINUTES_PATH.match(urlparse(u).path).group(1) == str(year)]
        log.info("%s: %d minutes pages", year, len(links))
        found.extend(u for u in links if u not in found)
        time.sleep(config.REQUEST_DELAY_SECONDS)
    return found


def raw_path(url: str, raw_dir: Path = config.RAW_DIR) -> Path:
    year, stem = MINUTES_PATH.match(urlparse(url).path).groups()
    return raw_dir / year / f"{stem}.html"


def fetch_minutes(session: requests.Session, url: str, raw_dir: Path = config.RAW_DIR,
                  refresh: bool = False) -> tuple[Path, bool]:
    """Download one minutes page unless it is already cached. Returns (path, downloaded)."""
    path = raw_path(url, raw_dir)
    if path.exists() and not refresh:
        return path, False
    response = session.get(url, timeout=config.REQUEST_TIMEOUT_SECONDS)
    response.raise_for_status()
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(response.content)
    return path, True


def scrape_all(first_year: int = config.FIRST_YEAR, last_year: int | None = None,
               raw_dir: Path = config.RAW_DIR, refresh: bool = False) -> list[str]:
    """List every minutes URL and download any not yet cached. Returns the URLs."""
    session = make_session()
    urls = list_minutes_urls(session, first_year, last_year)
    for url in urls:
        _, downloaded = fetch_minutes(session, url, raw_dir, refresh)
        if downloaded:
            log.info("Downloaded %s", url)
            time.sleep(config.REQUEST_DELAY_SECONDS)
    raw_dir.mkdir(parents=True, exist_ok=True)
    (raw_dir / "urls.txt").write_text("\n".join(urls) + "\n")
    return urls
