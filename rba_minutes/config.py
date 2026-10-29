from pathlib import Path

BASE_URL = "https://www.rba.gov.au"
INDEX_PATH = "/monetary-policy/rba-board-minutes/"

# The RBA began publishing minutes with the October 2006 meeting, so the
# 2006 index page is the earliest one that exists.
FIRST_YEAR = 2006

PROJECT_ROOT = Path(__file__).resolve().parent.parent
DATA_DIR = PROJECT_ROOT / "data"
RAW_DIR = DATA_DIR / "raw"
EXPORT_DIR = DATA_DIR / "exports"
DB_PATH = DATA_DIR / "rba_minutes.sqlite"

USER_AGENT = (
    "rba-minutes-research/1.0 "
    "(+https://github.com/JoeFernando/RBA-Minutes; academic text analysis)"
)
REQUEST_DELAY_SECONDS = 1.0
REQUEST_TIMEOUT_SECONDS = 30
