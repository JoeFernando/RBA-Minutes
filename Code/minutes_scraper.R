# Libraries ---------------------------------------------------------------
require(RCurl); require(XML)


# Globals -----------------------------------------------------------------
BASE_URL    <- "http://www.rba.gov.au"
MINUTES_URL <- "monetary-policy/rba-board-minutes"
DATA_PATH   <- "./Data"


# Functions ---------------------------------------------------------------
get_minutes_links <- function(year){
  # Get and parse URL
  link <- paste(BASE_URL, MINUTES_URL, year, "", sep = "/")
  html_doc <- htmlParse(getURL(link))
  
  # Get relevant links from document
  all_links <- getHTMLLinks(html_doc)
  link_pattern <- paste(MINUTES_URL, year, "(.*)(html)$", sep = "/")
  minutes_links <- regmatches(all_links, regexpr(link_pattern, all_links))
  
  return(minutes_links)
}

download_minutes <- function(link){
  # Get URL
  link <- paste(BASE_URL, link, sep = "/")
  html_doc <- getURL(link)
  
  # Define filename
  filename <- gsub(paste(BASE_URL, MINUTES_URL, "", sep = "/"), "", link)
  filename <- gsub("/", "-", filename)
  
  # Save to data directory
  writeLines(html_doc, paste(DATA_PATH, filename, sep = "/"))
  
  return(TRUE)
}

parse_minutes_file <- function(filename){
  # Get and parse the URL
  html_doc <- readLines(paste(DATA_PATH, filename, sep = "/"))
  html_doc <- htmlParse(html_doc)
  
  # Parse the relevant data
  loc_date <- xpathSApply(html_doc, "//div[@id='content']//strong", xmlValue)
  headings <- xpathSApply(html_doc, "//div[@id='content']//h2", xmlValue)
  content <- xpathSApply(html_doc, "//div[@id='content']//p", xmlValue)
  
  # Combine into a single row data frame
  df <- data.frame(
    Location = loc_date,
    Headings = paste(headings, collapse = "\r\n"),
    Text     = paste(content, collapse = "\r\n")
  )
  
  return(df)
}


# Main --------------------------------------------------------------------
# Scrape
minutes_links <- unlist(sapply(2006:2016, get_minutes_links))
sapply(minutes_links, download_minutes)

# Parse
minutes_files <- dir(DATA_PATH)
minutes_texts <- lapply(minutes_files, parse_minutes_file)

# Combine
df <- do.call("rbind", minutes_texts)