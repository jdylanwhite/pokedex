# J. Dylan White
# Scrape the full Pokedex from Pokemon.db

# Load packages
library(rvest)
library(dplyr)
library(tidyr)

# Set the URL to fetch data from
url <- "https://pokemondb.net/pokedex/national"

# Read the URL
read_html(url)

# Read the body from the page
body <- url %>% read_html() %>% html_nodes("body")

# Get the info cards for each Pokemon
infocards <- html_nodes(body, "span.infocard-lg-data.text-muted")

# Fetch the Pokemon numbers
numbers <- infocards %>%
  html_element("small") %>%
  html_text()

# Fetch the Pokemon names
names <- infocards %>%
  html_element("a") %>%
  html_text()

# Fetch the Pokemon URLs
urls <- infocards %>%
  html_element("a") %>%
  html_attr("href")
urls <- paste0("https://pokemondb.net", urls)

# Create tibble for Pokedex
data_tbl <- tibble(numbers, names, urls) %>%
  rename(Number = numbers, Name = names, URLs = urls)
data_tbl$Number <- substring(data_tbl$Number, 2) %>% as.numeric()