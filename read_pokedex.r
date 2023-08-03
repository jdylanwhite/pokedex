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
infocards <- html_nodes(body,"span.infocard-lg-data.text-muted")

# Fetch the Pokemon numbers and names from the info card
numbers <- html_text(html_element(infocards,"small"))
names <- html_text(html_element(infocards,"a"))

# Create tibble for Pokedex
data_tbl <- tibble(numbers,names) %>% rename(Number=numbers,Name=names)
data_tbl$Number <- substring(data_tbl$Number,2) %>% as.numeric()