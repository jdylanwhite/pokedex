# Use the rvest package for scraping data
library(rvest)
library(dplyr)

# Set the URL to fetch data from
url <- "https://pokemondb.net/pokedex/gengar"

# Read the URL
read_html(url)

# Read the body frotabm the page
body <- url %>% read_html() %>% html_nodes("body")

# Get the table with the "type" information
vitalsTables <- body %>% html_nodes("table.vitals-table")
typeTable <- html_table(vitalsTables[1])