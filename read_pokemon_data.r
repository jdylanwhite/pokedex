# Use the rvest package for scraping data
library(rvest)
library(dplyr)

# Set the URL to fetch data from
url <- "https://pokemondb.net/pokedex/gengar"

# Read the URL
read_html(url)

# Read the body from the page
body <- url %>% read_html() %>% html_nodes("body")

# Get the table with the "type" information
vitalsTables <- body %>% html_nodes("table.vitals-table")
mainTable <- html_table(vitalsTables[1])[[1]]

# Get the pokemon types from the table
types <- mainTable[mainTable$X1 == "Type",2] %>% pull %>% strsplit(" ")
types <- types[[1]]

# Get the height and weight from the table
height <- mainTable[mainTable$X1 == "Height",2] %>% pull
weight <- mainTable[mainTable$X1 == "Weight",2] %>% pull

# Put together the main data table for the pokemon
columnNames <- c("Weight","Height", paste("Type", c(1:length(types))))
dataTbl <- c(weight,height,types) %>% t %>% as_tibble
names(dataTbl) <- columnNames

# TODO: Build stats columns

# TODO: Build evolution columns