# Load packages
library(rvest)
library(dplyr)
library(tidyr)

# Set the URL to fetch data from
url <- "https://pokemondb.net/pokedex/gengar"

# Read the URL
read_html(url)

# Read the body from the page
body <- url %>% read_html() %>% html_nodes("body")

# Get the tables with the vital information
vitals_tables <- html_nodes(body, "table.vitals-table")
main_table <- html_table(vitals_tables[1])[[1]]
stats_table <- html_table(vitals_tables[4])[[1]]

# Function to help us turn the first row of our tibble into the header
header_from_row <- function(df) {
  names(df) <- as.character(unlist(df[1, ]))
  df[-1, ]
}

# Get the types, species, height, and weight from the table
main_tbl <- main_table %>%
  t %>%
  as_tibble %>%
  header_from_row %>%
  select(c("Type", "Species", "Height", "Weight"))

# Get stats columns
stats_tbl <- stats_table %>%
  select(c("X1", "X2")) %>%
  t %>%
  as_tibble %>%
  header_from_row

# Merge the data
data_tbl <- cbind(main_tbl, stats_tbl)

# Clean up the types by breaking them into different columns
num_types <- data_tbl$Type %>% strsplit(" ") %>% unlist %>% length
col_names <- paste("Type", c(1:num_types))
data_tbl <- data_tbl %>%
  separate(col = "Type", into = col_names)

# TODO: Build evolution columns