# Load packages
library(rvest)
library(dplyr)
library(tidyr)
library(tibble)

# Read Pokedex ------------------------------------------------------------

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

# Fetch Pokemon Data ------------------------------------------------------

fetch_pokemon_data <- function(row) {
  
  # Get the name from the Pokedex data tibble
  name <- row[2]
  
  # Convert name to lower case for the URL
  name <- tolower(name)
  
  # Build the URL to fetch Pokemon data
  url <- paste0("https://pokemondb.net/pokedex/",name)
  
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
  
  # Ensure columns are fixed. Types sometimes only has 1, but can be up to 3.
  cols <- c(
    `Type 1` = NA_character_, 
    `Type 2` = NA_character_, 
    `Type 3` = NA_character_
  )
  data_tbl <- add_column(data_tbl, 
                         !!!cols[setdiff(names(cols), names(data_tbl))])
  
  # Add a sleep timer to not overload the system
  Sys.sleep(1)
  
  return(data_tbl)
    
}

# Get Pokemon data
# TODO: Remove "head"
pokemon_tbl <- apply(head(data_tbl),1,fetch_pokemon_data)
pokemon_tbl <- bind_rows(pokemon_tbl)

# Merge data_tbl and pokemon_tbl
# TODO: Remove "head"
data_tbl <- cbind(head(data_tbl),pokemon_tbl)