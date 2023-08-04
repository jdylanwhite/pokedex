# Load packages
library(rvest)
library(dplyr)
library(tidyr)

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
  
  # TODO: Ensure columns are fixed. Types sometimes only has 1.
  # https://stackoverflow.com/questions/45857787/adding-column-if-it-does-not-exist
  # cols <- c(top_speed = NA_real_, nhj = NA_real_, mpg = NA_real_)
  # add_column(mtcars, !!!cols[setdiff(names(cols), names(mtcars))])
  
  # TODO: Add sleep timer

  return(data_tbl)
    
}

# Get Pokemon data
pokemon_tbl <- apply(head(data_tbl),1,fetch_pokemon_data)

# TODO: Get list of tbl into one tbl
# https://stackoverflow.com/questions/61322131/convert-a-large-list-of-tibbles-into-a-single-tibble
# rbind.data.frame(pokemon_tbl)

# TODO: Merge data_tbl and pokemon_tbl