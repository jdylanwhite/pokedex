# J. Dylan White
# Scrape information for all Pokemon from Pokemon.db


# Load Packages -----------------------------------------------------------


# Load packages
library(rvest)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)


# Read Pokedex ------------------------------------------------------------


fetch_pokedex_data <- function() {

  # Set the URL to fetch data from
  url <- "https://pokemondb.net/pokedex/national"

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

  return(data_tbl)

}

pokedex_tbl <- fetch_pokedex_data()


# Fetch Pokemon Data ------------------------------------------------------


fetch_pokemon_data <- function(row) {

  # Fetch the name and URL from the Pokedex data
  name <- row[2]
  url <- row[3]
  print(paste0(name, row[1]))

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

  # Ensure columns are fixed. Types sometimes only has 1, but can be up to 3.
  cols <- c(
    `Type 1` = NA_character_,
    `Type 2` = NA_character_,
    `Type 3` = NA_character_
  )
  data_tbl <- add_column(data_tbl,
                         !!!cols[setdiff(names(cols), names(data_tbl))])

  # Look for evolution information
  evo_node <- html_nodes(body, "div.infocard-list-evo")

  # Check to see if there was any evolution information
  has_evo <- length(evo_node) >= 1

  # If there was evolution information
  if (has_evo) {

    # Get the list of evolutions
    evo_list <- evo_node %>% html_nodes("a.ent-name") %>% html_text

    # Get the maximum number of evolutions for this Pokemon's evolution chain
    max_evo <- length(unique(evo_list))

    # Find out where in the evolution chain this Pokemon sits
    evo_place <- which(tolower(evo_list) == tolower(name))[1]

    # Calculate an evolution index, how far to max evolution the Pokemon is
    evo_index <- round(as.double(evo_place) / as.double(max_evo), 2)

    # Otherwise, assume there is not evolution of this Pokemon
  } else {

    # Set the evolution information to NA
    max_evo <- NA_integer_
    evo_place <- NA_integer_
    evo_index <- NA_integer_

  }

  # Append evolution information to the data tibble
  evo_list <- c(
    `Has Evolution` = has_evo,
    `Evolution Place` = evo_place,
    `Maximum Evolution Count` = max_evo,
    `Evolution Index` = evo_index
  )
  evo_tbl <- evo_list %>%
    t %>%
    as_tibble
  data_tbl <- cbind(data_tbl, evo_tbl)

  # Add a sleep timer to not overload the system
  Sys.sleep(1)

  return(data_tbl)

}

# Get Pokemon data
pokemon_tbl <- apply(pokedex_tbl, 1, fetch_pokemon_data) %>%
  bind_rows()

# Merge pokedex_tbl and pokemon_tbl
pokemon_tbl <- cbind(pokedex_tbl, pokemon_tbl)


# Clean data --------------------------------------------------------------


clean_pokemon_data <- function(data_tbl) {

  # Clean up Height field to only show meters
  data_tbl$Height <- data_tbl$Height %>%
    str_extract("\\d+\\.*\\d*") %>%
    as.numeric

  # Clean up Weight field to only show kilograms
  data_tbl$Weight <- data_tbl$Weight %>%
    str_extract("\\d+\\.*\\d*") %>%
    as.numeric

  return(data_tbl)

}

pokemon_tbl <- clean_pokemon_data(pokemon_tbl)


# Write data to output file -----------------------------------------------


# Write data to CSV
write.table(pokemon_tbl, "~/Projects/pokedex/data/pokedex.csv",
            sep = ",", row.names = FALSE)