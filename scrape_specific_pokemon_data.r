# J. Dylan White
# Read information for a specific Pokemon from that Pokemon's main page
#   on Pokemon.db (e.g., https://pokemondb.net/pokedex/gengar).


# Load Packages -----------------------------------------------------------


# Load packages
library(rvest)
library(dplyr)
library(tidyr)
library(tibble)


# Set Parameters ----------------------------------------------------------


# Set the name of the Pokemon
name <- "Tauros"

# Convert name to lower case for the URL
name <- tolower(name)

# Build the URL to fetch Pokemon data
url <- paste0("https://pokemondb.net/pokedex/", name)


# Parse Main Tables -------------------------------------------------------



# Read the body from the page
body <- url %>% read_html() %>% html_nodes("body")

# Get the tables with the vital information
vitals_tables <- html_nodes(body, "table.vitals-table")
main_table <- html_table(vitals_tables[1])[[1]]
stats_table <- html_table(vitals_tables[4])[[1]]


# Get Main Stats ----------------------------------------------------------


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


# Get Type Information ----------------------------------------------------


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


# Get Evolution Information -----------------------------------------------


# Look for evolution information
evo_node <- html_nodes(body, "div.infocard-list-evo")

# Check to see if there was any evolution information
has_evo <- length(evo_node) >= 1

# If there was evolution information, fill out the data
# Otherwise, assume there is not evolution of this Pokemon
if (has_evo) {

  # Get the list of evolutions
  evo_list <- evo_node %>% html_nodes("a.ent-name") %>% html_text

  # Get the maximum number of evolutions for this Pokemon's evolution chain
  max_evo <- length(unique(evo_list))

  # Find out where in the evolution chain this Pokemon sits
  evo_place <- which(tolower(evo_list) == name)[1]

  # Calculate an evolution index, how far to max evolution the Pokemon is
  evo_index <- round(as.double(2) / as.double(max_evo), 2)

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
evo_tbl <- evo_list %>% t %>% as_tibble
data_tbl <- cbind(data_tbl, evo_tbl)