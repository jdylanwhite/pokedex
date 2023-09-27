# J. Dylan White
# Add some extra information, such as Pokemon generation and legendary status.
# This could have been done at the scraping stage, but I have already scraped
# the data and don't want to hit the server again just to add some known data.


# Load packages -----------------------------------------------------------


library(rvest)


# Read scraped pokedex data -----------------------------------------------


# Read the pokemon data
pokedex_data <- read.csv('./data/pokedex.csv',sep=',')


# Add generation data as a column -----------------------------------------


# Specify how many pokemon were in each generation
generation_count <- list(151,100,135,107,156,72,88,96,105)


# Build the list of generation IDs for each pokemon
generation_id = list()
for (i in 1:length(generation_count)) {
  generation_id <- c(generation_id,rep(i,generation_count[[i]]))
}

# Add the list as a column in the data
# TODO: Fix this, it's setting the column type as a list
pokedex_data$Generation <- generation_id


# Add legendary status as a column ---------------------------------------


url <- "https://www.serebii.net/pokemon/legendary.shtml"

# Read the body from the page
body <- url %>% read_html() %>% html_nodes("body")

# Get the info cards for each Pokemon
tables <- html_nodes(body, "table.trainer")

# Fetch the Pokemon numbers from the source in the table for sub-legendary
sub_legendary_id <- tables[1] %>%
  html_nodes("img") %>% 
  html_attr("src")
sub_legendary_id <- gsub("\\D", "",
                         sub_legendary_id[grepl("*[0-9].png", 
                                                sub_legendary_id)])
sub_legendary_id <- as.numeric(sub_legendary_id)

# Fetch the Pokemon numbers from the source in the table for legendary
legendary_id <- tables[2] %>%
  html_nodes("img") %>% 
  html_attr("src")
legendary_id <- gsub("\\D", "", legendary_id[grepl("*[0-9].png",legendary_id)])
legendary_id <- as.numeric(legendary_id)

# Fetch the Pokemon numbers from the source in the table for mythical
mythical_id <- tables[3] %>%
  html_nodes("img") %>% 
  html_attr("src")
mythical_id <- gsub("\\D", "", mythical_id[grepl("*[0-9].png", mythical_id)])
mythical_id <- as.numeric(mythical_id)

# Trim off any numbers that exceed our current pokedex. This is happening
# because Serebii has additional pokemon from a recent expansion pack that
# Pokemon DB did not yet have.
sub_legendary_id <- sub_legendary_id[sub_legendary_id < 
                                       length(pokedex_data$Number)]
legendary_id <- legendary_id[legendary_id < length(pokedex_data$Number)]
mythical_id <- mythical_id[mythical_id < length(pokedex_data$Number)]

# Build the column for legendary status
legendary_status <- rep(NA,length(pokedex_data$Number))
legendary_status[sub_legendary_id] <- "Sub-Legendary"
legendary_status[legendary_id] <- "Legendary"
legendary_status[mythical_id] <- "Mythical"

# Add legendary status as a column for the pokedex data
pokedex_data$Legendary.Status <- legendary_status


# Write data to output file -----------------------------------------------


# Write data to CSV
write.table(pokedex_data, "~/Projects/pokedex/data/pokedex_ext.csv",
            sep = ",", row.names = FALSE)
