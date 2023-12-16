# J. Dylan White
# Some initial exploratory data analysis on Pokemon stats

# Load libraries ----------------------------------------------------------

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Read and format data ----------------------------------------------------

# Read the data
pokedex <- read.csv('./data/pokedex_ext.csv',sep=',')

# Change categorical columns
pokedex$Generation <- as.factor(pokedex$Generation)
pokedex$Evolution.Place <- as.factor(pokedex$Evolution.Place)
pokedex$Has.Evolution <- as.logical(pokedex$Has.Evolution)

# Show stats by generation ------------------------------------------------

# Get the stats of Pokemon by generation
gen_stats <- pokedex %>%
  group_by(Generation) %>%
  summarize_at(vars(HP:Speed),mean) %>%
  pivot_longer(-1,names_to="Stat",values_to="Value")

# Specify Stat as a factor and set order
gen_stats$Stat <- factor(gen_stats$Stat, 
                         levels=c("HP",
                                  "Attack",
                                  "Defense",
                                  "Sp..Atk",
                                  "Sp..Def",
                                  "Speed"))

# Plot the generation stats
ggplot(gen_stats, aes(x=Generation, y=Value, fill=Stat, ymin=0)) + 
  geom_bar(stat="identity",position="dodge",width=0.7) + 
  scale_fill_brewer(palette="Dark2") + 
  theme_minimal() +
  ggtitle("Average Base Stats for Each Generation") +
  xlab("Generation") + 
  ylab("Stat Value")

gen_stats <- pokedex %>%
  group_by(Generation) %>%
  summarize_at(vars(Total),mean)

# Plot the generation stats
ggplot(gen_stats, aes(x=Generation, y=Total)) + 
  geom_bar(stat="identity",position="dodge",width=0.7) + 
  scale_fill_brewer(palette="Dark2") + 
  coord_cartesian(ylim=c(400,450)) +
  theme_minimal() +
  ggtitle(label="Average Total Base Stat for Each Generation") +
  xlab("Generation") + 
  ylab("Total Stat Value")

# Show stats by legendary status ------------------------------------------

# Get the stats of Pokemon by legendary status
legendary_stats <- pokedex %>%
  group_by(Legendary.Status) %>%
  summarize_at(vars(HP:Speed),mean) %>%
  pivot_longer(-1,names_to="Stat",values_to="Value")

# Plot the legendary status stats
ggplot(legendary_stats, aes(x=Legendary.Status, y=Value, fill=Stat, ymin=0)) + 
  geom_bar(stat="identity",position="dodge",width=0.7) + 
  scale_fill_brewer(palette="Dark2") + 
  theme_minimal() +
  ggtitle(label="Average Base Stats for Each Legendary Status") +
  xlab("Generation") + 
  ylab("Stat Value")

# Show stats by evolution status ------------------------------------------

# Get the stats of Pokemon by evolution place
# Exclude any evolution > 3, as most of those were scraped incorrectly
evolution_stats <- pokedex %>%
  filter(as.numeric(Evolution.Place) <= 3 | is.na(Evolution.Place)) %>%
  group_by(Evolution.Place) %>%
  summarize_at(vars(HP:Speed),mean) %>%
  pivot_longer(-1,names_to="Stat",values_to="Value")

# Plot the evolution place stats
ggplot(evolution_stats, aes(x=Evolution.Place, y=Value, fill=Stat, ymin=0)) + 
  geom_bar(stat="identity",position="dodge",width=0.7) + 
  scale_fill_brewer(palette="Dark2") + 
  theme_minimal() +
  ggtitle(label="Average Base Stats for Each Evolution Stage") +
  xlab("Evolution Stage") + 
  ylab("Stat Value")

# Stats by Pokemon Type ---------------------------------------------------

# Show a scatter plot of total base stats, shade by legendary status
ggplot(pokedex, aes(x=Type.1,y=Total,color=Legendary.Status, alpha=0.5)) +
  geom_point() +
  scale_fill_brewer(palette="Dark2") + 
  theme_minimal() +
  ggtitle(label="Total Base Stat for All Pokemon") +
  xlab("Type 1") + 
  ylab("Total Stat") + 
  labs(color='Legendary Status') +
  scale_alpha(guide = 'none') +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

# Get a count of of each type 
type_count <- pokedex %>%
  group_by(Type.1) %>%
  count() %>%
  arrange(desc(n))

# Show a bar plot of type counts
ggplot(type_count, aes(x = reorder(Type.1, -n),y=n, ymin=0)) + 
  geom_bar(stat="identity",position="dodge",width=0.7,fill="#81a2be") + 
  ggtitle(label="Count of Pokemon for Each Type") +
  xlab("Type 1") + 
  ylab("Count") + 
  scale_alpha(guide = 'none') +
  theme_minimal() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

# Calculate mean stats by Pokemon type
type_stats <- pokedex %>%
  group_by(Type.1) %>%
  summarize_at(vars(HP:Speed),mean) %>%
  pivot_longer(-1,names_to="Stat",values_to="Value")

# Specify Stat as a factor and set order
type_stats$Stat <- factor(type_stats$Stat, 
                          levels=c("HP",
                                   "Attack",
                                   "Defense",
                                   "Sp..Atk",
                                   "Sp..Def",
                                   "Speed"))

# Plot Pokemon Stats as a stacked bar
ggplot(data = type_stats, aes(x=Type.1, y=Value, fill=Stat, ymin=0)) +
  geom_bar(stat="identity",position="stack",width=0.7) +
  labs(title = "Average Base Stats by Type",
       y = "Stat Value", 
       x = "Type 1") +
  scale_fill_brewer(palette="Dark2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
# Plot each Pokemon Stat on an individual bar chart
ggplot(data = type_stats, aes(x=Type.1, y=Value, ymin=0)) +
  geom_bar(stat="identity",position="dodge",width=0.7,fill="#81a2be") + 
  labs(title = "Average Base Stats by Type",
       y = "Stat Value", x = "Type 1") + 
  facet_wrap(~Stat,ncol=2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_brewer(palette="Dark2")