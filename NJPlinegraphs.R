# Library -----------------------------------------------------------------
library(tidytext)                # For stop_words()
library(dplyr)                   # For tidyverse grammar
library(tidyr)                   # For drop_na()
library(anytime)                 # For date manipulation
source("r/unlist_column.R")      # For data manipulation
library(ggplot2)                 # For plotting

# Load Data ---------------------------------------------------------------

df <- read.csv("output/njp-articles-clean.csv", stringsAsFactors = FALSE)
cstops <- readLines("data/customstops.txt")

# Clean Data --------------------------------------------------------------

# Update stopwords.
stops <- c(stop_words$word, cstops)

# Get timestamps associated with each word/freq pairing.
full <- NULL
for (i in 1:nrow(df)){
  temp <- df[i,] %>% 
    unnest_tokens("word", "text") %>%
    group_by(word) %>%
    tally(name = "freq")
  temp$date <- df$date[i]
  full <- rbind(full, temp)
}

# Get the top 10 tokens (minus stop words) per day.
tokens_by_day <- full %>%
  group_by(word) %>%
  tally(freq) %>%
  arrange(desc(n)) %>%
  filter(!word %in% stops) %>%
  top_n(10, wt = n) %>%
  inner_join(full, by = "word") %>%
  select(word, freq, date) %>%
  mutate(date = as.POSIXct(date, format = "%B %d, %Y"))

# Create records for tokens with missing date values.
dates <- unique(tokens_by_day$date)
missing <- tokens_by_day %>%
  distinct(word, date) %>%
  group_by(word) %>%
  summarise(date = list(anytime(setdiff(dates, date)))) %>%
  drop_na(date)

# Create "freq = 0" records for missing token/date combinations.
insert <- missing %>%
  unlist_column("date") %>%
  mutate(freq = 0) %>%
  select(word, freq, date)

# Insert the columns.
tokens_by_day <- rbind(tokens_by_day, insert)


# Plot --------------------------------------------------------------------

ggplot(tokens_by_day, aes(x = date, y = freq, color = word)) +
  geom_line(size = .75) +
  geom_point(color = "black") +
  labs(title = "Top Ten Words in NJP articles",
       y = "Frequency of occurence per article",
       x = "Date")
