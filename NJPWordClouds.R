# Library -----------------------------------------------------------------
library(dplyr)          # For tidyverse grammar
library(tm)             # To strip stopwords
library(textclean)      # For replace_contraction()
library(stringr)        # For str_remove_all()
library(tidytext)       # For stop_words() & unnest_tokens()
library(wordcloud)      # For word clouds


# Load Data ---------------------------------------------------------------

# Read in data
df <- read.csv("data/njp-articles.csv", fileEncoding = "UTF-8-BOM")

# Read in string replacement list.
rep <- read.csv("data/rep.csv", fileEncoding = "UTF-8-BOM",
                stringsAsFactors = FALSE)


# Clean Data --------------------------------------------------------------

df <- df %>%
  mutate(text = paste(title, text)) %>%
  mutate(text = tolower(text))

for (i in 1:nrow(rep)){
  df$text <- str_replace_all(string = df$text, 
                             pattern = rep$old[i], 
                             replacement = rep$new[i])
}

df <- df %>%
  mutate(text = removeWords(text, words = stop_words$word)) %>%
  mutate(text = replace_contraction(text)) %>%
  mutate(text = str_remove_all(text, "[[:digit:]]"))

write.csv(df, "output/njp-articles-clean.csv")

# Word Clouds -------------------------------------------------------------

# Iteratively generate wordclouds for unigrams, bigrams, and trigrams.
for (i in 3:1){
  # Create tokens
  tokens <- df %>% 
    unnest_tokens("word", "text", token = "ngrams", n = i) %>%
    group_by(word) %>%
    tally(name = "freq") %>%
    arrange(desc(freq))
  
  # Create clouds
  png(file = paste0("output/wordcloud_", i, "gram.png"))
  set.seed(1956)
  wordcloud(words = tokens$word, 
            freq = tokens$freq, 
            min.freq = 3,
            max.words = 200,
            random.order = FALSE, 
            rot.per = 0,
            colors = brewer.pal(8, "Dark2"))
  dev.off()
}
