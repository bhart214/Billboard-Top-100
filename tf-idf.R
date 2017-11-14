# TF-IDF

library(tidyverse)
library(billboard)
library(stringr)
library(tidytext)
library(ggridges)

lyrics <- as_data_frame(billboard::lyrics)
glimpse(lyrics)


# clean up the text in the lyrics column
lyrics <- lyrics %>% 
  mutate(lyrics_clean = str_replace_all(lyrics, pattern = "VERSE|HOOK|CHORUS", replacement = ""), # remove these words
         lyrics_clean = str_replace_all(lyrics_clean, pattern = "\n", replacement = " "), # remove new line indicator
         lyrics_clean = str_replace_all(lyrics_clean, pattern = "\\[.*?\\]", replacement = ""), # remove anything in brackets
         lyrics_clean = str_replace_all(lyrics_clean, pattern = "\\'", replacement = ""))

# looks better! now let's tidy things up...
lyrics_tidy <- lyrics %>% 
  select(-lyrics) %>% 
  unnest_tokens(output = word, input = lyrics_clean, token = "words") %>% 
  filter(!is.na(word))

lyrics_tidy$year <- factor(lyrics_tidy$year)

# add column for decade...
lyrics_tidy$year <- as.numeric(as.character(lyrics_tidy$year))

lyrics_tidy <- lyrics_tidy %>% 
  mutate(decade = case_when(
    year < 2000 ~ paste0(floor(((year - 1900)/10)) * 10, "s"),
    year >= 2000 ~ paste0(floor(((year - 2000)/10)) * 10, "s")
  )
  )

# set decade as factor
lyrics_tidy$decade <- factor(lyrics_tidy$decade, 
                             levels = c("60s", "70s", "80s", "90s", "0s", "10s"))



## create dataframe of individual word counts by decade and total words by decade
# count the words by decade...
total_words_by_decade <- lyrics_tidy %>% 
  group_by(decade) %>% 
  summarize(total_words = n())
  
# count individual words by decade...
unique_words_by_decade <- lyrics_tidy %>% 
count(decade, word, sort = TRUE) %>%
  ungroup() %>% 
  arrange(decade)

# join the two dfs above
tfidf_data <- unique_words_by_decade %>% 
  left_join(total_words_by_decade)

tfidf_data


# Calculate tf-idf
tfidf_data <- tfidf_data %>%
  bind_tf_idf(word, decade, n) %>% 
  arrange(decade) %>% 
  group_by(decade) %>% 
  top_n(10, wt = tf_idf)

tfidf_data %>% 
  ggplot(aes(x = reorder(word, -tf_idf), y = tf_idf, fill = decade)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ decade, scales = "free_x") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "none") +
  labs(title = "Highest TF-IDF Words from Top 100 Song Lyrics by Decade",
       x = "",
       y = "tf-idf")




