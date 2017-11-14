# Sentiment Analysis

library(tidyverse)
library(billboard)
library(stringr)
library(tidytext)
library(ggridges)

lyrics <- as_data_frame(billboard::lyrics)
glimpse(lyrics)

# check on missing lyrics...
lyrics %>% 
  group_by(year) %>% 
  filter(is.na(lyrics)) %>% 
  count(year) %>% 
  ggplot(aes(x = year, y = n, group = 1)) +
  geom_line(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Year",
       y = "Count of Tracks with Missing Lyrics",
       title = "Missing Lyrics by Year",
       subtitle = "More lyrics are missing in more recent years, but the dataset contains lyrics for more than half of the songs for every year.")

# let's take a look at the format of the lyrics column...
lyrics$lyrics[1:10]

# clean up the text in the lyrics column
lyrics <- lyrics %>% 
  mutate(lyrics_clean = str_replace_all(lyrics, pattern = "VERSE|HOOK|CHORUS", replacement = ""), # remove these words
         lyrics_clean = str_replace_all(lyrics_clean, pattern = "\n", replacement = " "), # remove new line indicator
         lyrics_clean = str_replace_all(lyrics_clean, pattern = "\\[.*?\\]", replacement = ""), # remove anything in brackets
         lyrics_clean = str_replace_all(lyrics_clean, pattern = "\\'", replacement = ""))

# now, let's check...
lyrics$lyrics_clean[1:10]

# looks better! now let's tidy things up...
lyrics_tidy <- lyrics %>% 
  select(-lyrics) %>% 
  unnest_tokens(output = word, input = lyrics_clean, token = "words")
lyrics_tidy$year <- factor(lyrics_tidy$year)


# and now for some sentiment analysis usinf AFINN lexicon (-5, 5)...
# most negative contribution
lyrics_tidy %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(year, artist, word, score) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(year, artist) %>% 
  mutate(contribution = (score*count) / sum(count)) %>% 
  arrange(contribution)

# most positive contribution
lyrics_tidy %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(year, word, score) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(contribution = (score*count) / sum(count)) %>% 
  arrange(year, desc(contribution))
# consider removing some words from sentiment contribution

lyrics_tidy %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(year, artist, title, word, score) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(year, artist, title) %>% 
  mutate(contribution = (score*count) / sum(count)) %>% 
  summarize(song_score = mean(contribution)) %>% 
  ggplot(aes(x = song_score, y = reorder(year, desc(year)))) +
  geom_density_ridges(scale = 4, alpha = 0.8) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits=c(-0.25, 0.35)) +
  theme_minimal() +
  labs(x = "Song Sentiment Score",
       y = "",
       title = "Song Sentiment Score Distribution by Year")

lyrics_tidy %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(year, word, score) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(contribution = (score*count) / sum(count)) %>% 
  summarize(yearly_score = sum(contribution)) %>% 
  ggplot(aes(x = year, y = yearly_score, group = 1)) +
  geom_line() +
  geom_smooth() +
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Year",
       y = "Sentiment Score",
       title = "Total Lyrical Sentiment (1960-2016)")

# Which songs are the most positive and negative by year?
lyrics_tidy %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(year, artist, title, word, score) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(year, artist, title) %>% 
  mutate(contribution = (score*count) / sum(count)) %>% 
  summarize(song_score = mean(contribution)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  top_n(1, song_score)

# Flaws: sometimes, sentiment scores for individual words aren't correct or words are missing ("Unsteady")
# try other sentiment lexicon ("bing")
lyrics_sentiment_ratios <- lyrics_tidy %>% 
  inner_join(get_sentiments("bing")) %>% 
  group_by(year, artist, word, sentiment) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(year, sentiment) %>% 
  summarize(total = sum(count)) %>% 
  mutate(prop = total/sum(total)) %>% 
  arrange(year) 

lyrics_sentiment_ratios %>% 
  ggplot(aes(x = year, y = prop, group = sentiment, color = sentiment)) +
  geom_line(size = 1) + 
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "top",
        legend.title = element_blank()) +
  labs(x = "Year",
       y = "Proportion",
       title = "Positive vs. Negative Lyrical Sentiment (1960-2016)",
       subtitle = "Bing Lexicon")



  