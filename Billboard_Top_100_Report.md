Billboard Top 100 (1960-2015)
================
Brian Hart
November 12, 2017

The Problem
-----------

Pop music is one of the rare art forms that is consumed by everyone. Whether you'd describe yourself as a "music person" or not, it would be pretty difficult to make your way through the world without being at least somewhat familiar with the works of Madonna, Elton John, Michael Jackson, Usher, or Beyonce.

Like all art forms, pop music undergoes constant innovation -- each decade has its own defining characteristics as evidenced by Spotify playlists like *\[insert decade here\] Smash Hits*. It's undeniable though that pop music can often be a unifying force, a reflection of the times and culture.

The goal of this analysis it to investigate musical trends over the last 50+ years and to identify ways in which music might be evolving. The analysis is exploratory in nature, and it gives me a good excuse to experiment with some new tools.

The Data
--------

Github user "mikkelkrogsholm" has open-sourced an R package `billboard` that contains several interesting datasets:

-   wiki\_hot\_100s: A data set containing 57 years of Billboards Hot 100 songs. The data is scraped from Wikipedia.
-   lyrics: A data set containing lyrics for songs on the Billboard Hot 100 over the past 57 years. The lyrics were identified and collected by webscraping so there might be some errors and mistakes - have that in mind.
-   spotify\_track\_data: contains features from all tracks using Spotify playlists

I owe a big thanks to "mikkelkrogsholm" for putting this package together. With the package, we can skip right over the data-gathering phase and get to the fun part!

Preliminary Analysis
--------------------

The `spotify_track_data` dataset contains Spotify audio features for each track that made the Billboard Top 100 from 1960 to 2016. For more information about each of the features, see <https://developer.spotify.com/web-api/get-audio-features/>.

#### Load Packages

I'll start out by loading a few packages that will come in handy for the analysis:

``` r
library(tidyverse)
library(billboard)
library(stringr)
library(tidytext)
library(ggridges)
library(ggrepel)
```

#### Top Artists

First, let's take a look at the artists that show up with the highest frequency. It could be argued that these are some of the most influential artists...

``` r
spotify_track_data <- billboard::spotify_track_data

# top 30 artists
spotify_track_data %>% 
  group_by(artist_name) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(30) %>% 
  ggplot(aes(x = reorder(artist_name, -n), y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = 1, color = "white") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "",
       y = "Count of Top 100 Hits", 
       title = "Top 30 Artists with the Most Billboard Top 100 Tracks")
```

![](Billboard_Top_100_Report_files/figure-markdown_github-ascii_identifiers/top%20musicians-1.png)

The graph above shouldn't be viewed as a rigid ranking because some of the artists in the graph above are still at the height of their careers and may continue to produce popular tracks. It also simply counts the number of Billboard Top 100 hits and fails to incorporate any weighting for where on the list a track falls. For example, one may argue that a single \#1 hit is more impressive than multiple hits that don't break the top 25 for the year. However, the graph does provide a pretty good representation of the most successful and influential artists over the past 5 or 6 decades.

#### Evolution of Spotify Audio Features

Now let's take a look at how the music has evolved over time. We can use the Spotify audio features for this.

``` r
# spotify song metrics
spotify_track_data %>% 
  select(year,
         artist_name,
         track_name,
         explicit,
         danceability,
         energy,
         key,
         loudness,
         mode,
         speechiness,
         acousticness,
         instrumentalness,
         liveness,
         valence,
         tempo,
         duration_ms,
         time_signature) %>% 
  gather(key = measure, value = score, explicit:time_signature) %>% 
  group_by(year, measure) %>% 
  summarize(avg_score = mean(score)) %>% 
  filter(measure %in% c("explicit", "danceability", "energy", "speechiness",
                        "acousticness", "instrumentalness", "liveness", "valence")) %>% 
  ggplot(aes(x = year, y = avg_score, color = measure, group = measure)) +
  geom_line(size = 1) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Year",
       y = "Average Score",
       title = "Billboard Top 100: Average Trends in Spotify Metrics")
```

![](Billboard_Top_100_Report_files/figure-markdown_github-ascii_identifiers/track%20feature%20evolution-1.png)

The graph above shows the *average* trend for each audio feature. Based on this graph, we can make several observations:

-   Acousticness, a feature describing the confidence of whether or not a track is acoustic, decreased steeply throughout the 60s and has continued a downward trend through about 2011. In recent years, it appears that there has been a slight increase in acoustic tracks in the Top 100.
-   Danceability, a rating of whether the song is suitable for dancing, has always been highly associated with tracks in the Top 100, and it has only become more and more characteristic of popular music as time has gone on.
-   Energy, which Spotify describes as relating to tracks that feel fast, loud, and noisy, appears to be fairly highly correlated with danceability and has increased steadily over the years but has plateaued in the most recent decade.
-   Explicit simply describes the proportion of tracks that contain explicit lyrics. The Billboard Top 100 saw very few explicit tracks until the early 90s, and the proportion peaked in the early- to mid-2000s at about 30%.
-   Instrumentalness is associated with tracks that contain no vocals. As expected, this feature started out low and steadily decreased to near zero for most years in the past 3 or 4 decades.
-   Speechiness, which corresponds to spoken words in tracks, has increased in the last decade or so, possibly due to the increased popularity of rap music in the Top 100.
-   Valence is an interesting feature intended to describe the general positivity of a track. Valence appears to have decreased slightly over the years in the dataset.

We could do a much deeper dive and look at specific artists that contribute to each of these features, but I'll leave that for another time. Now that we have a general understanding of the Spotify data and trends over time, I'd like to look more closely at the lyrics.

Lyrics Sentiment Analysis
-------------------------

I'm curious about what we can learn from the actual lyrics of each track. Simple sentiment analysis is notoriously finicky when it comes to small corpuses (corpi??), but it will be interesting to see how it works! It will also give me a good excuse to try out the highly acclaimed `tidytext` package created by Julia Silge and David Robinson.

First, I will load the dataset and do some cleaning to remove some of the excess information that isn't helpful for this analysis.

``` r
# load lyrics dataset
lyrics <- as_data_frame(billboard::lyrics)

# clean up the text in the lyrics column
lyrics <- lyrics %>% 
          # remove a few specific words
  mutate(lyrics_clean = str_replace_all(lyrics, pattern = "VERSE|HOOK|CHORUS", replacement = ""),
          # remove new line indicator 
         lyrics_clean = str_replace_all(lyrics_clean, pattern = "\n", replacement = " "),
          # remove anything in brackets
         lyrics_clean = str_replace_all(lyrics_clean, pattern = "\\[.*?\\]", replacement = ""), 
          # remove apostrophe
         lyrics_clean = str_replace_all(lyrics_clean, pattern = "\\'", replacement = ""))
```

#### Tidying

Now we want to tidy this dataset -- instead of just having big chunks of text, we would like a column with only single words. This will make our dataframe much longer, and the lyrics will be more difficult to read, but we aren't interested in the entire set of lyrics for each track; we want to focus on words because simple sentiment analysis relies on the connotation of single words.

The `tidytext::unnest_tokens` function makes this easy...

``` r
# looks better! now let's tidy things up...
lyrics_tidy <- lyrics %>% 
  select(-lyrics) %>% 
  unnest_tokens(output = word, input = lyrics_clean, token = "words")
lyrics_tidy$year <- factor(lyrics_tidy$year)

head(lyrics_tidy)
```

    ## # A tibble: 6 x 4
    ##                       title      artist   year   word
    ##                       <chr>       <chr> <fctr>  <chr>
    ## 1 Theme from A Summer Place Percy Faith   1960 theres
    ## 2 Theme from A Summer Place Percy Faith   1960      a
    ## 3 Theme from A Summer Place Percy Faith   1960 summer
    ## 4 Theme from A Summer Place Percy Faith   1960  place
    ## 5 Theme from A Summer Place Percy Faith   1960  where
    ## 6 Theme from A Summer Place Percy Faith   1960     it

Above, we can see the first 6 records of the transformed dataset.

#### Obtaining Sentiment

Now, we need to apply some sort of sentiment score to each word. The `tidytext` package contains 3 built-in sentiment lexicons:

-   AFINN: assigns scores from -5 to +5
-   bing: binary classification of words as positive or negative
-   nrc: assigns words to 1 or more of several categories (anger, joy, fear, sadness, etc.)

For more information on `tidytext` sentiment lexicons, see the following excellent online book created by the authors of the package: <http://tidytextmining.com/sentiment.html>

I'll try out the "bing" lexicon for this project because I'm really just interested in the proportion of positive to negative lyrical sentiment for each year. It's important to note that not every word will have a sentiment score. If fact, most words are pretty neutral and will be ignored for sentiment analysis.

To match the new "word" column with sentiment ratings, we can just use `dplyr:inner_join`. This will remove any words that are not represented in the bing sentiment lexicon.

``` r
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
```

![](Billboard_Top_100_Report_files/figure-markdown_github-ascii_identifiers/lyrics%20sentiment-1.png)

The plot above shows the proportion of words with positive and negative sentiment for all years in the `lyrics` dataset. The proportions are highly variable from year to year, but there appears to be a general downward proportion of positive sentiment starting in about 1986. There is actually an interesting peak of positive sentiment in 1985 and a low point in which the proportion of negative sentiment actually exceeds that of positive sentiment in 2005. These years require closer inspection!

#### A Deeper Dive into Artists and Words

We can use the same code from above with a few changes to zero in on the artists and tracks from each year...

``` r
lyrics_tidy %>% 
  inner_join(get_sentiments("bing")) %>% 
  group_by(year, artist, word, sentiment) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(year, artist, sentiment) %>% 
  summarize(total = sum(count)) %>% 
  mutate(prop = total/sum(total)) %>% 
  filter(year == 1985,
         sentiment == "positive") %>% 
  arrange(desc(prop))
```

    ## # A tibble: 62 x 5
    ## # Groups:   year, artist [62]
    ##      year                    artist sentiment total      prop
    ##    <fctr>                     <chr>     <chr> <int>     <dbl>
    ##  1   1985                   Chicago  positive    13 1.0000000
    ##  2   1985        Harold Faltermeyer  positive     1 1.0000000
    ##  3   1985         The Honeydrippers  positive    11 1.0000000
    ##  4   1985     Katrina and the Waves  positive    36 0.9729730
    ##  5   1985           Whitney Houston  positive    49 0.9607843
    ##  6   1985               Billy Ocean  positive    47 0.9591837
    ##  7   1985           Kool & the Gang  positive   156 0.9397590
    ##  8   1985 Prince and The Revolution  positive    14 0.9333333
    ##  9   1985                     Heart  positive    13 0.9285714
    ## 10   1985         Ashford & Simpson  positive    69 0.9200000
    ## # ... with 52 more rows

``` r
lyrics_tidy %>% 
  inner_join(get_sentiments("bing")) %>% 
  group_by(year, artist, word, sentiment) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(year, artist, sentiment) %>% 
  summarize(total = sum(count)) %>% 
  mutate(prop = total/sum(total)) %>% 
  filter(year == 2005,
         sentiment == "positive") %>% 
  arrange(desc(prop))
```

    ## # A tibble: 48 x 5
    ## # Groups:   year, artist [48]
    ##      year              artist sentiment total      prop
    ##    <fctr>               <chr>     <chr> <int>     <dbl>
    ##  1   2005          Faith Hill  positive     3 1.0000000
    ##  2   2005 Natasha Bedingfield  positive    58 0.9508197
    ##  3   2005      Jennifer Lopez  positive    36 0.9230769
    ##  4   2005          John Mayer  positive    19 0.9047619
    ##  5   2005             Rihanna  positive     6 0.8571429
    ##  6   2005               Mario  positive    40 0.8333333
    ##  7   2005              Weezer  positive     9 0.8181818
    ##  8   2005        3 Doors Down  positive    19 0.7600000
    ##  9   2005        Foo Fighters  positive    49 0.7205882
    ## 10   2005          Toby Keith  positive    18 0.7200000
    ## # ... with 38 more rows

There are definitely more tracks in 1985 with higher proportions of positive sentiment. It's also important to remember that these proportions of positive and negative sentiment are dependent on the bing lexicon classification. A thorough analysis should always inspect the actual words to make sure that the classifications make sense in the context of music.

Let's look at the top 20 positive sentiment words and top 20 negative sentiment words in the corpus by frequency.

``` r
lyrics_tidy %>% 
  inner_join(get_sentiments("bing")) %>% 
  group_by(year, artist, word, sentiment) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(word, sentiment) %>% 
  summarize(total = sum(count)) %>% 
  ungroup() %>% 
  group_by(sentiment) %>% 
  top_n(n = 20, wt = total) %>% 
  ggplot(aes(x = reorder(word, total), y = total, fill = sentiment)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~ sentiment, scales = "free") +
  labs(x = "Word Frequency",
       y = "",
       title = "Top 20 Highest Frequency Positive and Negative Words")
```

![](Billboard_Top_100_Report_files/figure-markdown_github-ascii_identifiers/positive%20and%20negative%20words-1.png)

Single word sentiment analysis does not always do well with sarcasm or nuance, but these words seem to be classified correctly for the most part. It's interesting to see the differences in frequency distributions -- the frequencies of positive words appear to fall off exponentially while the negative word frequencies are more evenly distributed. Maybe we just aren't as creative when expressing positive emotion as opposed to negative!

Comparison of tidytext Sentiment Analysis to Spotify Valence Audio Feature
--------------------------------------------------------------------------

As discussed earlier, the "valence" audio feature is a measure ranging from 0 to 1 that is intended to describe the positivity conveyed by a song where high valence corresponds to happier sounding songs and low valence corresponds to sadder sounding songs. Valence does not take into account the lyrics of an audio track and instead focuses on the musical sound. I am curious to see how valence and lyrical sentiment compare.

``` r
# make df of proportion of positive lyrical sentiment by year
yearly_positive_sentiment <- lyrics_sentiment_ratios %>% 
  filter(sentiment == "positive") %>% 
  rename(prop_positive_lyrical_sentiment = prop)

# make df of average track valence by year
yearly_valence <- spotify_track_data %>% 
  select(year, valence) %>% 
  group_by(year) %>% 
  summarize(avg_valence = mean(valence))

# join the two dfs above
sentiment_and_valence <- 
  yearly_positive_sentiment %>% 
  left_join(yearly_valence)

corr_coef <- cor(sentiment_and_valence$prop_positive_lyrical_sentiment, 
                 sentiment_and_valence$avg_valence,
                 use = "pairwise.complete.obs")

# plot the data
ggplot(sentiment_and_valence, aes(x = avg_valence, y = prop_positive_lyrical_sentiment)) +
  geom_point() +
  geom_smooth(data = subset(sentiment_and_valence, !year %in% c(1984)), method = "lm", se = FALSE) +
  #geom_abline(intercept = 0, slope = 1, alpha = 0.5, color = "red") +
  geom_text_repel(aes(label = year), size = 3, vjust = 1.5) +
  geom_text(x = 0.55, y = 0.7, label = paste("r = ", round(corr_coef, 3)), size = 6) +
  theme_minimal() +
  labs(x = "Average Valence (Spotify Audio Feature)",
       y = "Proportion of Positive Lyrical Sentiment",
       title = "Billboard Top 100: Valence vs. Proportion of Positive Lyrical Sentiment",
       subtitle = "Blue line provides linear trend after removing 1984. Correlation coefficient includes all values.")
```

![](Billboard_Top_100_Report_files/figure-markdown_github-ascii_identifiers/valence%20vs%20sentiment-1.png)

With the exception of the year 1984, which appears to be an outliers, the Average Valence and the Proportion of Positive Sentiment appear to be pretty well correlated with a correlation coefficient of 0.585. Because both of these features are highly variable at the song level, this seems like a pretty good correlation, and it gives me confidence in the quality of the sentiment information obtained from the actual lyrics.

Term Frequency - Inverse Document Frequency (TF-IDF)
----------------------------------------------------

TF-IDF is a way of measuring the relative importance of a word in a collection of documents. We can think of our document collection as consisting of all of the song lyrics in a given year or decade, for example. The TF-IDF value increases proportional to the frequency of the word in the document, but it is down-weighted by the frequency of the word in the entire set of documents. Again, we have a small corpus since song lyrics are pretty short compared to other types of text that one might want to analyze (like books or newspaper articles), but I'm curious to see if there are any words that more associated with specific decades.

My analysis below borrows heavily from Julia Silge's blog post on tf-idf of Jane Austen novels. Her entire blog is great and definitely worth a read! <https://juliasilge.com/blog/term-frequency-tf-idf/>

The first step toward calculating tf-idf values for each word in a decade is to create a dataframe of with the following variables:

-   decade
-   word
-   n: count of each individual word by decade
-   total\_words: count of all words by decade

We can create this dataframe with the code below:

``` r
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
```

    ## # A tibble: 54,641 x 4
    ##    decade  word     n total_words
    ##    <fctr> <chr> <int>       <int>
    ##  1    60s   you  6396      167536
    ##  2    60s     i  5545      167536
    ##  3    60s   the  5166      167536
    ##  4    60s   and  3790      167536
    ##  5    60s    to  3677      167536
    ##  6    60s     a  3597      167536
    ##  7    60s    me  3337      167536
    ##  8    60s    my  2526      167536
    ##  9    60s  love  2155      167536
    ## 10    60s    in  1829      167536
    ## # ... with 54,631 more rows

Now, we can calculate the tf-idf values for each word in each decade using the built-in `bind_tf_idf` function from the `tidytext` package. I will choose the top 10 tf-idf words by decade and plot them below.

``` r
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
```

![](Billboard_Top_100_Report_files/figure-markdown_github-ascii_identifiers/tf-idf%20plot-1.png)

In the graphs above, it is important to note that we don't see too many "stop-words." These are common words that are often removed before processing any text. We didn't have to filter them out here because they were so common in every decade that their resulting tf-idf values were very low. The words with the highest tf-idf values are a little silly, but they do provide an interesting summary that is not immediately obvious. I'm not going to go into detail here, but quiz yourself and see if you can determine the artist and track associated with many of the more obscure words. Other words are common across many artists and songs in a decade.

Closing Thoughts
----------------

This analysis focused on interesting data from the `billboard` package and contained a quick overview of some of the useful functions in the `tidytext` package.

The `billboard` package is full of interesting information, and I encourage you to check it out for yourself. The Spotify track data by itself could keep me entertained for days!

While it's not perfect, sentiment analysis of song lyrics does provide some interesting longitudinal insight that is correlated with Spotify's valence audio feature. We can also extract a lot of interesting information from tf-idf analysis. It's pretty cool that we can discern so much from qualitative and unstructured data!
