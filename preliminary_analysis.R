library(tidyverse)
library(billboard)
library(stringr)


# data
#glimpse(lyrics)
#glimpse(spotify_playlists)
#glimpse(spotify_track_data)
#glimpse(wiki_hot_100s)

spotify_track_data <- billboard::spotify_track_data

# top artists
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

