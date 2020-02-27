library(tidyverse)
library(lubridate)
library(ggthemes)

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

popular_songs <- filter(spotify_songs, track_popularity > 80)

ggplot(popular_songs, aes(x = track_album_release_date, y = valence)) +
  geom_jitter()

popular_songs <- popular_songs %>%
  mutate(year = lubridate::year(track_album_release_date),
         month = lubridate::month(track_album_release_date))

ggplot(popular_songs, aes(x = year, y = danceability)) +
  geom_jitter() +
  scale_x_continuous(limits = c(2010, 2020))


ggplot(popular_songs, aes(x = year, y = valence)) +
  geom_jitter() +
  scale_x_continuous(limits = c(2010, 2020))


ggplot(popular_songs, aes(x = playlist_genre, y = year, colour = playlist_genre)) +
  geom_violin() +
  ggtitle ("Genre Popularity Over Time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Genre", y = "Year", colour = element_blank()) +
  scale_y_continuous(limits = c(2014, 2019)) +
  coord_flip() +
  scale_fill_discrete(
    breaks = c("edm", "latin", "pop", "r&b", "rap", "rock"),
    labels = c("EDM", "Latin", "Pop", "R&B", "Rap", "Rock"))

