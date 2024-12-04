library(tidyverse)
library(bladdr)
library(ggsci)

songs <- read_csv("songs.csv")

songs <- songs |>
  mutate(
    song_by_artist = paste0(song, " by ", artist),
  ) |>
  mutate(
    n_years_in = factor(n(), levels = 1:6), .by = song_by_artist,
  )

summary_songs <- songs |>
  summarize(n_years_in = n(), .by = song_by_artist)

ggplot(summary_songs, aes(x = n_years_in)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), nudge_y = 10) +
  labs(x = "# years placed", y = "Frequency") +
  theme_tufte(15) +
  scale_x_continuous(breaks = 1:6) +
  coord_cartesian(ylim = c(0, 570), expand = FALSE)

more_than_one_year <- songs |>
  filter(n_years_in != 1) |>
  mutate(n_years_in = fct_drop(n_years_in))

ggplot(more_than_one_year, aes(year, rank, color = song_by_artist)) +
  geom_line(aes(group = song_by_artist)) +
  facet_grid(~n_years_in) +
  theme_tufte(15) +
  scale_color_viridis_d(option = "inferno", end = 0.8) +
  scale_y_reverse() +
  scale_x_continuous(breaks = 2016:2023) +
  theme(legend.position = "none") +
  labs(x = "Year", y = "Rank")

road_dogs <- more_than_one_year |>
  mutate(n_years_in = as.numeric(as.character(n_years_in))) |>
  filter(n_years_in > 4)

ggplot(road_dogs, aes(x = year, y = rank, color = song_by_artist)) +
  geom_line() +
  theme_tufte(15) +
  scale_y_reverse() +
  scale_x_continuous(breaks = 2016:2023) +
  labs(x = "Year", y = "Rank", color = NULL) +
  theme(legend.position = "right") +
  scale_color_npg()

# Show common artists
# All gray except one artist color
freq_artists <- songs |>
  distinct(song_by_artist, .keep_all = TRUE) |>
  mutate(artist_freq = n(), .by = artist) |>
  distinct(artist, .keep_all = TRUE)

ggplot(freq_artists, aes(artist_freq)) +
  geom_histogram(bins = 14) +
  theme_tufte(15) +
  labs(x = "Number of songs/artist", y = "Frequency") +
  coord_cartesian(expand = FALSE)

freq_artist_names <- filter(freq_artists, artist_freq > 5) |>
  arrange(desc(artist_freq))

# Show my glass animals period

highlight_freqs <- songs |>
  filter(artist %in% c(freq_artist_names$artist)) |>
  mutate(artist = factor(artist, levels = freq_artist_names$artist))

ggplot(highlight_freqs, aes(year, rank, color = artist)) +
  geom_point() +
  theme_tufte(15) +
  scale_color_npg(na.value = "gray90") +
  scale_y_reverse() +
  scale_x_continuous(breaks = 2016:2023, labels = paste0("'", 16:23)) +
  theme(legend.position = "right") +
  facet_wrap(~artist) +
  labs(x = "Year", y = "Rank")
