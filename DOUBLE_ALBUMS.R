# Double Albums from my early CD collection
# Smashing Pumpkins, Nine Inch Nails and Outkast

# libraries
library(tidyverse)
library(spotifyr)
library(showtext)
library(scales)
library(ggtext)

# add font ----------------------------------------------------------------
font_add_google(name = "IBM Plex Mono", family = "IBM Plex Mono")

# turn on showtext --------------------------------------------------------
showtext_auto()

# API AUTHENTICATION ------------------------------------------------------
Sys.setenv(SPOTIFY_CLIENT_ID = 'Insert ID')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'Insert SECRET')

access_token_spotify <- get_spotify_access_token()

# GET SPOTIFY DATA --------------------------------------------------------

# Smashing Pumpkins --------------------------------------------------------
# Artist ID "40Yq4vzPs9VNUrIBG5Jr2i" | Album ID "55RhFRyQFihIyGf61MgcfV"
# Song IDs "2rBp8ix2ieRutorlsmPf9K","7zwwvrJAWGjfc9wFD3bVzZ","5LtIsKctHGXbE1M3U6g7qO","7bu0znpSbTks0O6I98ij0W","6912SIXb1G9ql3quOxeCaa","4YFcGTdgmEuw8xTO4XrxbB","5INe9EN6cNF6axQ4A4BQfy","6GtX0jaNL8IjVQfrDBx81z","428dyogxbsqj9U5leQ9why","6W9nK2wPyJklgDVf0AQcPO","2rBp8ix2ieRutorlsmPf9K,7zwwvrJAWGjfc9wFD3bVzZ","2yKrGPtIwsiwv5tGTDgpNd","6UuGRkwfLwBAfmHktU9GNA","7mNyAFramOVsGZxdRyz4KQ","72ZZqYFcpdIxC84EYsWjpP","7luW8OHN1fDFj6l0lDTViG","4A0f5CrcOYRVwihIQkcHdP","1fPKWu8NKS1ZGHjFZ5QCAs","0DqjIXjwhDOKVl7ChheeIc","5QLHGv0DfpeXLNFo7SFEy1","0i5zjBavIn1LsTsh9oh54d","5W8AYj6e6syodRPzPrJcrb","4bbdwn07T9xWdZv8siAptT","3nnvMbieuZH3L3caIRdpjk","4vkcU3UFjYsKuBibmYA3hL","3HJLGDGbW7TIVq3hGJz0W9","6gKUop4OJBuLrkSqRckUNU","56z8HT2VyKL2nTg0Q3URPb","1VZGj6BcHwbfLxxYlhk8kI"

# GET AUDIO FEATURES ------------------------------------------------------
spotify_smashing_pumpkins <- spotifyr::get_artist_audio_features("Smashing Pumpkins")

# FILTER SONGS AND REMOVE DUPLICATES --------------------------------------
spotify_smashing_pumpkins_filtered <- spotify_smashing_pumpkins %>%
  filter(track_id %in% c("2rBp8ix2ieRutorlsmPf9K","7zwwvrJAWGjfc9wFD3bVzZ",
                          "5LtIsKctHGXbE1M3U6g7qO","7bu0znpSbTks0O6I98ij0W",
                          "6912SIXb1G9ql3quOxeCaa","4YFcGTdgmEuw8xTO4XrxbB",
                          "5INe9EN6cNF6axQ4A4BQfy","6GtX0jaNL8IjVQfrDBx81z",
                          "428dyogxbsqj9U5leQ9why","6W9nK2wPyJklgDVf0AQcPO",
                          "2rBp8ix2ieRutorlsmPf9K,7zwwvrJAWGjfc9wFD3bVzZ",
                          "2yKrGPtIwsiwv5tGTDgpNd","6UuGRkwfLwBAfmHktU9GNA",
                          "7mNyAFramOVsGZxdRyz4KQ","72ZZqYFcpdIxC84EYsWjpP",
                          "7luW8OHN1fDFj6l0lDTViG","4A0f5CrcOYRVwihIQkcHdP",
                          "1fPKWu8NKS1ZGHjFZ5QCAs","0DqjIXjwhDOKVl7ChheeIc",
                          "5QLHGv0DfpeXLNFo7SFEy1","0i5zjBavIn1LsTsh9oh54d",
                          "5W8AYj6e6syodRPzPrJcrb","4bbdwn07T9xWdZv8siAptT",
                          "3nnvMbieuZH3L3caIRdpjk","4vkcU3UFjYsKuBibmYA3hL",
                          "3HJLGDGbW7TIVq3hGJz0W9","6gKUop4OJBuLrkSqRckUNU",
                          "56z8HT2VyKL2nTg0Q3URPb","1VZGj6BcHwbfLxxYlhk8kI"))

# Nine Inch Nails ---------------------------------------------------------
# Artist ID "0X380XXQSNBYuleKzav5UO" | Album ID "4uiVwLbTzE6VMkXpaDAwB8"

# get spotify audio features for artist
spotify_nine_inch_nails <- spotifyr::get_artist_audio_features('Nine Inch Nails')

# filter songs and remove doubles
spotify_nine_inch_nails_filtered <- spotify_nine_inch_nails %>%
  filter(album_id == "4uiVwLbTzE6VMkXpaDAwB8")

# Outkast -----------------------------------------------------------------
# Artist ID "1G9G7WwrXka3Z1r7aIDjI7" | Album ID "1UsmQ3bpJTyK6ygoOOjG1r"

# get spotify audio features for artist
spotify_outkast <- spotifyr::get_artist_audio_features('Outkast')

# filter songs and remove doubles
spotify_outkast_filtered <- spotify_outkast %>%
  filter (album_id == "1UsmQ3bpJTyK6ygoOOjG1r")

# original plan was to use {ggradar} to use radar plots and create one chart per album and combine using {patchwork}
# ultimately bagged that idea after some trial and error...probably could have (should have) gone back to simplify the data wrangling

# create data frames for Smashing Pumpkins
# per song
df_SP_point <- spotify_smashing_pumpkins_filtered %>%
  select(artist_name,
         album_name,
         disc_number,
         track_id,
         danceability,
         valence,
         energy,
         acousticness,
         speechiness,
         instrumentalness,
         liveness,
         acousticness) %>%
  mutate(album_name = "Mellon Collie And The Infinite Sadness") %>%
  gather(key = feature, value = measure, 
                              danceability, 
                              valence, 
                              energy, 
                              acousticness, 
                              speechiness, 
                              instrumentalness, 
                              liveness, 
                              acousticness) %>%
  group_by(disc_number)

# by disc number
df_SP_bar <- spotify_smashing_pumpkins_filtered %>%
  select(artist_name,
         album_name,
         disc_number,
         danceability,
         valence,
         energy,
         acousticness,
         speechiness,
         instrumentalness,
         liveness,
         acousticness) %>%
  group_by(disc_number) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  add_column(artist_name = "The Smashing Pumpkins") %>%
  add_column(album_name = "Mellon Collie And The Infinite Sadness") %>%
  gather(key = feature, value = measure, 
                               danceability, 
                               valence, 
                               energy, 
                               acousticness, 
                               speechiness, 
                               instrumentalness, 
                               liveness, 
                               acousticness)

# create data frames for Nine Inch Nails
# per song
df_NIN_point <- spotify_nine_inch_nails_filtered %>%
  select(artist_name,
         album_name,
         disc_number,
         track_id,
         danceability,
         valence,
         energy,
         acousticness,
         speechiness,
         instrumentalness,
         liveness,
         acousticness) %>%
  gather(key = feature, value = measure, 
                               danceability, 
                               valence, 
                               energy, 
                               acousticness, 
                               speechiness, 
                               instrumentalness, 
                               liveness, 
                               acousticness) %>%
  group_by(disc_number)

# by disc number
df_NIN_bar <- spotify_nine_inch_nails_filtered %>%
  select(artist_name,
         album_name,
         disc_number,
         danceability,
         valence,
         energy,
         acousticness,
         speechiness,
         instrumentalness,
         liveness,
         acousticness) %>%
  group_by(disc_number) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  add_column(artist_name = "Nine Inch Nails") %>%
  add_column(album_name = "The Fragile") %>%
  gather(key = feature, value = measure, 
                               danceability, 
                               valence, 
                               energy, 
                               acousticness, 
                               speechiness, 
                               instrumentalness, 
                               liveness, 
                               acousticness)

# create data frames for Outkast
# per song
df_O_point <- spotify_outkast_filtered %>%
  select(artist_name,
         album_name,
         disc_number,
         track_id,
         danceability,
         valence,
         energy,
         acousticness,
         speechiness,
         instrumentalness,
         liveness,
         acousticness) %>%
  gather(key = feature, value = measure, 
                               danceability, 
                               valence, 
                               energy, 
                               acousticness, 
                               speechiness, 
                               instrumentalness, 
                               liveness, 
                               acousticness) %>%
  group_by(disc_number)

# by disc number
df_O_bar <- spotify_outkast_filtered %>%
  select(artist_name,
         album_name,
         disc_number,
         danceability,
         valence,
         energy,
         acousticness,
         speechiness,
         instrumentalness,
         liveness,
         acousticness) %>%
  group_by(disc_number) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  add_column(artist_name = "Outkast") %>%
  add_column(album_name = "Speakerboxxx/The Love Below") %>%
  gather(key = feature, value = measure, 
                               danceability, 
                               valence, 
                               energy, 
                               acousticness, 
                               speechiness, 
                               instrumentalness, 
                               liveness, 
                               acousticness)

# decided to skip {patchwork} and use a single plot combining the three albums using facet_wrap  
# combine to one data frame for each geom
df_ALL_point <- rbind(df_SP_point, df_NIN_point, df_O_point) %>%
  mutate(feature = str_to_sentence(feature)) %>%
  mutate(artist_name = str_to_upper(artist_name))
df_ALL_bar <- rbind(df_SP_bar, df_NIN_bar, df_O_bar) %>%
  mutate(feature = str_to_sentence(feature)) %>%
  mutate(artist_name = str_to_upper(artist_name))

# create plot
ggplot() +
  geom_col(data = df_ALL_bar, aes(x = measure, y = feature, fill = as.character(disc_number), alpha = .07), position = position_dodge2(reverse = TRUE)) +
  geom_point(data = df_ALL_point, aes(x = measure, y = feature, color = as.character(disc_number)), position = position_jitterdodge(dodge.width = -1, set.seed(seed = 74))) +
  scale_fill_manual(values = c("#D6A2E8","#9AECDB")) +
  scale_color_manual(values = c("#D6A2E8","#9AECDB")) +
  #scale_fill_manual(values = c("#ba78c8","#dce775")) +
  #scale_color_manual(values = c("#ba78c8","#dce775")) +
  facet_grid(~ artist_name + album_name) +
  theme_minimal() +
  theme(text = element_text(size = 8, family = "IBM Plex Mono", color = "#ffffff"),
        plot.title = element_text(family = "IBM Plex Mono", size = 34, hjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(family = "IBM Plex Mono", size = 9, hjust = 0.5, lineheight = 1.5, margin = margin(0, 0, 5, 0,"mm")),
        plot.caption = element_text(hjust = 0.5, size = 7),
        panel.grid = element_line(size = 0.25, color = "#ffffff"),
        axis.line.x = element_line(size = 1.4, color = "#ffffff"),
        panel.grid.major.y = element_line(size = 0.7),
        axis.text.x = element_text(family = "IBM Plex Mono", color = "#ffffff", size = 7.3),
        axis.title.y = element_blank(),
        axis.text.y = element_text(family = "IBM Plex Mono", color = "#ffffff", size = 7.3),
        strip.text = element_text(family = "IBM Plex Mono", color = "#ffffff", size = 7.3),
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
        plot.background = element_rect(color = "#2C3A47", fill = "#2C3A47"), #262626
        legend.position = "none",
        panel.spacing = unit(2, "lines")) +
  scale_x_continuous(breaks = c(0, 0.5, 1)) +
  scale_y_discrete(limits=rev) +
  labs(title = "DOUBLE ALBUMS",
        subtitle = "<span style = 'font-size:10pt;'>This visualization explores three albums released as double CD sets using Spotify's musical attributes<br>to compare the average mean score by disc and the individual track attributes for <span style = 'color:#D6A2E8;'>DISC ONE</span> and <span style = 'color:#9AECDB;'>DISC TWO</span>.",
        caption = "Data: Spotify via {spotifyr} | Design: Ryan Hart",
        x = "\nAttribute Values (0 = Least and 1 = Most)\n\n")
  
# save plot
ggsave(paste0("DOUBLE_ALBUMS_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 10, height = 8)
  

