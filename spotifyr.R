library(spotifyr)
library(tidyverse)

install.packages("knitr")
library(knitr)

Sys.setenv(SPOTIFY_CLIENT_SECRET = "4f590616548d4ccfa783b4c62f990234")
Sys.setenv(SPOTIFY_CLIENT_ID = "1a37c1f7998241ada9a5a950e674d309")
access_token <- get_spotify_access_token()

avicii <- get_artist_audio_features("avicii")

avicii %>% 
  count(key_mode, sort = TRUE) %>% 
  head(5) %>% 
  kable()

library(lubridate)###sort out the time format issue


##### Web scrapping

url <- "https://www.mobile01.com/forumtopic.php?c=30"
title <- read_html(url)
title <- html_nodes(title, ".u-ellipsis") #
title_text <- html_text(title) %>%  
View()





get_my_recently_played(limit = 5) %>% 
  mutate(artist.name = map_chr(track.artists, fuction(x) x$name[1]),
         played_at = as_datetime(played_at)) %>% 
  select(track.name, artist.name, track.album.name, played_at) %>% 
  kable()

