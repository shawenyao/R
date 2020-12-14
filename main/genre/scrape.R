library(tidyverse)
library(rvest)
library(janitor)

setwd("C:/Users/Wenyao/Desktop/R/R")

scrape_ps4 <- function(url, css){
  url %>% 
    read_html() %>% 
    html_node(css) %>%
    html_table(fill = TRUE) %>% 
    clean_names() %>% 
    slice(-1)
}

df <- list(
  url = c(
    "https://en.wikipedia.org/wiki/List_of_PlayStation_4_games",
    "https://en.wikipedia.org/wiki/List_of_PlayStation_4_games_(M%E2%80%93Z)",
    "https://en.wikipedia.org/wiki/List_of_PlayStation_4_free-to-play_games"),
  css = c(
    "#softwarelist",
    "#softwarelist",
    "#f2plist"
  )
) %>% 
  pmap(scrape_ps4) %>% 
  bind_rows()

df %>% 
  group_by(genre_s) %>% 
  count() %>% 
  arrange(-n) %>% 
  View()
