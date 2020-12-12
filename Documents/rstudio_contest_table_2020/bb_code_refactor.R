library(tidyverse)

pbp <- readRDS(url("https://github.com/guga31bb/sport_radar/blob/master/data/participation/c2b24b1a-98c5-465c-8f83-b82e746b4fcf.rds?raw=true"))

what_to_pull <- c('home.players', 'away.players')

get_names <- function(big_dat, dat_attr) {
  
  pluck(big_dat, "plays", dat_attr) %>%
    map(~ pluck(., "name")) %>%
    map(as.data.frame) %>%
    map_dfr(~ mutate(., num = row_number()), .id = "play") %>%
    rename(name = 2) %>%
    pivot_wider(names_from = "num", values_from = "name")
  
}

names <- map(what_to_pull, ~ get_names(pbp, .)) %>% 
  setNames(., what_to_pull) %>%
  bind_rows(.id = "type")



