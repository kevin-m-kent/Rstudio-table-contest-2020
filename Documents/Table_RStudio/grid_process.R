library(tesseract)
library(magick)
library(tidyverse)
library(glue)
library(Lahman)
library(fuzzyjoin)
source('helper_functions.R')

all_grids <- list.files(here::here("Scans", "Grids", "UpperDeck_1998"))

backs <- all_grids[str_detect(all_grids, "back")]

fronts <- all_grids[str_detect(all_grids, "front")]

x_inc <- 753
y_inc <- 1050
num_rows <- 2
num_cols <- 4
num_cards <- num_rows*num_cols
y_positions <- c(rep(0, num_cols), rep(y_inc, num_cols))
cols <- seq(0, num_cols -1, by = 1)

locations <- tibble(x_inc = rep(x_inc, num_cards), y_inc = rep(y_inc, num_cards), 
       cols = rep(cols, num_rows), y_pos = y_positions)

crop_locations <- locations %>%
  mutate(x_pos = x_inc*cols) %>%
  str_glue_data("{x_inc}x{y_inc}+{x_pos}+{y_pos}")

front_images <- fronts %>%
  map(~ crop(., crop_locations)) %>%
  unlist(.)

back_images <- backs %>%
  map(~ crop(., crop_locations)) %>%
  unlist(.)

eng_table <- tesseract(options = list(load_system_dawg =0, 
                                      load_freq_dawg = 0,
                                      tessedit_char_whitelist = " .0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

eng_name <- tesseract(options = list(tessedit_char_whitelist = " ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

eng_description <- tesseract()

names_list <- map(back_images, get_name) %>%
  str_extract(., "[A-Z]{3,} [A-Z]{3,}") 
  
table_data <- map(back_images, get_table)

num_missing <- sum(is.na(names_list))

names_list[is.na(names_list)] <- seq(1, num_missing, by = 1)

players <- as.data.frame(People) %>%
  mutate(debut = lubridate::ymd(debut)) %>%
  filter(between(lubridate::year(debut), 1960, 1998)) %>%
  mutate(name = tolower(paste(nameFirst, nameLast))) %>%
  select(name) %>%
  distinct()

names_list_df <- as.data.frame(names_list) %>%
  rename(name = 1)

names_df <- names_list_df %>%
  stringdist_left_join(players, distance_col = "distance", ignore_case = TRUE, max_dist = 3)  %>%
  group_by(name.x) %>%
  arrange(distance) %>%
  slice(1:1) %>%
  rename(name = name.x)

names_list_df <- names_list_df %>%
  left_join(names_df) %>%
  mutate(name = case_when(
    !is.na(name.y.y) ~ name.y.y,
    TRUE ~ name
  )) %>%
  select(name)
  
names(table_data) <-seq(1: length(table_data))

us_cities <- maps::us.cities %>%
  mutate(name = str_remove(name, paste0(" ", country.etc))) %>%
  select(name)

ca_cities <- maps::canada.cities %>%
  mutate(name = str_remove(name, paste0(" ", country.etc))) %>%
  select(name)

all_cities <- bind_rows(us_cities, ca_cities)

team_match <- Teams %>%
  filter(between(yearID, 1980, 1998)) %>%
  select(name, teamIDBR) %>%
  distinct() %>%
  mutate(name = str_replace(name, "St\\.", "Saint")) %>%
  mutate(name = str_replace(name, "Tampa Bay", "Tampa")) %>%
  mutate(state = str_extract(name, paste0(state.name, collapse = "|"))) %>%
  mutate(city = str_extract(name, paste0(all_cities$name, collapse = "|"))) %>%
  mutate(location = case_when(
    is.na(city) ~ state,
    TRUE ~ city
  )) %>%
  mutate(name = str_remove_all(name, location)) %>%
  mutate(name = str_trim(name)) %>%
  mutate(name = str_remove_all(name, "^[A-Z]{1} ")) %>%
  select(name) %>%
  transmute(team = tolower(name)) 
  
table_clean <- table_data %>%
  map(~ filter(., info!= "")) %>%
  map_df(~ mutate(., team = str_extract(info, "[A-Z]{4,}")), .id = "player") %>%
  mutate(team = tolower(team)) %>%
  filter(!is.na(team)) %>%
  stringdist_inner_join(., team_match,
                        by ="team", distance_col = NULL) %>%
  mutate(info = str_replace_all(info, toupper(team.x), toupper(team.y))) %>%
  select(-team.x) %>%
  rename(team = team.y) %>%
  mutate(info = str_remove(info, toupper(team))) %>%
  mutate(info = str_replace_all(info, "Q", "9")) %>%
  mutate(info = str_replace_all(info, "B", "8")) %>%
  distinct() %>%
  filter(!str_detect(info, "YR")) %>%
  filter(str_detect(info, "^[7-9]")) %>%
  mutate(info = str_replace_all(info, "O", "0")) %>%
  mutate(info = str_replace_all(info, "S", "5")) %>%
  mutate(info = str_replace_all(info, "G", "6")) %>%
  filter(nchar(info) > 10) %>%
  mutate(info = str_remove_all(info, "[A-Z]|[a-z]")) %>%
  mutate(info = str_trim(info)) %>%
  mutate(position = case_when(
    str_detect(info, "[0-9]\\.[0-9]") ~ "pitcher",
    str_detect(info, " \\.[0-9]") ~ "position_player", 
    TRUE ~ "not_sure")) %>%
  mutate(team = str_remove(team, "^h ")) %>%
  mutate(year = str_extract(info, "^[0-9]{1,2}")) %>%
  mutate(info = str_remove(info, "^[0-9]{1,2}")) %>%
  mutate(info = str_trim(info))



position <- map(back_images, get_position)

names(position) <- seq(1: length(position))

names_list_df <- names_list_df %>%
  mutate(id = row_number())

positions <- c("des. hitter", "catcher",  "infield", "first base", "second base", "shortstop", "third base", "outfield", "pitcher", "dh", "-?1b-?", "-?2b-?", '-?3b-?', "-?ss-?", "-?of-?")

positions_df <- tibble(position = positions)

position_clean <- position %>%
  map(~ str_extract_all(., paste0(positions, collapse = "|"))) %>%
  map(~ unlist(paste0(., collapse = ","))) %>%
  bind_rows(.id = "id") %>%
  pivot_longer(names_to = "id", values_to = "position", everything()) %>%
  mutate(id =  as.numeric(id))

table_meta <- table_clean %>%
  mutate(player = as.numeric(player)) %>%
  select(-position) %>%
  left_join(names_list_df, by = c("player" = "id")) %>%
  left_join(position_clean, by = c("player" = "id")) %>%
  rename(id = player) %>%
  mutate(position = str_remove(position, "-")) 

metadata_list <- map(back_images, get_metdata)

metadata_clean <- metadata_list %>%
  bind_rows(.id = "id") %>%
  rename(info = 2) %>%
  filter(str_detect(info, "Ht:")) %>%
  mutate(info = str_remove(info, "^.*?(?=Ht)")) %>%
  mutate(info = str_remove_all(info,  paste0(positions, collapse = "|"))) %>%
  rename(player_info = info) %>%
  mutate(id = as.numeric(id))

table_meta <- table_meta %>%
  left_join(metadata_clean)

metadata_list[[200]]

description_clean <- description_list %>%
  map_df(~ tibble(info = .), .id = "id") 

description_clean <- description_clean %>%
  group_by(id) %>%
  slice((str_which(info$info, "TOTALS")[1]+1)) %>%
  ungroup() %>%
  mutate(id = as.numeric(id)) %>% 
  rename(description = info)
  
description_clean$description <- description_clean$description$info

table_all <- table_meta 

split_position <- table_all %>%
  mutate(is_pitcher = case_when(
    position == "pitcher" ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  mutate(player_info = str_replace_all(player_info, "\\.", " ")) %>%
  mutate(player_info = str_replace_all(player_info, ";", ":")) %>%
  separate(player_info, into = c("HT", "WT", "Bats", "Throws", "Born"), sep = " [A-Z]([a-z])+:") %>%
  mutate(Born = str_trim(Born)) %>%
  separate(Born, into = c("DOB", "Birth_location"), sep = "(?<=[0-9]) ") %>%
  mutate(Birth_location = str_remove_all(Birth_location, "(?<=[A-Z] ).*")) %>%
  split(.$is_pitcher)

batting_cats <- c("AVG", "G", "AB", "R", "H", "2B", "3B", "HR", "RBI", "SB")

pitching_cats <- c("W", "L", "ERA", "G", "GS", "SV", "IP", "H", "BB", "K")

position_players<- split_position[[1]] %>%
  separate(info, into = batting_cats, sep = " +") %>%
  select(-is_pitcher)

pitchers <- split_position[[2]] %>%
  separate(info, into = pitching_cats, sep = " +")  %>%
  select(-is_pitcher) 
  
write_csv(pitchers, here::here("Cleaned_Data", "pitchers.csv"))
write_csv(position_players, here::here("Cleaned_Data", "position_players.csv"))
write_csv(names_list_df, here::here("Cleaned_Data", "player_name_card_key.csv"))
write_csv(description_clean, here::here("Cleaned_Data", "description_clean.csv"))
  