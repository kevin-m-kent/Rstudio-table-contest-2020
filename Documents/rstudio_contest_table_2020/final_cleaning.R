## some final cleaning before the shiny app 

#get all data ready for shiny app  
pitchers <- read_csv(here::here("Cleaned_Data", "pitchers.csv")) %>%
  mutate(team = str_to_title(team),
         name = str_to_title(name)) 

names(pitchers) <- toupper(names(pitchers))

position_players <- read_csv(here::here("Cleaned_Data", "position_players.csv")) %>%
  mutate(team = str_to_title(team),
         name = str_to_title(name))

names(position_players) <- toupper(names(position_players))

descriptions <- read_csv(here::here("Cleaned_Data", "description_clean.csv"))

player_name_key <- read_csv(here::here("Cleaned_Data", "player_name_card_key.csv")) %>%
  left_join(descriptions)

names(player_name_key) <- toupper(names(player_name_key))

pitchers_unique <- pitchers %>% 
  select(NAME) %>%
  distinct() %>%
  mutate(position = "pitcher") 

position_players_unique <- position_players %>%
  select(NAME) %>%
  distinct() %>%
  mutate(position = "position_player")

all_players_position <- pitchers_unique %>%
  bind_rows(position_players_unique)

player_name_key <- player_name_key %>%
  mutate(NAME = str_to_title(NAME)) %>%
  left_join(all_players_position) 

write_csv(player_name_key, here::here("Cleaned_Data", "player_name_key.csv"))
