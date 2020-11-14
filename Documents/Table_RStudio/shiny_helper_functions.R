# helper functions for shiny app 

get_dt <- function(player_name){
  
  player <- player_name_key %>%
    filter(NAME == player_name) 
  
  pos <- player$position[1]
  
  description <- player$DESCRIPTION[1]
  
  
  if (pos == 'pitcher') {
    
    meta_info <- pitchers %>%
      filter(NAME == player_name) %>%
      select(HT, WT, BATS, THROWS, DOB, BIRTH_LOCATION) %>%
      distinct() %>%
      slice(1:1) %>%
      mutate(all_info = str_glue("{HT} WT: {WT} BATS: {BATS} THROWS: {THROWS} DOB: {DOB} Birth Location: {BIRTH_LOCATION}"))
    
    characteristics <- meta_info$all_info[1]
    
    pitchers_clean <-  pitchers %>%
      filter(NAME == player_name) %>%
      distinct() %>%
      select(-HT, -WT, -BATS, -THROWS, -DOB, -BIRTH_LOCATION) %>%
      arrange(YEAR) %>%
      select(-NAME, -POSITION, -ID) %>%
      relocate(any_of(c("YEAR", "TEAM"))) 
    
    pitchers_clean %>%
      gt() %>%
      cols_width(everything() ~ px(72)) %>%
      tab_header(title = player_name, subtitle = characteristics) %>%
      tab_options( column_labels.background.color = "gray") %>%
      opt_table_font(
        font = list(
          google_font(name = "Goldman"),
          "Cochin", "Serif"
        )
      ) }
  
  else {
    
    
    
    meta_info <- position_players %>%
      filter(NAME == player_name) %>%
      select(HT, WT, BATS, THROWS, DOB, BIRTH_LOCATION) %>%
      distinct() %>%
      slice(1:1) %>%
      mutate(all_info = str_glue("{HT} WT: {WT} BATS: {BATS} THROWS: {THROWS} DOB: {DOB} Birth Location: {BIRTH_LOCATION}"))
    
    characteristics <- meta_info$all_info[1]
    
    position_players_clean <-  position_players %>%
      filter(NAME == player_name) %>%
      distinct() %>%
      select(-HT, -WT, -BATS, -THROWS, -DOB, -BIRTH_LOCATION) %>%
      arrange(YEAR) %>%
      select(-NAME, -POSITION, -ID) %>%
      relocate(any_of(c("YEAR", "TEAM"))) 
    
    position_players_clean %>%
      gt() %>%
      cols_width(everything() ~ px(72)) %>%
      tab_header(title = player_name, subtitle = characteristics) %>%
      tab_options( column_labels.background.color = "gray") %>%
      opt_table_font(
        font = list(
          google_font(name = "Goldman"),
          "Cochin", "Serif"
        )
      ) 
    
  }
  
}


get_ft <- function(player_name){
  
  player <- player_name_key %>%
    filter(NAME == player_name) 
  
  pos <- player$position[1]
  
  description <- player$DESCRIPTION[1]
  
  if (pos == 'pitcher') {
    
    meta_info <- pitchers %>%
      filter(NAME == player_name) %>%
      select(HT, WT, BATS, THROWS, DOB, BIRTH_LOCATION) %>%
      distinct() %>%
      slice(1:1) %>%
      mutate(all_info = str_glue("{HT} WT: {WT} BATS: {BATS} THROWS: {THROWS} DOB: {DOB} Birth Location: {BIRTH_LOCATION}"))
    
    characteristics <- meta_info$all_info[1]
    
    pitchers_clean <-  pitchers %>%
      filter(NAME == player_name) %>%
      distinct() %>%
      select(-HT, -WT, -BATS, -THROWS, -DOB, -BIRTH_LOCATION) %>%
      arrange(YEAR) %>%
      select(-NAME, -POSITION, -ID) %>%
      relocate(any_of(c("YEAR", "TEAM"))) 
    
    summary_spk <- pitchers_clean %>%
      select(-TEAM, -YEAR) %>%
      mutate_all(as.numeric) %>%
      summarise_all(spk_chr, type = "line") %>%
      mutate(YEAR = "TRENDS", TEAM = "") %>%
      relocate(any_of(c("YEAR", "TEAM")))
    
    # summary_names <- tibble(names = rep('&nbsp', 12), mult = seq(1, 12, by = 1)) %>%
    #     mutate(name_full = map2(names, mult, ~ paste0(rep(.x, .y), collapse = "")))
    # 
    # names(summary_spk) <- summary_names$name_full
    
    tab <- summary_spk %>%
      formattable(width = "50%") %>%
      formattable::as.htmlwidget() %>%
      div() %>%
      spk_add_deps() %>%   
      {column(width=4, .)} %>%
      fluidRow()
    
    list(tab, description)
  }
  else {
    
    meta_info <- position_players %>%
      filter(NAME == player_name) %>%
      select(HT, WT, BATS, THROWS, DOB, BIRTH_LOCATION) %>%
      distinct() %>%
      slice(1:1) %>%
      mutate(all_info = str_glue("{HT} WT: {WT} BATS: {BATS} THROWS: {THROWS} DOB: {DOB} Birth Location: {BIRTH_LOCATION}"))
    
    characteristics <- meta_info$all_info[1]
    
    position_players_clean <-  position_players %>%
      filter(NAME == player_name) %>%
      distinct() %>%
      select(-HT, -WT, -BATS, -THROWS, -DOB, -BIRTH_LOCATION) %>%
      arrange(YEAR) %>%
      select(-NAME, -POSITION, -ID) %>%
      relocate(any_of(c("YEAR", "TEAM"))) 
    
    summary_spk <- position_players_clean %>%
      select(-TEAM, -YEAR) %>%
      mutate_all(as.numeric) %>%
      summarise_all(spk_chr, type = "line") %>%
      mutate(YEAR = "TRENDS", TEAM = "") %>%
      relocate(any_of(c("YEAR", "TEAM")))
    
    # summary_names <- tibble(names = rep('&nbsp', 12), mult = seq(1, 12, by = 1)) %>%
    #     mutate(name_full = map2(names, mult, ~ paste0(rep(.x, .y), collapse = "")))
    # 
    # names(summary_spk) <- summary_names$name_full
    
    tab <- summary_spk  %>%
      formattable(width = "50%") %>%
      formattable::as.htmlwidget() %>%
      div() %>%
      spk_add_deps()  %>%
      {column(width=4, .)} %>%
      fluidRow()
    
    list(tab, description)
    
  }
  
}