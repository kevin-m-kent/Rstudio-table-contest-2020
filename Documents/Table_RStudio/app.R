#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(gt)
library(tesseract)
library(magick)
library(stringr)
library(sparklines)
library(shinythemes)


all_grids <- list.files(here::here("Scans", "Grids", "UpperDeck_1998"))

backs <- all_grids[str_detect(all_grids, "back")]

fronts <- all_grids[str_detect(all_grids, "front")]

size <- "753x1050"

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
    map(~ map2(., crop_locations, ~ image_crop(image_read(here::here("Scans", "Grids", "UpperDeck_1998", .x)), .y))) %>%
    unlist(.)

pitchers <- read_csv(here::here("Cleaned_Data", "pitchers.csv")) %>%
    mutate(team = str_to_title(team),
           name = str_to_title(name)) %>%
    select(-description) 

names(pitchers) <- toupper(names(pitchers))

position_players <- read_csv(here::here("Cleaned_Data", "position_players.csv")) %>%
    mutate(team = str_to_title(team),
    name = str_to_title(name)) %>%
    select(-description)

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

player_choices <- setNames(player_name_key$NAME, player_name_key$NAME)

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

player_name = 'Derek Jeter'

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


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),

    # Application title
    titlePanel("The Modern Baseball Card"),

    # Sidebar with a slider input for number of bins 
    
    
    mainPanel(
        selectInput("Player", "Choose a Player",
                    player_choices, selected = "Derek Jeter"),
        # Show a plot of the generated distribution
        fluidRow(column(7,imageOutput("img")), column(4,
                                                      fluidRow(gt_output(outputId = "table"),
                                                               htmlOutput("spark_table", width = 1)), textOutput("description")))))
            
test <- lubridate::as_datetime('2020-10-01 00:05:00')
                



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$img <- renderImage({
        id <- player_name_key %>%
            filter(NAME == input$Player) %>%
            slice(1:1) %>%
            pull(2)

        tmpfile <- front_images[[id]] %>%
            image_scale("600") %>%
            image_write(tempfile(fileext='jpg'), format = 'jpg')

        list(src = tmpfile, contentType = "image/jpeg")
    })
    
    output$table <-  render_gt(
        expr = get_dt(input$Player),
        width = 850)
    
    output$spark_table <- renderUI({
        
        get_ft(input$Player)[[1]]
    })
    
    output$description <- renderText({
        
        get_ft(input$Player)[[2]]
        
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
