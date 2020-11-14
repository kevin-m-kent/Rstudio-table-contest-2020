library(shiny)
library(tidyverse)
library(gt)
library(tesseract)
library(magick)
library(stringr)
library(sparklines)
library(shinythemes)
source('helper_functions.R')
source('shiny_helper_functions.R')

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
    map(~ crop(., crop_locations)) %>%
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

player_name = 'Derek Jeter'

# UI definition
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
            
                
# server logic 
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
