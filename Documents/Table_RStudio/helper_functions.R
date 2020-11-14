## Helper functions for image processing

# Image cropping function for 2x4 scanning grid

crop <- function(file, crop_locations) {
  
  map2(file, crop_locations, ~ image_crop(image_read(here::here("Scans", "Grids", "UpperDeck_1998", .x)), .y))
}

# 

get_table <- function(image) {
  
  image %>%
    image_scale("x2000") %>%
    image_rotate(90) %>%
    image_deskew() %>%
    image_contrast() %>%
    image_write(format = 'png', density = '300x300') %>%
    tesseract::ocr(engine = eng_table) %>%
    str_split("\n") %>%
    as.data.frame(.) %>%
    rename(info = 1)
  
}

get_description <- function(image) {
  
  image %>%
    image_scale("x2000") %>%
    image_rotate(90) %>%
    image_deskew() %>%
    image_contrast() %>%
    image_write(format = 'png', density = '300x300') %>%
    tesseract::ocr(engine = eng_description) %>%
    str_split("\n") %>%
    as.data.frame(.) %>%
    rename(info = 1)
  
}

get_metdata <- function(image) {
  
  image %>%
    image_rotate(90) %>%
    image_crop("1050x150+0+0") %>%
    #image_scale("x2000") %>%
    image_deskew() %>%
    image_contrast() %>%
    image_negate()  %>%
    image_contrast() %>%
    image_modulate(brightness = 120) %>%
    #image_threshold("white", "20%") 
    image_write(format = 'png', density = '300x300') %>%
    tesseract::ocr(engine = eng_description) %>%
    str_split("\n") %>%
    as.data.frame(.) %>%
    rename(info = 1) 
  
}

get_name <- function(image) {
  
  image %>%
    image_rotate(90) %>%
    image_crop("1050x105+0+0") %>%
    image_negate() %>%
    image_threshold(type = "white", "10%") %>%
    image_reducenoise() %>%
    image_write(format = 'png', density = '300x300') %>%
    tesseract::ocr(engine = eng_name) 
    
    
}

get_position <- function(image) {
  
  image_rotate(90) %>%
    image_crop("1050x105+0+0") %>%
    image_threshold(type = "white", "30%") %>%
    image_write(format = 'png', density = '300x300') %>%
    tesseract::ocr() 
  
}

get_player_char <- function(image) {
  
  image_rotate(90) %>%
    image_crop("1050x105+0+0") %>%
    image_threshold(type = "white", "30%") %>%
    image_write(format = 'png', density = '300x300') %>%
    tesseract::ocr() 
  
}

get_position <- function(image) {
  
  image %>%
    image_rotate(90) %>%
    image_crop("1050x140+0+0") %>%
    image_threshold(type = "white", "30%") %>%
    image_reducenoise() %>%
    tesseract::ocr() 
  
}
