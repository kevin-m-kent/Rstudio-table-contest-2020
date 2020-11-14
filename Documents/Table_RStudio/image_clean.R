library(tesseract)

library(magick)
library(tidyverse)

input <- image_read(here::here("Scans", "UpperDeck_1998", "Nomar_Garciaparra", "back.jpg"))

eng_table <- tesseract(options = list(load_system_dawg =0, 
                                load_freq_dawg = 0,
                                tessedit_char_whitelist = " .0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

eng_description <- tesseract()

get_table <- function(image) {
  
  image %>%
    image_deskew() %>%
    image_contrast() %>%
    image_scale("x2000") %>%
    image_write(format = 'png', density = '300x300') %>%
    tesseract::ocr(engine = eng_table) %>%
    str_split("\n") %>%
    as.data.frame(.) %>%
    rename(info = 1)
  
}

get_description <- function(image) {
  
  image %>%
    image_deskew() %>%
    image_contrast() %>%
    image_scale("x2000") %>%
    image_write(format = 'png', density = '300x300') %>%
    tesseract::ocr(engine = eng_description) %>%
    str_split("\n") %>%
    as.data.frame(.) %>%
    rename(info = 1)
  
}

get_metdata <- function(image) {
  
  input %>%
    image_deskew() %>%
    image_contrast() %>%
    image_scale("x2000") %>%
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


table <- input %>%
  image_deskew() %>%
  image_contrast() %>%
  image_scale("x2000") %>%
image_write(format = 'png', density = '300x300') %>%
  tesseract::ocr(engine = eng) %>%
  str_split("\n") %>%
  as.data.frame(.) %>%
  rename(info = 1)

description <- input %>%
  image_deskew() %>%
  image_contrast() %>%
  image_scale("x2000") %>%
  image_write(format = 'png', density = '300x300') %>%
  tesseract::ocr(engine = eng_description) %>%
  str_split("\n") %>%
  as.data.frame(.) %>%
  rename(info = 1)

metadata <- input %>%
  image_deskew() %>%
  image_contrast() %>%
  image_scale("x2000") %>%
  image_negate()  %>%
  image_contrast() %>%
  image_modulate(brightness = 120) %>%
  #image_threshold("white", "20%") 
  image_write(format = 'png', density = '300x300') %>%
  tesseract::ocr(engine = eng_description) %>%
  str_split("\n") %>%
  as.data.frame(.) %>%
  rename(info = 1) 


