library(tidyverse)
library(magick)
library(gtools)

all_grids <- list.files(here::here("Scans", "Grids", "UpperDeck_1998"))

fronts <- all_grids[str_detect(all_grids, "front")]
fronts <- mixedsort(fronts)

front_images <- map(fronts, ~ image_read(here::here("Scans", "Grids", "UpperDeck_1998", .)))

front_smaller <- map(front_images, ~ image_scale(., "x500"))

for (i in seq(1, length(front_smaller))) {
  
  image_write(front_smaller[[i]], format <- "jpg", path = here::here("Scans", "Grids","UpperDeck_1998", "Smaller", fronts[[i]]))
}
  

