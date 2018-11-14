library(OpenImageR)
library(tidyverse)
library(beepr)

setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_save_jpg.R")
set.seed(1)

setwd("C:/Users/Wenyao/Desktop/R/R/output/milky_way")

files <- list.files("frames") %>% 
  # 30 fps for 10 seconds
  sample(size = 30 * 10, replace = TRUE)

for(i in seq_along(files)){
  
  print(paste0("Processing frame ", i))
  
  frame <- paste0("./frames/", files[i]) %>% 
    readImage() %>% 
    resizeImage(width = 880, height = 880) %>% 
    rotateImage(angle = seq(from = 360, to = 360 / length(files), length.out = length(files))[i])
  
  writeImage(
    data = frame,
    file_name = paste0("./rotated_frames/milky_way_rotated_frame_", str_pad(i, 2, side = "left", pad = "0"), ".jpg"),
    bg = "#000011",
    quality = 0.8
  )
}

# play sound when finished
beep(sound = 2)
