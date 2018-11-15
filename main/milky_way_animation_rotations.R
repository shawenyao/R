library(OpenImageR)
library(tidyverse)
library(beepr)

setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_save_jpg.R")
set.seed(1)

setwd("C:/Users/Wenyao/Desktop/R/R/output/milky_way")

files <- list.files("frames") %>% 
  # 30 fps for 6 seconds
  sample(size = 30 * 6, replace = TRUE)

for(i in seq_along(files)){
  
  print(paste0("Processing frame ", i))
  
  frame <- paste0("./frames/", files[i]) %>% 
    readImage() %>% 
    resizeImage(width = 800, height = 800) %>% 
    rotateImage(angle = seq(from = 360, to = 360 / length(files), length.out = length(files))[i])
  
  # fix background color after rotation
  frame[frame[,,1] == 0 & frame[,,2] == 0 & frame[,,3] == 0, 1] = 0
  frame[frame[,,1] == 0 & frame[,,2] == 0 & frame[,,3] == 0, 2] = 0
  frame[frame[,,1] == 0 & frame[,,2] == 0 & frame[,,3] == 0, 3] = 17
  
  writeImage(
    data = frame,
    file_name = paste0("./rotated_frames/milky_way_rotated_frame_", str_pad(i, 3, side = "left", pad = "0"), ".jpg"),
    bg = "#000011",
    quality = 0.75
  )
}

# play sound when finished
beep(sound = 2)
