library(OpenImageR)
library(imager)
library(tidyverse)
library(beepr)

setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_save_jpg.R")
set.seed(1)

setwd("C:/Users/Wenyao/Desktop/R/R/output/milky_way")

files <- list.files("frames")[1] %>% 
  # 25 fps for 20 seconds
  sample(size = 25 * 30, replace = TRUE)

for(i in seq_along(files)){
  
  print(paste0("Processing frame ", i))
  
  frame <- paste0("./frames/", files[i]) %>% 
    readImage() %>% 
    resizeImage(width = 800, height = 800) %>% 
    rotateImage(angle = seq(from = 360, to = 360 / length(files), length.out = length(files))[i])
  
  # fix background color after rotation
  black_pixel_index <- frame[,,1] == 0 & frame[,,2] == 0 & frame[,,3] == 0
  frame[,,1][black_pixel_index] <-  0.003921569
  frame[,,2][black_pixel_index] <-  0
  frame[,,3][black_pixel_index] <-  0.0627451
  
  writeImage(
    data = frame,
    file_name = paste0("./video_frames/frame_", str_pad(i, 3, side = "left", pad = "0"), ".jpg"),
    bg = "#000011",
    quality = 0.75
  )
}

system2("ffmpeg", "-i video_frames/frame_%03d.jpg -y -c:v libx264 -preset veryslow -r 25 -tune stillimage -crf 30 videos/milky_way.mp4")

# play sound when finished
beep(sound = 2)

