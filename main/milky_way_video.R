library(OpenImageR)
library(imager)
library(tidyverse)
library(beepr)

#==== general setup ====
setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_plot_milky_way.R")
source("./functions/functions_save_jpg.R")
set.seed(350)
# number of raw frames
N <- 25
# number of rotated frames
M <- 25 * 40


#==== create frames ====
milky_way_frame <- function(
  arm_alpha_from,
  arm_alpha_to,
  gc_alpha_from,
  gc_alpha_slope,
  star_alpha_adj1,
  star_alpha_adj2,
  gc_halo_alpha1,
  gc_halo_alpha2,
  seed
){
  
  plot_milky_way(
    num_of_arms = 4,
    theta_from = c(2.7, 3.3, 2.7, 3.3),
    theta_to = c(7.75, 8.5, 7.5, 8.25),
    theta_length = 1000,
    theta_power = -1.5,
    arm_sd_x = 0.01,
    arm_sd_y = 0.01,
    arm_width = c(1.1, 0.8, 1.1, 0.8),
    arm_alpha_from = arm_alpha_from,
    arm_alpha_to = arm_alpha_to,
    
    star_intensity = 5,
    star_size_from = 0.5,
    star_size_to = 10,
    star_size_interval = 0.5,
    star_color = c("#64C5E8", "#81D3DE", "#C5DED8", "#E3E9D2", "#F2FFCF", "firebrick1"),
    
    gc_rho = 0.9,
    gc_intensity = 10000,
    gc_sd_x = 1.5,
    gc_sd_y = 2.25,
    gc_size_from = 0.5,
    gc_size_to = 12,
    gc_size_interval = 0.5,
    gc_alpha_from = gc_alpha_from,
    gc_alpha_slope = gc_alpha_slope,
    gc_color = c("lemonchiffon", "lemonchiffon1", "khaki1", "khaki2", "gold"),
    
    star_halo_size1 = 80,
    star_halo_size2 = 120,
    star_alpha_adj1 = star_alpha_adj1,
    star_alpha_adj2 = star_alpha_adj2,
    
    gc_halo_size1 = 30,
    gc_halo_size2 = 60,
    gc_halo_alpha1 = gc_halo_alpha1,
    gc_halo_alpha2 = gc_halo_alpha2,
    
    x_axis_range = c(-26, 26),
    y_axis_range = c(-26, 26),
    background_color = "#000011",
    seed = seed
  )
}

# pre-draw noises
rand_adj <- 1 / 10
r_arm_alpha_from <- rnorm(N, sd = 0.5 * rand_adj)
r_arm_alpha_to <- rnorm(N, sd = 0.05 * rand_adj)
r_gc_alpha_from <- rnorm(N, sd = 0.01 * rand_adj)
r_gc_alpha_slope <- rnorm(N, sd = 0.2 * rand_adj / 3)
r_star_alpha_adj1 <- rnorm(N, sd = 18 * rand_adj)
r_star_alpha_adj2 <- rnorm(N, sd = 36 * rand_adj)
r_gc_halo_alpha1 <- rnorm(N, sd = 0.01 * rand_adj)
r_gc_halo_alpha2 <- rnorm(N, sd = 0.005 * rand_adj)

for(i in 1:N){
  
  print(paste0("Processing raw frame ", i))
  
  milky_way_frame(
    arm_alpha_from = (0.5 + r_arm_alpha_from[i]) %>% pmax(0) %>% pmin(1),
    arm_alpha_to = (0.05 + r_arm_alpha_to[i]) %>% pmax(0) %>% pmin(1),
    gc_alpha_from = (0.01 + r_gc_alpha_from[i]) %>% pmax(0) %>% pmin(1),
    gc_alpha_slope = (0.2 + r_gc_alpha_slope[i]) %>% pmax(0) %>% pmin(1),
    star_alpha_adj1 = (1 / (18 + r_star_alpha_adj1[i])) %>% pmax(0) %>% pmin(1),
    star_alpha_adj2 = (1 / (36 + r_star_alpha_adj2[i])) %>% pmax(0) %>% pmin(1),
    gc_halo_alpha1 = (0.01 + r_gc_halo_alpha1[i]) %>% pmax(0) %>% pmin(1),
    gc_halo_alpha2 = (0.005 + r_gc_halo_alpha2[i]) %>% pmax(0) %>% pmin(1),
    seed = 1
  ) %>% 
    save_jpg(
      file_name = paste0("output/milky_way/video_frames/raw_", str_pad(i, 3, side = "left", pad = "0"), ".jpg"),
      width = 2900, 
      height = 2900,
      print_plot = FALSE,
      bg = "#000011"
    )
}


#==== rotate frames ====
setwd("C:/Users/Wenyao/Desktop/R/R/output/milky_way")
files <- list.files(path = "video_frames") %>% 
  sample(size = M, replace = TRUE)

for(i in seq_along(files)){
  
  print(paste0("Processing frame ", i))
  
  frame <- paste0("./video_frames/", files[i]) %>% 
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


#==== encode video ====
setwd("C:/Users/Wenyao/Desktop/R/R/output/milky_way")
system2("ffmpeg", "-i video_frames/frame_%03d.jpg -y -c:v libx264 -preset veryslow -r 25 -tune stillimage -crf 30 videos/milky_way.mp4")

# play sound when finished
beep(sound = 2)

