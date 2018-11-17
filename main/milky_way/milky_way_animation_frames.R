library(tidyverse)
library(beepr)

setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_plot_milky_way.R")
source("./functions/functions_save_jpg.R")
set.seed(1)

milky_way_frame <- function(){
  
  plot_milky_way(
    num_of_arms = 4,
    theta_from = c(2.7, 3.3, 2.7, 3.3),
    theta_to = c(7.75, 8.5, 7.5, 8.25),
    theta_length = 1000,
    theta_power = -1.5,
    arm_sd_x = 0.01,
    arm_sd_y = 0.01,
    arm_width = c(1.1, 0.8, 1.1, 0.8),
    arm_alpha_from = 0.5,
    arm_alpha_to = 0.05,
    
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
    gc_alpha_from = 0.01,
    gc_alpha_slope = 0.2,
    gc_color = c("lemonchiffon", "lemonchiffon1", "khaki1", "khaki2", "gold"),
    
    star_halo_size1 = 80,
    star_halo_size2 = 120,
    star_alpha_adj1 = 1 / 18,
    star_alpha_adj2 = 1 / 36,
    
    gc_halo_size1 = 30,
    gc_halo_size2 = 60,
    gc_halo_alpha1 = 0.01,
    gc_halo_alpha2 = 0.005,
    
    x_axis_range = c(-26, 26),
    y_axis_range = c(-26, 26),
    background_color = "#000011"
  )
}

for(i in 1:20){
  
  print(paste0("Processing frame ", i))
  
  milky_way_frame() %>% 
    save_jpg(
      file_name = paste0("output/milky_way/frames/milky_way_frame_", str_pad(i, 2, side = "left", pad = "0"), ".jpg"),
      width = 2900, 
      height = 2900,
      print_plot = FALSE,
      bg = "#000011"
    )
}

# play sound when finished
beep(sound = 2)
