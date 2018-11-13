library(tidyverse)
library(beepr)

setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_plot_milky_way.R")
source("./functions/functions_save_png.R")
set.seed(1)

milky_way_plot_large <- plot_milky_way(
  num_of_arms = 4,
  theta_from = c(2.7, 3.3, 2.7, 3.3),
  theta_to = c(7.75, 8.5, 7.5, 8.25),
  theta_length = 1000,
  arm_width = c(1.1, 0.8, 1.1, 0.8),
  theta_power = -1.5,
  arm_alpha_from = 0.4,
  arm_alpha_to = 0.01,
  
  star_intensity = 2,
  star_size_from = 0.5,
  star_size_to = 2,
  star_size_interval = 0.1,
  star_color = c("#64C5E8", "#81D3DE", "#C5DED8", "#E3E9D2", "#F2FFCF", "firebrick1"),
  
  gc_rho = 0.9,
  gc_intensity = 1500,
  gc_sd_x = 2.25,
  gc_sd_y = 2.25,
  gc_size_from = 0.5,
  gc_size_to = 3,
  gc_size_interval = 0.5,
  gc_alpha_from = 0.01,
  gc_alpha_slope = 0.2,
  gc_color = c("lemonchiffon", "lemonchiffon1", "khaki1", "khaki2", "gold"),
  
  star_halo_size1 = 40,
  star_halo_size2 = 60,
  star_alpha_adj1 = 1 / 18,
  star_alpha_adj2 = 1 / 36,
  
  gc_halo_size1 = 15,
  gc_halo_size2 = 30,
  gc_halo_alpha1 = 0.01,
  gc_halo_alpha2 = 0.005,
  
  background_color = "#000011"
)

save_png(
  milky_way_plot_large,
  file_name = "output/milky_way/milky_way.png",
  width = 880, 
  height = 1000,
  print_plot = FALSE,
  bg = "#000011"
)

# play sound when finished
beep(sound = 2)
