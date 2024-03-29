library(tidyverse)
library(beepr)

setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_plot_typhoon.R")
source("./functions/functions_save_png.R")


set.seed(350)

list(
  plot_id = 1:3,
  iteration = c(3, 5, 15),
  total = c(30, 45, 60)
) %>% 
  pmap(
    function(plot_id, iteration, total){
      plot_typhoon(iteration = iteration, total = total) %>% 
        save_png(
          file_name = paste0("output/typhoon/typhoon", plot_id, ".png"),
          width = 880, 
          height = 880,
          print_plot = FALSE
        )
    }
  )

# play sound when finished
beep(sound = 2)
