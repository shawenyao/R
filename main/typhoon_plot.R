library(tidyverse)
library(beepr)

setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_plot_typhoon.R")
source("./functions/functions_save_svg.R")
source("./functions/functions_save_png.R")


set.seed(1)

list(
  plot_id = 1:3,
  iteration = c(1, 3, 10),
  total = c(30, 45, 60)
) %>% 
  pmap(
    function(plot_id, iteration, total){
      plot_typhoon(iteration = iteration, total = total) %>% 
        save_png(
          file_name = paste0("output/typhoon/typhoon", plot_id, ".png"),
          width = 880, 
          height = 880
        )
    }
  )

# play sound when finished
beep(sound = 2)
