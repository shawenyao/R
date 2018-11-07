library(tidyverse)
library(beepr)

setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_save_svg.R")

set.seed(1)

a <- 6
b <- 3

total <- 60
data <- seq_len(total) %>% 
  map(function(i){
    tibble(
      id = i,
      theta = seq(from = sample(3:10, size = 1), to = sample(11:30, size = 1), length.out = 1000)
    ) %>% 
      mutate(
        r = 1 / theta,
        x = r * cos(theta + 2 * pi * (i + rnorm(1, sd = 0.2)) / total),
        y = r * sin(theta + 2 * pi * (i + rnorm(1, sd = 0.2)) / total)
      )
  }) %>% 
  bind_rows()

plot <- ggplot(data, aes(x = x, y = y, group = id)) +
  geom_path(size = 0.5) +
  coord_fixed() +
  theme_minimal() +
  theme(
    legend.position = "none", 
    axis.line = element_blank(),
    axis.text = element_blank(), 
    strip.text = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(x = "", y = "")

save_svg(plot = plot, file_name = "output/typhoon/typhoon.svg", width = 6, height = 6)

# play sound when finished
beep(sound = 2)

