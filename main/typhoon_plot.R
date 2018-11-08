library(tidyverse)
library(beepr)

setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_save_svg.R")
source("./functions/functions_save_png.R")

set.seed(1)

iteration <- 2
total <- 30
data <- seq_len(iteration) %>% 
  map(function(i){
    seq_len(total) %>% 
      map(function(j){
        tibble(
          id = j + (i - 1) * total,
          theta = seq(from = sample(3:10, size = 1), to = sample(11:30, size = 1), length.out = 1000)
        ) %>% 
          mutate(
            r = 1 / theta,
            x = r * cos(theta + 2 * pi * (j + rnorm(1, sd = 0.2)) / total) + rnorm(1, sd = 0.005),
            y = r * sin(theta + 2 * pi * (j + rnorm(1, sd = 0.2)) / total) + rnorm(1, sd = 0.005)
          )
      }) %>% 
      bind_rows()
  }) %>% 
  bind_rows()

plot1 <- ggplot(data, aes(x = x, y = y, group = id)) +
  geom_path(size = 0.4, lineend = "round", alpha = 0.2) +
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

save_svg(plot = plot1, file_name = "output/typhoon/typhoon.svg", width = 10, height = 10)


set.seed(1)

iteration <- 10
total <- 60
data <- seq_len(iteration) %>% 
  map(function(i){
    seq_len(total) %>% 
      map(function(j){
        tibble(
          id = j + (i - 1) * total,
          theta = seq(from = sample(3:10, size = 1), to = sample(11:30, size = 1), length.out = 1000)
        ) %>% 
          mutate(
            r = 1 / theta,
            x = r * cos(theta + 2 * pi * (j + rnorm(1, sd = 0.3)) / total) + rnorm(1, sd = 0.0075),
            y = r * sin(theta + 2 * pi * (j + rnorm(1, sd = 0.3)) / total) + rnorm(1, sd = 0.0075)
          )
      }) %>% 
      bind_rows()
  }) %>% 
  bind_rows()

plot2 <- ggplot(data, aes(x = x, y = y, group = id)) +
  geom_path(size = 0.4, lineend = "round", alpha = 0.2) +
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

save_png(plot = plot2, print_plot = TRUE, file_name = "output/typhoon/typhoon2.png", width = 880, height = 880)

# play sound when finished
beep(sound = 2)
