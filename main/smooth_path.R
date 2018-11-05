library(dplyr)
library(purrr)
library(ggplot2)
library(beepr)

setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_save_svg.R")
source("./functions/functions_smooth_path.R")

# the Cartiesian coordinates of the original path
path <- data.frame(
  x = c(1, 3, 4, 1,-1,-0.5, 2),
  y = c(3, 4, 2, 1.5, 4, 6, 7)
)

plot1 <- bind_rows(
  path %>% 
    mutate(iteration = "original"),
  path %>% 
    smooth_path_double() %>% 
    smooth_path_double() %>% 
    mutate(iteration = "smoothed")
) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_path(size = 3, linejoin = "round", lineend = "round") +
  geom_point(size = 8, aes(color = iteration)) +
  scale_color_manual(values = c("original" = "#F8766D", "smoothed" = "#619CFF")) +
  scale_x_continuous(breaks = seq(min(path$x), max(path$x), 1), limits = c(min(path$x) - 0.4, max(path$x) + 0.4)) +
  scale_y_continuous(breaks = seq(min(path$y), max(path$y), 1), limits = c(min(path$y) - 0.4, max(path$y) + 0.4)) +
  coord_fixed() +
  facet_wrap(iteration~.) +
  theme_minimal() +
  theme(legend.position = "none", axis.text = element_blank(), strip.text = element_blank()) +
  labs(x = "", y = "")

save_svg(plot = plot1, file_name = "output/plot1.svg", width = 4, height = 8)


bind_rows(
  path %>% 
    mutate(iteration = "Original", type = "1-way"),
  path %>% 
    smooth_path() %>% mutate(iteration = "Smooth x 1", type = "1-way"),
  path %>% 
    smooth_path() %>% 
    smooth_path() %>% 
    mutate(iteration = "Smooth x 2", type = "1-way"),
  
  path %>% 
    mutate(iteration = "Original", type = "1-way Reversed"),
  path %>%
    reverse_df() %>% 
    smooth_path() %>%
    reverse_df() %>% 
    mutate(iteration = "Smooth x 1", type = "1-way Reversed"),
  path %>% 
    reverse_df() %>% 
    smooth_path() %>%
    smooth_path() %>%
    reverse_df() %>% 
    mutate(iteration = "Smooth x 2", type = "1-way Reversed"),
  
  path %>% 
    mutate(iteration = "Original", type = "2-way"),
  path %>% 
    smooth_path_double() %>% 
    mutate(iteration = "Smooth x 1", type = "2-way"),
  path %>% 
    smooth_path_double() %>% 
    smooth_path_double() %>% 
    mutate(iteration = "Smooth x 2", type = "2-way")
) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_path(size = 3, linejoin = "round", lineend = "round") +
  geom_point(size = 6, aes(color = iteration)) +
  facet_grid(type~iteration) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "") +
  coord_fixed()

# play sound when finished
beep(sound = 2)
