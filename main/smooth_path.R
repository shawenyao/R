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

#==== plot 1 ====
plot1 <- bind_rows(
  path %>% 
    mutate(iteration = "Original"),
  path %>% 
    smooth_path_double() %>% 
    smooth_path_double() %>% 
    mutate(iteration = "Smoothed")
) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_path(size = 3, linejoin = "round", lineend = "round") +
  geom_point(size = 5, aes(color = iteration)) +
  scale_color_manual(values = c("Original" = "#F8766D", "Smoothed" = "#619CFF")) +
  scale_x_continuous(breaks = seq(min(path$x), max(path$x), 1), limits = c(min(path$x) - 0.6, max(path$x) + 0.6)) +
  scale_y_continuous(breaks = seq(min(path$y), max(path$y), 1), limits = c(min(path$y) - 0.6, max(path$y) + 0.6)) +
  coord_fixed() +
  facet_wrap(iteration~.) +
  theme_minimal() +
  theme(legend.position = "none", axis.text = element_blank(), strip.text = element_blank()) +
  labs(x = "", y = "")

save_svg(plot = plot1, file_name = "output/plot1.svg", width = 6, height = 3)


#==== final example ====
plot_example <- bind_rows(
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
  geom_point(size = 5, aes(color = iteration)) +
  facet_grid(type~iteration, switch = "y") +
  scale_x_continuous(breaks = seq(min(path$x), max(path$x), 1), limits = c(min(path$x) - 0.6, max(path$x) + 0.6)) +
  scale_y_continuous(breaks = seq(min(path$y), max(path$y), 1), limits = c(min(path$y) - 0.6, max(path$y) + 0.6)) +
  coord_fixed() +
  theme_minimal() +
  theme(legend.position = "none", axis.text = element_blank(), strip.text = element_text(size = 20)) +
  labs(x = "", y = "")

save_svg(plot = plot_example, file_name = "output/plot_example.svg", width = 10, height = 10)

# play sound when finished
beep(sound = 2)
