library(dplyr)
library(purrr)
library(ggplot2)
library(colourlovers)
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
  geom_path(size = 2.5, linejoin = "round", lineend = "round") +
  geom_point(size = 5, aes(color = iteration)) +
  scale_color_manual(values = c("Original" = "#F8766D", "Smoothed" = "#619CFF")) +
  scale_x_continuous(breaks = seq(min(path$x), max(path$x), 1), limits = c(min(path$x) - 0.6, max(path$x) + 0.6)) +
  scale_y_continuous(breaks = seq(min(path$y), max(path$y), 1), limits = c(min(path$y) - 0.6, max(path$y) + 0.6)) +
  coord_fixed() +
  facet_wrap(iteration~.) +
  theme_minimal() +
  theme(legend.position = "none", axis.text = element_blank(), strip.text = element_blank()) +
  labs(x = "", y = "")

save_svg(plot = plot1, file_name = "output/smooth_path/plot1.svg", width = 6, height = 3)


#==== plot 2 & 3 ====
simple_path <- tibble(
  x = c(1, 6, 7.5),
  y = c(1, 6, 2),
  label = c("A", "B", "C"),
  iteration = "Original"
)

added_points <- tibble(
  x = c(3.5, 2.086197, 4.75),
  y = c(3.5, 4.913803, 4.75),
  label = c("D", "P", "E"),
  iteration = "Added"
)

all_points <- bind_rows(simple_path, added_points)

plot_problem_formulation <- simple_path %>% 
  ggplot(aes(x = x, y = y)) +
  geom_path(size = 2.5, linejoin = "round", lineend = "round") +
  geom_line(data = all_points %>% filter(label %in% c("A", "B", "P")), size = 1.5, linetype = 2) +
  geom_point(data = all_points %>% filter(label %in% c("A", "B", "C", "P")), size = 5, aes(color = iteration)) +
  geom_text(data = all_points %>% filter(label %in% c("A", "B", "C", "P")), aes(x = x - 0.6, y = y + 0.2, label = label), size = 8) +
  scale_color_manual(values = c("Original" = "#F8766D", "Added" = "gold")) +
  scale_x_continuous(breaks = seq(min(simple_path$x), max(simple_path$x), 1), limits = c(min(simple_path$x) - 0.6, max(simple_path$x) + 0.6)) +
  scale_y_continuous(breaks = seq(min(simple_path$y), max(simple_path$y), 1), limits = c(min(simple_path$y) - 0.3, max(simple_path$y) + 0.3)) +
  coord_fixed() +
  theme_minimal() +
  theme(legend.position = "none", axis.text = element_blank(), strip.text = element_blank()) +
  labs(x = "", y = "")

save_svg(plot_problem_formulation, file_name = "output/smooth_path/plot_problem_formulation.svg", width = 4, height = 3)


plot_heuristic_solution <- simple_path %>% 
  ggplot(aes(x = x, y = y)) +
  geom_path(size = 2.5, linejoin = "round", lineend = "round") +
  geom_line(data = all_points %>% filter(label %in% c("D", "P")), size = 1.5, linetype = 2) +
  geom_line(data = all_points %>% filter(label %in% c("C", "E")), size = 1.5, linetype = 2) +
  geom_point(data = all_points, size = 5, aes(color = iteration)) +
  geom_text(data = all_points, aes(x = x - 0.6, y = y + 0.2, label = label), size = 8) +
  scale_color_manual(values = c("Original" = "#F8766D", "Added" = "gold")) +
  scale_x_continuous(breaks = seq(min(simple_path$x), max(simple_path$x), 1), limits = c(min(simple_path$x) - 0.6, max(simple_path$x) + 0.6)) +
  scale_y_continuous(breaks = seq(min(simple_path$y), max(simple_path$y), 1), limits = c(min(simple_path$y) - 0.3, max(simple_path$y) + 0.3)) +
  coord_fixed() +
  theme_minimal() +
  theme(legend.position = "none", axis.text = element_blank(), strip.text = element_blank()) +
  labs(x = "", y = "")

save_svg(plot_heuristic_solution, file_name = "output/smooth_path/plot_heuristic_solution.svg", width = 4, height = 3)


#==== final example ====
plot_example <- bind_rows(
  path %>% 
    mutate(iteration = "Original", type = "Forward"),
  path %>% 
    smooth_path() %>% mutate(iteration = "Smooth x 1", type = "Forward"),
  path %>% 
    smooth_path() %>% 
    smooth_path() %>% 
    mutate(iteration = "Smooth x 2", type = "Forward"),
  
  path %>% 
    mutate(iteration = "Original", type = "Backward"),
  path %>%
    reverse_df() %>% 
    smooth_path() %>%
    reverse_df() %>% 
    mutate(iteration = "Smooth x 1", type = "Backward"),
  path %>% 
    reverse_df() %>% 
    smooth_path() %>%
    smooth_path() %>%
    reverse_df() %>% 
    mutate(iteration = "Smooth x 2", type = "Backward"),
  
  path %>% 
    mutate(iteration = "Original", type = "Average"),
  path %>% 
    smooth_path_double() %>% 
    mutate(iteration = "Smooth x 1", type = "Average"),
  path %>% 
    smooth_path_double() %>% 
    smooth_path_double() %>% 
    mutate(iteration = "Smooth x 2", type = "Average")
) %>% 
  mutate(type = factor(type, levels = unique(type))) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_path(size = 2.5, linejoin = "round", lineend = "round") +
  geom_point(size = 5, aes(color = iteration)) +
  facet_grid(type~iteration, switch = "y") +
  scale_x_continuous(breaks = seq(min(path$x), max(path$x), 1), limits = c(min(path$x) - 0.6, max(path$x) + 0.6)) +
  scale_y_continuous(breaks = seq(min(path$y), max(path$y), 1), limits = c(min(path$y) - 0.6, max(path$y) + 0.6)) +
  coord_fixed() +
  theme_minimal() +
  theme(legend.position = "none", axis.text = element_blank(), strip.text = element_text(size = 20)) +
  labs(x = "", y = "")

save_svg(plot = plot_example, file_name = "output/smooth_path/plot_example.svg", width = 10, height = 10)


#==== the effect of lambda plot ====
path_lambda <- seq(0, 0.3, length.out = 200) %>% 
  map(
    function(lambda){
      
      output <- path %>% 
        smooth_path_double(lambda = lambda) %>% 
        smooth_path_double(lambda = lambda) %>% 
        smooth_path_double(lambda = lambda) %>% 
        smooth_path_double(lambda = lambda) %>%
        mutate(lambda = lambda)
    }
  ) %>% 
  bind_rows()

plot_lambda <- path_lambda %>% 
  # filter(lambda %in% a[1:3]) %>%
  ggplot(aes(x = x, y = y)) +
  geom_path(size = 1, linejoin = "round", lineend = "round", aes(color = factor(lambda))) +
  scale_colour_hue(h = c(270, 360)) +
  # scale_color_gradientn(colours = clpalette('4607999') %>% swatch() %>% .[[1]]) +
  scale_x_continuous(breaks = seq(min(path$x), max(path$x), 1), limits = c(min(path$x) - 0.6, max(path$x) + 0.6)) +
  scale_y_continuous(breaks = seq(min(path$y), max(path$y), 1), limits = c(min(path$y) - 0.6, max(path$y) + 0.6)) +
  coord_fixed() +
  theme_minimal() +
  theme(legend.position = "none", axis.text = element_blank(), strip.text = element_text(size = 20)) +
  labs(x = "", y = "")

save_svg(plot = plot_lambda, file_name = "output/smooth_path/plot_lambda.svg", width = 10, height = 10)


# play sound when finished
beep(sound = 2)
