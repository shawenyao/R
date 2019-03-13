library(tidyverse)
library(latex2exp)
library(beepr)

setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_save_svg.R")

#==== plot ====
plot1 <- tibble(
  x = 2:20,
  y_upper = 1
) %>% 
  mutate(y_lower = -1 / (x - 1)) %>% 
  ggplot(aes(x = x)) +
  geom_ribbon(aes(x = x, ymin = y_lower, ymax = y_upper), alpha = 0.1) +
  geom_line(aes(y = y_lower), color = "gray66", size = 2) +
  geom_point(aes(y = y_lower), color = "coral3", size = 5) +
  geom_point(aes(y = y_lower), color = "white", size = 2) +
  geom_line(aes(y = y_upper), color = "gray66", size = 2) +
  geom_point(aes(y = y_upper), color = "dodgerblue3", size = 5) +
  geom_point(aes(y = y_upper), color = "white", size = 2) +
  ylim(-1, 1) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))
  ) +
  labs(x = "", y = TeX("$\\rho$"))

plot2 <- tibble(
  x = 2:20,
  y_upper = 0
) %>% 
  mutate(y_lower = acos(-1 / (x - 1)) / pi * 180) %>% 
  ggplot(aes(x = x)) +
  geom_ribbon(aes(x = x, ymin = y_lower, ymax = y_upper), alpha = 0.1) +
  geom_line(aes(y = y_lower), color = "gray66", size = 2) +
  geom_point(aes(y = y_lower), color = "coral3", size = 5) +
  geom_point(aes(y = y_lower), color = "white", size = 2) +
  geom_line(aes(y = y_upper), color = "gray66", size = 2) +
  geom_point(aes(y = y_upper), color = "dodgerblue3", size = 5) +
  geom_point(aes(y = y_upper), color = "white", size = 2) +
  ylim(0, 180) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))
  ) +
  labs(x = "", y = TeX("$\\theta^{o}$"))


#==== output ====
save_svg(plot = plot1, file_name = "output/three_random_variables_part_ii/rho_range.svg", width = 6, height = 3)
save_svg(plot = plot2, file_name = "output/three_random_variables_part_ii/theta_range.svg", width = 6, height = 3)

# play sound when finished
beep(sound = 2)
