suppressWarnings(library(tidyverse))
suppressWarnings(library(beepr))

setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_save_svg.R")

#==== plot ====
plot1 <- tibble(
  x = 0,
  y = 0,
  xend = 0,
  yend = 1
) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_segment(arrow = arrow(length = unit(0.5,"cm"), type = "closed"), size = 1.5, linejoin = "round", lineend = "round", color = "blue") +
  geom_point(aes(x = 0, y = 0), color = "black", size = 4) +
  xlim(-1, 1) +
  ylim(-0.5, 1.5) +
  coord_fixed() +
  theme_minimal() +
  theme(
    legend.position = "none", 
    axis.line = element_blank(),
    axis.text = element_blank(), 
    axis.title = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(1.5, "lines")
  )

plot2 <- tibble(
  x = c(0, 0, 0),
  y = c(0, 0, 0),
  xend = c(0, -sqrt(3)/2, sqrt(3)/2), 
  yend = c(1, -0.5, -0.5),
  id = 1:3
) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, group = id)) +
  geom_segment(arrow = arrow(length = unit(0.5,"cm"), type = "closed"), size = 1.5, linejoin = "round", lineend = "round", color = "blue") +
  geom_point(aes(x = 0, y = 0), color = "black", size = 4) +
  xlim(-1, 1) +
  ylim(-0.75, 1.25) +
  coord_fixed() +
  theme_minimal() +
  theme(
    legend.position = "none", 
    axis.line = element_blank(),
    axis.text = element_blank(), 
    axis.title = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(1.5, "lines")
  )


#==== output ====
save_svg(plot = plot1, file_name = "output/three_random_variables/max_rho.svg", width = 3, height = 3)
save_svg(plot = plot2, file_name = "output/three_random_variables/min_rho.svg", width = 3, height = 3)

# play sound when finished
beep(sound = 2)
