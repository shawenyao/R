suppressWarnings(library(tidyverse))
suppressWarnings(library(beepr))

setwd("C:/Users/Wenyao/Desktop/R/R/")

source("./functions/functions_save_png.R")


#==== general setup ====
r <- 80


#==== plot ====
plot <- tibble(theta = seq(from = 0, to = 2 * pi, by = 0.01)) %>%
  mutate(
    x = cos(theta) * r,
    y = sin(theta) * r
  ) %>% 
  ggplot() +
  
  # earth
  geom_path(aes(x = x, y = y), size = 1, color = "blue") +

  # rotational axis
  geom_segment(
    x = -r * 1.2 * cos(66.34 / 180 * pi),
    xend = r * 1.2 * cos(66.34 / 180 * pi),
    y = -r * 1.2 * sin(66.34 / 180 * pi),
    yend = r * 1.2 * sin(66.34 / 180 * pi),
    size = 1,
    linetype = "dotted"
  ) +
  # celestial equator
  geom_segment(
    x = -r * 1.2 * sin(66.34 / 180 * pi),
    xend = r * 1.2 * sin(66.34 / 180 * pi),
    y = r * 1.2 * cos(66.34 / 180 * pi),
    yend = -r * 1.2 * cos(66.34 / 180 * pi),
    size = 1,
    linetype = "dotted"
  ) +

  # orbital axis
  geom_vline(xintercept = 0, linetype = "longdash", size = 1) +

  # sun light/ecliptic
  geom_segment(
    x = -r * 1.2,
    xend = r * 1.2,
    y = 0,
    yend = 0,
    linetype = "longdash",
    size = 1,
    arrow = arrow(type = "closed", length = unit(0.1, "inches")),
    color = "orange"
  ) +
  # sun light 1
  geom_segment(
    x = -r * 1.2,
    xend = r * 1.2,
    y = sqrt(3) / 2 * r,
    yend = sqrt(3) / 2 * r,
    linetype = "longdash",
    size = 1,
    arrow = arrow(type = "closed", length = unit(0.1, "inches")),
    color = "orange"
  ) +
  # sun light 2
  geom_segment(
    x = -r * 1.2,
    xend = r * 1.2,
    y = -sqrt(3) / 2 * r,
    yend = -sqrt(3) / 2 * r,
    linetype = "longdash",
    size = 1,
    arrow = arrow(type = "closed", length = unit(0.1, "inches")),
    color = "orange"
  ) +

  # horizon
  geom_segment(
    x = -r,
    xend = 0,
    y = r / sqrt(3),
    yend = r / sqrt(3) * 2,
    linetype = "solid",
    size = 1
  ) +
  # center to observation point
  geom_segment(
    x = -r / 2,
    xend = 0,
    y = sqrt(3) / 2 * r,
    yend = 0,
    linetype = "solid",
    size = 1
  ) +

  # center
  geom_point(data = tibble(x = 0, y = 0), aes(x = x, y = y), size = 4, color = "red") +
  # observation point
  geom_point(data = tibble(x = -r / 2, y = sqrt(3) / 2 * r), aes(x = x, y = y),  size = 4, color = "red") +
  # subsolar point
  geom_point(data = tibble(x = -r, y = 0), aes(x = x, y = y),  size = 4, color = "red") +
  
  coord_fixed() +
  xlim(-r * 1.3, r * 1.3) +
  ylim(-r * 1.3, r * 1.3) +
  theme_minimal() +
  theme(
    text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank()
  )


#===== save =====
save_png(
  plot,
  file_name = "output/animal_crossing/1.png",
  width = 600, 
  height = 600
)

# play sound when finished
beep(sound = 2)

