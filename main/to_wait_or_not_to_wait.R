suppressWarnings(library(tidyverse))
suppressWarnings(library(beepr))

setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_save_svg.R")


#==== initialization ====
set.seed(10)

# the width/height of each block
block_width <- 1

# number of roads (either horizontal or vertical) 
road_dim <- 5

# the "importance" of each horizontal road
road_x <- tibble(
  name = seq_len(road_dim),
  importance = runif(road_dim, min = 0.1, max = 0.9)
)
# the "importance" of each vertical road
road_y <- tibble(
  name = seq_len(road_dim),
  importance = runif(road_dim, min = 0.1, max = 0.9)
)

# the center coordinates of blocks
block_x <- cumsum(c(0, road_y$importance)) +
  cumsum(c(block_width / 2, rep(block_width, times = road_dim)))
block_y <- cumsum(c(0, road_x$importance)) +
  cumsum(c(block_width / 2, rep(block_width, times = road_dim)))

road_network <- expand.grid(
  x = block_x,
  y = block_y
)

# plot
plot1 <- ggplot(road_network) +
  
  # the road blocks
  geom_tile(
    aes(x = x, y = y), 
    width = block_width, 
    height = block_width, 
    fill = "white", 
    colour = "black", 
    size = 0.5, 
    linejoin = "mitre"
  ) +
  
  # remove the outline of the outmost rectangle
  geom_rect(
    data = tibble(
      xmin = 0, 
      xmax = last(block_x) + block_width / 2, 
      ymin = 0, 
      ymax = last(block_y) + block_width / 2
    ), 
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
    fill = "white",
    colour = "white", 
    alpha = 0,
    size = 1, 
    linejoin = "mitre"
  ) +
  
  # the key points
  geom_point(
    data = tibble(
      x = c(block_width, last(block_x) - block_width / 2),
      y = c(block_width, last(block_y) - block_width / 2), 
    ),
    aes(x = x, y = y),
    color = c("blue", "blue"),
    size = 1.5
  ) +
  
  # labels for the key points
  geom_text(
    data = tibble(
      x = c(first(block_x) + block_width / 4, last(block_x) - block_width / 4),
      y = c(first(block_y) + block_width / 4, last(block_y) - block_width / 4),
      label = c("A", "B")
    ),
    aes(x = x, y = y, label = label)
  ) +
  coord_fixed() +
  scale_y_reverse() +
  theme_minimal() +
  theme(
    text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank()
  )

plot2 <- plot1 +
  
  # the key points cont'd
  geom_point(
    data = tibble(
      x = c(nth(block_x, road_dim) + block_width / 2, last(block_x) - block_width / 2),
      y = c(last(block_y) - block_width / 2, nth(block_y, road_dim) + block_width / 2), 
    ),
    aes(x = x, y = y),
    color = c("red", "red"),
    size = 1.5
  ) +
  
  # labels for the key points
  geom_text(
    data = tibble(
      x = c(nth(block_x, road_dim) + block_width / 4, last(block_x) - block_width / 4),
      y = c(last(block_y) - block_width / 4, nth(block_y, road_dim) + block_width / 4),
      label = c("C", "D")
    ),
    aes(x = x, y = y, label = label)
  )


#==== output ====
save_svg(plot = plot1, file_name = "output/to_cross_or_not_to_cross/plot1.svg", width = 3.5, height = 3.5)
save_svg(plot = plot2, file_name = "output/to_cross_or_not_to_cross/plot2.svg", width = 3.5, height = 3.5)

# play sound when finished
beep(sound = 2)

