library(tidyverse)
library(beepr)

#==== general setup ====
setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_plot_milky_way.R")
source("./functions/functions_save_jpg.R")

set.seed(1)


#==== galaxy parameters ====
num_of_arms <- 4
theta_from <- c(2.7, 3.3, 2.7, 3.3)
theta_to <- c(7.75, 8.5, 7.5, 8.25)
theta_length <- 1000
theta_power <- -1.5
arm_sd_x <- 0.01
arm_sd_y <- 0.01
arm_width <- c(1.1, 0.8, 1.1, 0.8)
arm_alpha_from <- 0.5
arm_alpha_to <- 0.05

star_intensity <- 5
star_size_from <- 0.25
star_size_to <- 5
star_size_interval <- 0.5
star_color <- c("#64C5E8", "#81D3DE", "#C5DED8", "#E3E9D2", "#F2FFCF", "firebrick1")

gc_rho <- 0.9
gc_intensity <- 10000
gc_sd_x <- 1.5
gc_sd_y <- 2.25
gc_size_from <- 0.5
gc_size_to <- 12
gc_size_interval <- 0.5
gc_alpha_from <- 0.01
gc_alpha_slope <- 0.2
gc_color <- c("lemonchiffon", "lemonchiffon1", "khaki1", "khaki2", "gold")

star_halo_size1 <- 30
star_halo_size2 <- 45
star_alpha_adj1 <- 1 / 18
star_alpha_adj2 <- 1 / 36

gc_halo_size1 <- 30
gc_halo_size2 <- 60
gc_halo_alpha1 <- 0.01
gc_halo_alpha2 <- 0.005

background_color <- "#000011"


#==== galaxy data ====
sprial_arms <- get_spiral_arms(
  num_of_arms = num_of_arms,
  theta_from = theta_from,
  theta_to = theta_to,
  theta_length = theta_length,
  theta_power = theta_power,
  arm_sd_x = arm_sd_x,
  arm_sd_y = arm_sd_y,
  arm_width = arm_width,
  arm_alpha_from = arm_alpha_from,
  arm_alpha_to = arm_alpha_to
)

stars <- get_stars(
  sprial_arms = sprial_arms,
  star_intensity = star_intensity,
  star_size_from = star_size_from,
  star_size_to = star_size_to,
  star_size_interval = star_size_interval,
  star_color = star_color
)

gc <- get_galactic_center(
  gc_rho = gc_rho,
  gc_intensity = gc_intensity,
  gc_sd_x = gc_sd_x,
  gc_sd_y = gc_sd_y,
  gc_size_from = gc_size_from,
  gc_size_to = gc_size_to,
  gc_size_interval = gc_size_interval,
  gc_alpha_from = gc_alpha_from,
  gc_alpha_slope = gc_alpha_slope,
  gc_color = gc_color
)


#==== plots ====
# spiral arms skeletons
plot_1_spiral_arms_skeleton <- ggplot(sprial_arms, aes(x = x, y = y)) +
  geom_point(color = "black", shape = 16) +
  coord_fixed() +
  theme_minimal() +
  theme(
    axis.text = element_blank()
  ) +
  labs(x = "", y = "")

# stars
plot_2_star_unit <- data.frame(
  x = seq_along(star_color),
  y = 0
) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(color = star_color, shape = 8, size = 40, stroke = 4) +
  theme_minimal() +
  theme(
    axis.text = element_blank()
  ) +
  labs(x = "", y = "")


# spiral arms
plot_3_spiral_arms <- ggplot(sprial_arms, aes(x = x, y = y)) +
  geom_point(data = stars, size = star_halo_size1, alpha = stars$alpha * star_alpha_adj1, color = "white", shape = 8) +
  geom_point(data = stars, size = star_halo_size2, alpha = stars$alpha * star_alpha_adj2, color = "white", shape = 8) +
  geom_point(data = stars, size = stars$size, alpha = stars$alpha, color = stars$color, shape = 8)  + 
  coord_fixed() +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    axis.text = element_blank(), 
    strip.text = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = background_color),
    plot.background = element_rect(fill = background_color),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")
  ) +
  labs(x = "", y = "")

# galactic center
plot_4_galactic_center_unit <- data.frame(
  x = seq_along(gc_color),
  y = 0
) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(color = gc_color, shape = 8, size = 40, stroke = 4) +
  theme_minimal() +
  theme(
    axis.text = element_blank()
  ) +
  labs(x = "", y = "")


#==== output ====
list(
  plot_name = c(
    "plot_1_spiral_arms_skeleton",
    "plot_2_star_unit",
    "plot_3_spiral_arms",
    "plot_4_galactic_center_unit"
  ),
  width = c(1600, 1600, 1600, 1600),
  height = c(1600, 400, 1600, 400),
  bg = c("white", "white", "#000011", "white")
) %>% 
  pmap(
    function(plot_name, width, height, bg){
      print(plot_name)
      
      save_jpg(
        get(plot_name),
        file_name = paste0(
          "output/milky_way/",
          plot_name,
          ".jpg"
        ),
        width = width, 
        height = height,
        print_plot = FALSE,
        bg = bg
      )
    }
  )
