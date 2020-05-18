suppressWarnings(library(tidyverse))
suppressWarnings(library(beepr))

setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_save_jpg.R")

set.seed(350)

star_intensity <-  5
star_size_from <-  1
star_size_to <-  20
star_size_interval <-  0.5
star_color = c("#64C5E8", "#81D3DE", "#C5DED8", "#E3E9D2", "#F2FFCF", "firebrick1")
width <- 0.075

star_halo_size1 <-  80
star_halo_size2 <-  160
star_alpha_adj1 <-  1 / 18
star_alpha_adj2 <-  1 / 36

arm_alpha_from <-  0.9
arm_alpha_to <-  0.01

for(i in 1:100){
  
  print(paste0("Creating galaxy ", i, "!"))
  
  f1 <- jitter(sample(c(2, 3), 1))
  f2 <- jitter(sample(c(2, 3), 1))
  f3 <- jitter(sample(c(2, 3), 1))
  f4 <- jitter(sample(c(2, 3), 1))
  d1 <- runif(1, 0, 1e-2)
  d2 <- runif(1, 0, 1e-2)
  d3 <- runif(1, 0, 1e-2)
  d4 <- runif(1, 0, 1e-2)
  p1 <- runif(1, 0, pi)
  p2 <- runif(1, 0, pi)
  p3 <- runif(1, 0, pi)
  p4 <- runif(1, 0, pi)
  xt <- function(t){
    exp(-d1 * t) * sin(t * f1 + p1) + exp(-d2 * t) * sin(t * f2 + p2)
  }
  yt <- function(t){
    exp(-d3 * t) * sin(t * f3 + p3) + exp(-d4 * t) * sin(t * f4 + p4)
  }
  t <- seq(1, 100, by = 0.08)
  
  sprial_arms <- tibble(t = t) %>% 
    mutate(
      x = xt(t), 
      y = yt(t),
      alpha = seq(
        from = arm_alpha_from, 
        to = arm_alpha_to, 
        length.out = n()
      )
    )
  
  stars <- sprial_arms %>% 
    slice(rep(row_number(), star_intensity)) %>% 
    mutate(
      x = x + rnorm(n(), sd = width),
      y = y + rnorm(n(), sd = width),
      size = seq(from = star_size_from, to = star_size_to, by = star_size_interval) %>% 
        sample(
          size = n(), 
          prob = 1 / seq(from = star_size_from, to = star_size_to, by = star_size_interval), 
          replace = TRUE
        ),
      color = star_color %>%
        sample(
          size = n(), 
          prob = c(rep(0.95 / (length(.) - 1), length(.) - 1), 0.05), 
          replace = TRUE
        )
    )
  
  plot <- ggplot(sprial_arms, aes(x = x, y = y)) +
    geom_point(size = 0.1) +
    geom_point(data = stars, size = star_halo_size1, alpha = stars$alpha * star_alpha_adj1, color = "white", shape = 8) +
    geom_point(data = stars, size = star_halo_size2, alpha = stars$alpha * star_alpha_adj2, color = "white", shape = 8) +
    geom_point(data = stars, size = stars$size, alpha = stars$alpha, color = stars$color, shape = 8) +
    coord_fixed() +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.line = element_blank(),
      axis.text = element_blank(), 
      strip.text = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "#000011"),
      plot.background = element_rect(fill = "#000011"),
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")
    ) +
    labs(x = "", y = "")
  
  #===== save =====
  save_jpg(
    plot,
    file_name = paste0("output/milky_way/harmonograph/", i, ".jpg"),
    width = 880 * 2, 
    height = 880 * 2,
    print_plot = FALSE,
    bg = "#000011"
  )
}

# play sound when finished
beep(sound = 2)
