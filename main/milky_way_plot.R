library(tidyverse)
library(beepr)

setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_save_png.R")


set.seed(1)

total <- 4
sprial_arms <- list(
  id = seq_len(total),
  from = c(2.7, 3.3, 2.7, 3.3),
  # to = c(10, 9, 8, 9),
  to = c(7.7, 8.5, 7.5, 8),
  width = c(1.1, 0.8, 1.1, 0.8)
) %>% 
  pmap(function(id, from, to, width, alpha){
    tibble(
      id = id,
      theta = seq(from = from, to = to, length.out = 1000)
    ) %>% 
      mutate(
        r = 1 / theta ^ (-1.5),
        x = r * cos(theta + 2 * pi * (id + rnorm(1, sd = 0.01)) / total),
        y = r * sin(theta + 2 * pi * (id + rnorm(1, sd = 0.01)) / total),
        width = width,
        alpha = 0.15 + (1 - row_number() / n()) * 0.2
      )
  }) %>% 
  bind_rows()

stars <- sprial_arms %>% 
  slice(rep(row_number(), 2)) %>% 
  mutate(
    x = x + rnorm(n(), sd = width),
    y = y + rnorm(n(), sd = width),
    size = seq(from = 0.5, to = 6, by = 0.5) %>% 
      sample(
        size = n(), 
        prob = 1 / seq(from = 0.5, to = 6, by = 0.5) / (sum(1 / seq(from = 0.5, to = 6, by = 0.5))), 
        replace = TRUE
      ),
    color = c("#64C5E8", "#81D3DE", "#C5DED8", "#E3E9D2", "#F2FFCF", "firebrick1") %>%
      sample(
        size = n(), 
        prob = c(rep(0.95 / (length(.) - 1), length(.) - 1), 0.05), 
        replace = TRUE
      )
  )

rho <- 0.9
galactic_center <- tibble(
  x = rnorm(1000, sd = 2.25)
) %>% 
  mutate(
    y = rho * x + sqrt(1 - rho ^ 2) * rnorm(1000, sd = 2.25),
    r = sqrt(x ^ 2 + y ^ 2),
    size = seq(from = 0.5, to = 6, by = 0.5) %>% 
      sample(
        size = n(), 
        prob = 1 / seq(from = 0.5, to = 6, by = 0.5) / (sum(1 / seq(from = 0.5, to = 6, by = 0.5))), 
        replace = TRUE
      ),
    alpha = 0.01 + (1 - r / max(r)) * 0.2,
    color = c("lemonchiffon", "lemonchiffon1", "khaki1", "khaki2", "gold") %>% 
      sample(
        size = n(),
        replace = TRUE
      )
  )


milky_way_plot <- ggplot(sprial_arms, aes(x = x, y = y)) +
  # geom_path(size = 10, lineend = "round", alpha = 0.05, color = "white") +
  # spiral arms
  geom_point(data = stars, size = 40, shape = 8, alpha = stars$alpha / 18, color = "white") +
  geom_point(data = stars, size = 60, shape = 8, alpha = stars$alpha / 36, color = "white") +
  geom_point(data = stars, size = stars$size, shape = 8, alpha = stars$alpha, color = stars$color) +
  coord_fixed() +
  theme_minimal() +
  # galactic center
  geom_point(data = galactic_center, size = 15, shape = 8, alpha = 0.01, color = "gold") +
  geom_point(data = galactic_center, size = 30, shape = 8, alpha = 0.005, color = "gold") +
  geom_point(data = galactic_center, size = galactic_center$size, shape = 8, alpha = galactic_center$alpha, color = galactic_center$color) + 
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    axis.text = element_blank(), 
    strip.text = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#000010"),
    plot.background = element_rect(fill = "#000010"),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")
  ) +
  labs(x = "", y = "")

save_png(
  milky_way_plot,
  file_name = "output/milky_way/milky_way.png",
  width = 880, 
  height = 880,
  print_plot = FALSE,
  bg = "#000010"
)

# play sound when finished
beep(sound = 2)
