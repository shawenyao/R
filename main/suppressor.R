# Can brining in a new variable make a previously non-significant coefficient estimate siginificant?
# Yes!

suppressWarnings(library(tidyverse))

setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_save_svg.R")

set.seed(350)

n <- 100
x1 <- rnorm(n, 0, 0.01)
x2 <- runif(n, 0, 10)
alpha <- 3
beta1 <- 1
beta2 <- 1
epsilon <- rnorm(n, 0, 0.001)
y <- alpha + beta1 * x1 + beta2 * x2 + epsilon

# insiginificant coefficient estimate on variable epsilon_prime
lm1 <- lm(y~x1)
lm1 %>% summary()

# siginificant coefficient estimate on variable epsilon_prime
lm2 <- lm(y~x1 + x2)
lm2 %>% summary()

# plot
plot1 <- tibble(
  x = x1,
  y = y
) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 1, color = rainbow(n)) +
  labs(x = "x1", y = "y") +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))
  )


plot2 <- tibble(
  x = x1,
  y = y - x2,
) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 1, color = rainbow(n)) +
  labs(x = "x1", y = "y - x2") +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))
  )

save_svg(plot = plot1, file_name = "output/supressor/plot1.svg", width = 6, height = 3)
save_svg(plot = plot2, file_name = "output/supressor/plot2.svg", width = 6, height = 3)
