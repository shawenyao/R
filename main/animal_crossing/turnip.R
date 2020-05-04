library(tidyverse)
library(ggridges)
library(rio)


#===== general setup =====
setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_save_png.R")

dates <- c(
  "Sunday",
  "Monday a.m.",
  "Monday p.m.",
  "Tuesday a.m.",
  "Tuesday p.m.",
  "Wednesday a.m.",
  "Wednesday p.m.",
  "Thursday a.m.",
  "Thursday p.m.",
  "Friday a.m.",
  "Friday p.m.",
  "Saturday a.m.",
  "Saturday p.m."
)


#===== load data =====
turnip_prices <- import("input/animal_crossing/turnip_prices.csv")


#===== plot =====
# plot 1: the distribution of turnip price by day x a.m./p.m.
turnip <- turnip_prices %>% 
  rename(
    `Sunday` = base_price,
    `Monday a.m.` = sell_price1,
    `Monday p.m.` = sell_price2,
    `Tuesday a.m.` = sell_price3,
    `Tuesday p.m.` = sell_price4,
    `Wednesday a.m.` = sell_price5,
    `Wednesday p.m.` = sell_price6,
    `Thursday a.m.` = sell_price7,
    `Thursday p.m.` = sell_price8,
    `Friday a.m.` = sell_price9,
    `Friday p.m.` = sell_price10,
    `Saturday a.m.` = sell_price11,
    `Saturday p.m.` = sell_price12,
  ) %>% 
  gather(time, price, -id) %>% 
  mutate(
    time = factor(time, levels = dates)
  ) %>% 
  arrange(id, time) %>% 
  group_by(id) %>% 
  mutate(
    base_price = price[1]
  ) %>% 
  ungroup() %>% 
  mutate(
    return = price / base_price - 1
  ) %>% 
  ungroup()

plot1 <- turnip %>% 
  mutate(
    time = factor(time, levels = rev(dates))
  ) %>% 
  ggplot(aes(x = price, y = time, fill = factor(..quantile..))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE, 
    quantiles = c(0.25, 0.5, 0.75)
  ) +
  scale_fill_manual(
    name = "Probability Density",
    values = c("#E3F6FB", "#D6EEEF", "#A4CDCF", "#6CB4CC")
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 20),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
    axis.title.y = element_blank(),
    axis.text.y = element_text(angle = 0, hjust = 0.5, margin = margin(r = -20))
  ) +
  labs(x = "Turnip Price") +
  xlim(0, 200)


# plot 2: the distribution of turnip return on Wednesday a.m.
turnip %>% 
  filter(time != "Sunday") %>% 
  group_by(time) %>% 
  summarise(
    expected_return = mean(return),
    volatility = sd(return),
    sharpe_ratio = expected_return / volatility
  ) %>% 
  mutate_if(is.numeric, round, 4) %>%
  write.table("clipboard-128", row.names = FALSE, sep = " | ")

plot2 <- turnip %>% 
  filter(time == "Wednesday a.m.") %>%
  ggplot(aes(x = return, y = time, fill = factor(..quantile..))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE, 
    quantiles = c(0.25, 0.5, 0.75)
  ) +
  scale_fill_manual(
    name = "Probability Density",
    values = c("#E3F6FB", "#D6EEEF", "#A4CDCF", "#6CB4CC")
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 20),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
    axis.title.y = element_blank(),
    axis.text.y = element_text(angle = 0, hjust = 0.5, margin = margin(r = -20))
  ) +
  labs(x = "Return") +
  xlim(-1, 1)

# plot 3: the distribution of turnip return on Wednesday a.m., allowing for multiple islands
  turnip_returns_1_islands <- turnip %>% 
  filter(time %in% c("Sunday", "Wednesday a.m.")) %>% 
  mutate(island = 1, label = "1 Island")

turnip_returns_multiple_islands <- turnip_returns_1_islands

for(i in 2:6){
  turnip_returns_multiple_islands <- turnip_returns_multiple_islands %>% 
    bind_rows(
      turnip_returns_1_islands %>% 
        mutate(id = floor((id - 1) / i)) %>%
        group_by(id, time) %>% 
        summarise(
          base_price = base_price[1],
          price = max(price)
        ) %>% 
        ungroup() %>% 
        mutate(
          return = price / base_price - 1,
          island = i,
          label = paste0(i, " Islands")
        )
    )
}

turnip_returns_multiple_islands <- turnip_returns_multiple_islands %>% 
  filter(time == "Wednesday a.m.")

plot3 <- turnip_returns_multiple_islands %>% 
  filter(time == "Wednesday a.m.") %>%
  ggplot(aes(x = return, y = factor(label, levels = c("6 Islands", "5 Islands", "4 Islands", "3 Islands", "2 Islands", "1 Island")), fill = factor(..quantile..))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE, 
    quantiles = c(0.25, 0.5, 0.75)
  ) +
  scale_fill_manual(
    name = "Probability Density",
    values = c("#E3F6FB", "#D6EEEF", "#A4CDCF", "#6CB4CC")
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 20),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
    axis.title.y = element_blank(),
    axis.text.y = element_text(angle = 0, hjust = 0.5, margin = margin(r = -20))
  ) +
  labs(x = "Return") +
  xlim(-1, 1)

turnip_returns_multiple_islands %>% 
  group_by(island) %>% 
  summarise(
    expected_return = mean(return),
    volatility = sd(return),
    sharpe_ratio = expected_return / volatility
  ) %>% 
  mutate_if(is.numeric, round, 4) %>%
  write.table("clipboard-128", row.names = FALSE, sep = " | ")


#===== save =====
save_png(
  plot1,
  file_name = "output/animal_crossing/turnip_price.png",
  width = 800,
  height = 800
)

save_png(
  plot2,
  file_name = "output/animal_crossing/turnip_return.png",
  width = 800,
  height = 250
)

save_png(
  plot3,
  file_name = "output/animal_crossing/turnip_return_multiple_islands.png",
  width = 800,
  height = 600
)
