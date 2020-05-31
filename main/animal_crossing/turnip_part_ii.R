library(tidyverse)
library(ggridges)
library(rio)
library(Rfast)


#===== general setup =====
setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_save_png.R")
source("./functions/functions_efficient_frontier.R")

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

turnip_return <- ((turnip_prices %>% select(-id, -base_price)) / turnip_prices$base_price - 1) %>%
  rename(
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
  as.matrix()


plot_efficient_frontier(
  risky_assets = tibble(
    return = colmeans(turnip_return),
    volatility = colVars(turnip_return)
  ), 
  rho = cor(turnip_return), 
  risk_free_asset = tibble(
    return = 0,
    volatility = 0,
  ),
  x_range = c(0, 0.6),
  y_range = c(0, 0.3),
  show_tangency_line = TRUE,
  show_risky_asset = TRUE,
  show_tangency_portfolio = TRUE,
  show_rf_portfolio = TRUE
)
  
  