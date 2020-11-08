library(tidyverse)
library(rio)

setwd("C:/Users/Wenyao/Desktop/R/R/")

btc_on_ethereum_daily_history <- import(
  "input/defi/btc_on_ethereum_daily_history.csv"
)

bvol <- import(
  "input/defi/bvol.csv"
)

to_plot <- btc_on_ethereum_daily_history %>% 
  filter(
    day >= as.Date("2020-06-01")
  ) %>% 
  select(-blockNumber) %>% 
  inner_join(
    bvol %>% select(-symbol),
    by = c("day" = "timestamp")
  )

ggplot(to_plot, aes(x = price, y = btc_total_amount)) +
  geom_point(size = 1, color = "dodgerblue") +
  geom_smooth(method = lm) +
  theme_minimal() +
  labs(x = "BTC Monthly Volatility %", y = "BTC on Ethereum") +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20)
  )

coeff <- max(to_plot$btc_total_amount) / max(to_plot$price)
to_plot %>% 
  ggplot(aes(x = day)) +
  geom_line(aes(y = price), size = 1, color = "tomato") + 
  geom_line(aes(y = btc_total_amount / coeff), size = 1, color = "dodgerblue") +
  labs(x = "") +
  scale_y_continuous(
    name = "BTC Monthly Volatility %",
    sec.axis = sec_axis(~.*coeff, name = "BTC on Ethereum")
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "tomato", size = 20),
    axis.title.y.right = element_text(color = "dodgerblue", size = 20)
  )

