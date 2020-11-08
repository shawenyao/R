library(rio)
library(tidyverse)

setwd("C:/Users/Wenyao/Desktop/R/R/")

btc_on_ethereum_history <- list(
  coin = c(
    "wbtc", "renbtc", "hbtc", "sbtc", "imbtc", "tbtc", "pbtc"
  ),
  decimal = c(
    8, 8, 18, 18, 8, 18, 18
  )
) %>% 
  pmap(
    function(coin, decimal){
      import(paste0("input/defi/", coin, "-history.json")) %>% 
        mutate(
          coin = coin,
          timestamp = as.POSIXct(timestamp / 1e3, origin="1970-01-01"),
          amount = as.numeric(amount) / (10^decimal),
          total_amount = cumsum(amount)
        )
    }
  ) %>% 
  bind_rows() %>% 
  arrange(timestamp) %>% 
  mutate(
    btc_total_amount = cumsum(amount)
  )

btc_on_ethereum_daily_history <- btc_on_ethereum_history %>% 
  mutate(
    day = as.Date(timestamp)
  ) %>% 
  select(-timestamp, -coin, -amount, -total_amount) %>% 
  group_by(day) %>% 
  filter(row_number() == n())

export(
  btc_on_ethereum_daily_history,
  "input/defi/btc_on_ethereum_daily_history.csv"
)
