library(rio)
library(tidyverse)

ships <- import("input/ships.RData")

# the mapping between `ship_type` and `SHIPNAME`
ship_types <- ships %>%
  select(ship_type, SHIPNAME) %>% 
  distinct() %>% 
  arrange(ship_type, SHIPNAME)

initial_coordinates <- ships %>% 
  filter(
    ship_type == ship_types$ship_type[1], 
    SHIPNAME == ship_types$SHIPNAME[1]
  )

# ship_types %>%
#   group_by(ship_type) %>%
#   summarise(
#     duplicates = sum(duplicated(SHIPNAME))
#   )
