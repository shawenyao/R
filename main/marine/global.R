library(shiny)
library(leaflet)
library(tidyverse)
library(rio)
library(shiny.semantic)
library(plotly)

ships <- import("input/ships.RData")

# the mapping between `ship_type` and `SHIP_ID`
ship_types <- ships %>%
  select(ship_type, SHIP_ID, SHIPNAME) %>% 
  distinct() %>% 
  arrange(ship_type, SHIPNAME, SHIP_ID) %>% 
  mutate(
    display_name = paste0(SHIPNAME, " - ", SHIP_ID)
  )

# the initial coordinates (for setting the initial map view)
initial_coordinates <- ships %>% 
  filter(
    ship_type == ship_types$ship_type[1], 
    SHIP_ID == ship_types$SHIP_ID[1]
  )
