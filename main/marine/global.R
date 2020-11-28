library(shiny)
library(leaflet)
library(tidyverse)
library(rio)
library(shiny.semantic)
library(plotly)

# the pre-processed data prepared in data_prep.R
ships <- import("input/ships.RData")

# the mapping between `ship_type` and `SHIP_ID`
# to be used to specify the linkage between the two dropdown menus
ship_types <- ships %>%
  select(ship_type, SHIP_ID, SHIPNAME) %>% 
  distinct() %>% 
  arrange(ship_type, SHIPNAME, SHIP_ID) %>% 
  mutate(
    # to be used for display purposes for the dropdown menu
    display_name = paste0(SHIPNAME, " - ", SHIP_ID)
  )

# the initial coordinates
# for setting the initial map view
initial_coordinates <- ships %>% 
  filter(
    SHIP_ID == ship_types %>% 
      filter(ship_type == "Tanker") %>% 
      slice(1) %>% 
      pull(SHIP_ID)
  )
