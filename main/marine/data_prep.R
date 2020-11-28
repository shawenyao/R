library(rio)
library(tidyverse)
library(geodist)


#==== general setup ====
root_dir <- "C:/Users/Wenyao/Desktop/R/R/main/marine"
setwd(root_dir)


#==== input ====
marine <- import("input/ships_04112020.zip", colClasses = c("SHIP_ID" = "character"))


#==== data preparation ====
# for each `SHIP_ID`, calculate the distance traveled between every 2 consecutive observations
marine_with_distance <- marine %>% 
  arrange(SHIP_ID, DATETIME) %>% 
  group_by(SHIP_ID) %>% 
  mutate(
    # calculate the haversine distance given two sets of coordinates
    # see https://en.wikipedia.org/wiki/Haversine_formula
    distance = geodist_vec(
      x1 = LON,
      y1 = LAT,
      x2 = lag(LON),
      y2 = lag(LAT),
      paired = TRUE,
      sequential = FALSE,
      pad = FALSE,
      measure = "haversine"
    ),
    # take care of cases where there is only 1 observation given a SHIP_ID
    distance = if_else(is.na(distance), 0, distance)
  )

# for each `SHIP_ID`, find the longest distance that it has traveled between 2 consecutive observations
marine_longest_distance <- marine_with_distance %>% 
  filter(distance == max(distance, na.rm = TRUE)) %>% 
  # if there's a tie, choose the most recent
  filter(DATETIME == max(DATETIME)) %>% 
  select(SHIP_ID, DATETIME) %>% 
  mutate(ind_longest = TRUE)

# put everything together
output <- marine_with_distance %>% 
  ungroup() %>% 
  # bring `ind_longest` to the original data.frame
  left_join(
    marine_longest_distance,
    by = c("SHIP_ID", "DATETIME")
  ) %>% 
  # standardize `SHIPNAME` given a `SHIP_ID`
  group_by(SHIP_ID) %>% 
  mutate(
    SHIPNAME = SHIPNAME[1]
  ) %>% 
  ungroup()


#==== output ====
export(output, "input/ships.RData")
