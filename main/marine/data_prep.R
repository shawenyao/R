library(rio)
library(tidyverse)
library(geodist)


#==== general setup ====
root_dir <- "C:/Users/Wenyao/Desktop/R/R/main/marine"
setwd(root_dir)


#==== input ====
ships <- import("input/ships_04112020.zip", colClasses = c("SHIP_ID" = "character"))


#==== data preparation ====
# for each `SHIP_ID`, pre-calculate the distance traveled between every 2 consecutive observations
# (so that it doesn't have to be done at runtime)
ships_with_distance <- ships %>% 
  # notice that there are quite a few duplicates in the original data
  distinct() %>% 
  # order by unique identifier for efficiency
  arrange(SHIP_ID, DATETIME) %>% 
  # grouping by `SHIP_ID` for pairing two consecutive observations
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
ships_longest_distance <- ships_with_distance %>% 
  filter(distance == max(distance, na.rm = TRUE)) %>% 
  # if there's a tie, choose the most recent
  filter(DATETIME == max(DATETIME)) %>% 
  select(SHIP_ID, DATETIME, distance) %>% 
  mutate(ind_longest = TRUE)

# put everything together
output <- ships_with_distance %>% 
  ungroup() %>% 
  # bring `ind_longest` into the original data.frame
  left_join(
    ships_longest_distance,
    by = c("SHIP_ID", "DATETIME", "distance")
  ) %>% 
  # standardize `SHIPNAME` given a `SHIP_ID`
  # (otherwise for a given `SHIP_ID`, the `SHIPNAME` won't be consistent)
  group_by(SHIP_ID) %>% 
  mutate(
    SHIPNAME = SHIPNAME[1]
  ) %>% 
  ungroup()


#==== output ====
export(output, "input/ships.RData")
