library(rio)
library(tidyverse)
library(testthat)


#==== general setup ====
root_dir <- "C:/Users/Wenyao/Desktop/R/R/main/marine"
setwd(root_dir)


#==== input ====
# the pre-processed data prepared in data_prep.R
ships <- import("input/ships.RData")


#==== test ====
# one and only one longest observation is allowed per `SHIP_ID`
expect_uniqueness <- function(df) {
  
  longest_count <- df %>% 
    group_by(SHIP_ID) %>% 
    summarise(longest_count = sum(ind_longest, na.rm = TRUE)) %>% 
    pull(longest_count)
    
  expect(
    all(longest_count == 1),
    failure_message = paste0(
      "Failed: one and only one longest observation is allowed per `SHIP_ID`"
    )
  )
}

test_that(
  desc = "Uniqueness test", {
    expect_uniqueness(ships)
})
