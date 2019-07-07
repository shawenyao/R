suppressWarnings(library(tidyverse))
suppressWarnings(library(beepr))
suppressWarnings(library(ggridges))

setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_save_svg.R")
source("./functions/functions_to_wait_or_not_to_wait.R")
source("./functions/functions_to_wait_or_not_to_wait_strategies.R")


#==== initialization ====
set.seed(10)

# the width/height of each block
block_width <- 1

# number of roads (either horizontal or vertical) 
road_dim <- 5

# the "importance" of each horizontal road
road_x <- tibble(
  name = seq_len(road_dim),
  importance = runif(road_dim, min = 0.1, max = 0.9)
)
# the "importance" of each vertical road
road_y <- tibble(
  name = seq_len(road_dim),
  importance = runif(road_dim, min = 0.1, max = 0.9)
)

# plot crossroads
# source("./main/to_wait_or_not_to_wait/plot_crossroads.R")

# crossroads topology
crossroad_x <- matrix(road_x$importance, ncol = road_dim, nrow = road_dim)
crossroad_y <- matrix(road_y$importance, ncol = road_dim, nrow = road_dim, byrow = TRUE)

# standardized "importance" at each intersection
# equivalent to the maximum wait time
crossroad_standard_x <- crossroad_x / (crossroad_x + crossroad_y)
crossroad_standard_y <- crossroad_y / (crossroad_x + crossroad_y)


#==== simulation starts ====
N <- 100000

# random realization of traffic signal
traffic_signal_list <- seq_len(N) %>% 
  map(
    function(i){
      matrix(runif(road_dim^2), ncol = road_dim, nrow = road_dim)
    }
  )

wait_time_strategy1 <- tibble(
  strategy = "Let the Light Guide Your Way",
  wait_time = traffic_signal_list %>% 
    map(
      simulate_strategy,
      crossroad_standard_x = crossroad_standard_x,
      crossroad_standard_y = crossroad_standard_y,
      strategy = strategy_let_the_light_guide_your_way
    ) %>% 
    unlist()
)

wait_time_strategy2.1 <- tibble(
  strategy = "Conditional Wait (0.1, 1.1)",
  wait_time = traffic_signal_list %>% 
    map(
      simulate_strategy,
      crossroad_standard_x = crossroad_standard_x,
      crossroad_standard_y = crossroad_standard_y,
      strategy = strategy_conditional_wait,
      arg1 = 0.1,
      arg2 = 1.1
    ) %>% 
    unlist()
)

wait_time_strategy2.2 <- tibble(
  strategy = "Conditional Wait (0.2, 1.2)",
  wait_time = traffic_signal_list %>% 
    map(
      simulate_strategy,
      crossroad_standard_x = crossroad_standard_x,
      crossroad_standard_y = crossroad_standard_y,
      strategy = strategy_conditional_wait,
      arg1 = 0.2,
      arg2 = 1.2
    ) %>% 
    unlist()
)

wait_time_strategy2.3 <- tibble(
  strategy = "Conditional Wait (0.3, 1.3)",
  wait_time = traffic_signal_list %>% 
    map(
      simulate_strategy,
      crossroad_standard_x = crossroad_standard_x,
      crossroad_standard_y = crossroad_standard_y,
      strategy = strategy_conditional_wait,
      arg1 = 0.3,
      arg2 = 1.3
    ) %>% 
    unlist()
)

wait_time_strategy3 <- tibble(
  strategy = "Ride Along Main Street",
  wait_time = traffic_signal_list %>% 
    map(
      simulate_strategy,
      crossroad_standard_x = crossroad_standard_x,
      crossroad_standard_y = crossroad_standard_y,
      strategy = strategy_ride_along_main_street
    ) %>% 
    unlist()
)

wait_time <- bind_rows(
  wait_time_strategy1,
  wait_time_strategy2.1,
  wait_time_strategy2.2,
  wait_time_strategy2.3,
  wait_time_strategy3
) %>% 
  mutate(
    strategy = factor(strategy, level = rev(unique(strategy)))
  )

plot3 <- plot_wait_time(wait_time)


#==== output ====
save_svg(plot = plot3, file_name = "output/to_cross_or_not_to_cross/plot3.svg", width = 6, height = 6)
