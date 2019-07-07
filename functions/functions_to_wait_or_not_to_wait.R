#' plot wait time distribution
#' 
#' @param wait_time a data.frame of wait_time observation for each strategy
#' 
plot_wait_time <- function(wait_time){
  
  wait_time %>% 
  ggplot(aes(x = wait_time, y = strategy, fill = factor(..quantile..))) +
    stat_density_ridges(
      geom = "density_ridges_gradient",
      calc_ecdf = TRUE, 
      quantiles = c(0.25, 0.5, 0.75)
    ) +
    scale_fill_manual(
      name = "Probability",
      values = c("#E3F6FB", "#D6EEEF", "#A4CDCF", "#6CB4CC")
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      text = element_text(size = 15),
      axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
      axis.title.y = element_blank(),
      axis.text.y = element_text(angle = 35, hjust = 0.5, margin = margin(r = -20))
    ) +
    labs(x = "Wait Time") +
    xlim(0, 4)
}


#' calculate the total wait time for the strategy
#' "let the light guide your way":
#' 
#' @description 
#' you follow wherever the green light tells you to go
#' unless you’ve already reached the boundary
#' in which case you wait for the signals to turn green (if you have to)
#' 
#' @param traffic_signal the traffic signal at each crossroad
#' @param crossroad_standard_x the standardized "importance" at each crossroad for going right
#' @param crossroad_standard_y the standardized "importance" at each crossroad for going down
#' @strategy a function of decision strategy
#' @arg1 additional argument placeholder 1
#' @arg2 additional argument placeholder 2
#' 
simulate_strategy <- function(
  traffic_signal,
  crossroad_standard_x,
  crossroad_standard_y,
  strategy,
  arg1 = NULL,
  arg2 = NULL
){
  
  # indicate a green light for going right
  green_x <- (crossroad_standard_x > traffic_signal) * 1
  # indicate a green light for going down
  green_y <- (crossroad_standard_x < traffic_signal) * 1
  
  # how long it takes until the next green light for going right
  wait_time_x <- green_y * (traffic_signal - crossroad_standard_x)
  # how long it takes until the next green light for going down
  wait_time_y <- green_x * (1 - traffic_signal)
  
  # apply a decision rule
  strategy_outcome <- strategy(
    green_x, 
    green_y, 
    wait_time_x, 
    wait_time_y,
    crossroad_standard_x,
    crossroad_standard_y,
    arg1,
    arg2
  )
  
  # total wait time
  calculate_wait_time(
    strategy_outcome$wait_x, 
    strategy_outcome$wait_y, 
    wait_time_x, 
    wait_time_y
  )
}


#' calculate the total wait time
#' 
#' @param wait_x a matrix indicating whether you need to wait to go right at each intersection
#' @param wait_y a matrix indicating whether you need to wait to go down at each intersection
#' @param wait_time_x a matrix of wait time to go right at each intersection
#' @param wait_time_y a matrix of wait time to go down at each intersection
#' 
calculate_wait_time <- function(
  wait_x,
  wait_y,
  wait_time_x,
  wait_time_y
){
  sum(wait_x * wait_time_x + wait_y * wait_time_y)
}
