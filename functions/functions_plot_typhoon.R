#' plot typhoon graph
#'
#' @param iteration number of iterations
#' @param total total number of paths per iteration
#' 
plot_typhoon <- function(iteration, total){
  
  data <- seq_len(iteration) %>% 
    map(function(i){
      seq_len(total) %>% 
        map(function(j){
          tibble(
            id = j + (i - 1) * total,
            theta = seq(from = rnorm(1, mean = 5, sd = 1), to = rnorm(1, mean = 20, sd = 2), length.out = 1000)
          ) %>% 
            mutate(
              r = 1 / theta,
              x = r * cos(theta + 2 * pi * (j + rnorm(1, sd = 0.2)) / total) + rnorm(1, sd = 0.005),
              y = r * sin(theta + 2 * pi * (j + rnorm(1, sd = 0.2)) / total) + rnorm(1, sd = 0.005)
            )
        }) %>% 
        bind_rows()
    }) %>% 
    bind_rows()
  
  typhoon_plot <- ggplot(data, aes(x = x, y = y, group = id)) +
    geom_path(size = 0.4, lineend = "round", alpha = 0.2) +
    coord_fixed() +
    theme_minimal() +
    theme(
      legend.position = "none", 
      axis.line = element_blank(),
      axis.text = element_blank(), 
      strip.text = element_blank(),
      panel.grid = element_blank()
    ) +
    labs(x = "", y = "")
}
