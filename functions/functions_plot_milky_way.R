#' get the coordinates of the spiral arms
#'
#' @param num_of_arms
#' @param theta_from
#' @param theta_to
#' @param theta_length
#' @param theta_power
#' @param arm_sd_x
#' @param arm_sd_y
#' @param arm_width width of the spiral arms
#' @param arm_alpha_from
#' @param arm_alpha_to
#' 
#' @return a data.frame of coordinates
#' 
get_spiral_arms <- function(
  num_of_arms,
  theta_from,
  theta_to,
  theta_length,
  theta_power,
  arm_sd_x,
  arm_sd_y,
  arm_width,
  arm_alpha_from,
  arm_alpha_to
){
  
  list(
    id = seq_len(num_of_arms),
    theta_from = theta_from,
    theta_to = theta_to,
    arm_width = arm_width
  ) %>% 
    pmap(function(id, theta_from, theta_to, arm_width){
      tibble(
        id = id,
        theta = seq(from = theta_from, to = theta_to, length.out = theta_length)
      ) %>% 
        mutate(
          r = theta ^ theta_power,
          x = r * cos(theta + 2 * pi * (id + rnorm(1, sd = arm_sd_x)) / num_of_arms),
          y = r * sin(theta + 2 * pi * (id + rnorm(1, sd = arm_sd_y)) / num_of_arms),
          width = arm_width,
          alpha = seq(arm_alpha_from, arm_alpha_to, length.out = n())
        )
    }) %>% 
    bind_rows()
}


#' get the coordinates of the stars
#'
#' @param sprial_arms
#' @param star_intensity
#' @param star_size_from
#' @param star_size_to
#' @param star_size_interval
#' @param star_color
#' 
#' @return a data.frame of coordinates
#' 
get_stars <- function(
  sprial_arms,
  star_intensity,
  star_size_from,
  star_size_to,
  star_size_interval,
  star_color
){
  
  sprial_arms %>% 
    slice(rep(row_number(), star_intensity)) %>% 
    mutate(
      x = x + rnorm(n(), sd = width),
      y = y + rnorm(n(), sd = width),
      size = seq(from = star_size_from, to = star_size_to, by = star_size_interval) %>% 
        sample(
          size = n(), 
          prob = 1 / seq(from = star_size_from, to = star_size_to, by = star_size_interval), 
          replace = TRUE
        ),
      color = star_color %>%
        sample(
          size = n(), 
          prob = c(rep(0.95 / (length(.) - 1), length(.) - 1), 0.05), 
          replace = TRUE
        )
    )
}


#' get the coordinates of the galactic center
#'
#' @param gc_rho
#' @param gc_intensity
#' @param gc_sd_x
#' @param gc_sd_y
#' @param gc_size_from
#' @param gc_size_to
#' @param gc_size_interval
#' @param gc_alpha_from
#' @param gc_alpha_slope
#' @param gc_color
#' 
#' @return a data.frame of coordinates
#' 
get_galactic_center <- function(
  gc_rho,
  gc_intensity,
  gc_sd_x,
  gc_sd_y,
  gc_size_from,
  gc_size_to,
  gc_size_interval,
  gc_alpha_from,
  gc_alpha_slope,
  gc_color
){
  
  tibble(
    x = rnorm(gc_intensity, sd = gc_sd_x)
  ) %>% 
    mutate(
      y = gc_rho * x + sqrt(1 - gc_rho ^ 2) * rnorm(gc_intensity, sd = gc_sd_y),
      r = sqrt(x ^ 2 + y ^ 2),
      size = seq(from = gc_size_from, to = gc_size_to, by = gc_size_interval) %>% 
        sample(
          size = n(), 
          prob = 1 / seq(from = gc_size_from, to = gc_size_to, by = gc_size_interval), 
          replace = TRUE
        ),
      alpha = gc_alpha_from + (1 - r / max(r)) * gc_alpha_slope,
      color = gc_color %>% 
        sample(
          size = n(),
          replace = TRUE
        )
    )
}


#' plot milky way
#' 
#' @param num_of_arms
#' @param theta_from
#' @param theta_to
#' @param theta_length
#' @param theta_power
#' @param arm_sd_x
#' @param arm_sd_y
#' @param arm_width width of the spiral arms
#' @param arm_alpha_from
#' @param arm_alpha_to
#' 
#' @param star_intensity
#' @param star_size_from
#' @param star_size_to
#' @param star_size_interval
#' @param star_color
#' 
#' @param gc_rho
#' @param gc_intensity
#' @param gc_sd_x
#' @param gc_sd_y
#' @param gc_size_from
#' @param gc_size_to
#' @param gc_size_interval
#' @param gc_alpha_from
#' @param gc_alpha_slope
#' @param gc_color
#' 
#' @param star_halo_size1
#' @param star_halo_size2
#' @param star_alpha_adj1
#' @param star_alpha_adj2
#' 
#' @param gc_halo_size1
#' @param gc_halo_size2
#' @param gc_halo_alpha1
#' @param gc_halo_alpha2
#' 
#' @param x_axis_range
#' @param y_axis_range
#' @param background_color
#' @param seed
#' 
#' @return a ggplot object of milky way
#' 
plot_milky_way <- function(
  num_of_arms,
  theta_from,
  theta_to,
  theta_length,
  theta_power,
  arm_sd_x,
  arm_sd_y,
  arm_width,
  arm_alpha_from,
  arm_alpha_to,
  
  star_intensity,
  star_size_from,
  star_size_to,
  star_size_interval,
  star_color,
  
  gc_rho,
  gc_intensity,
  gc_sd_x,
  gc_sd_y,
  gc_size_from,
  gc_size_to,
  gc_size_interval,
  gc_alpha_from,
  gc_alpha_slope,
  gc_color,
  
  star_halo_size1,
  star_halo_size2,
  star_alpha_adj1,
  star_alpha_adj2,
  
  gc_halo_size1,
  gc_halo_size2,
  gc_halo_alpha1,
  gc_halo_alpha2,
  
  x_axis_range = NULL,
  y_axis_range = NULL,
  background_color,
  seed = NULL
){
  
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  sprial_arms <- get_spiral_arms(
    num_of_arms = num_of_arms,
    theta_from = theta_from,
    theta_to = theta_to,
    theta_length = theta_length,
    theta_power = theta_power,
    arm_sd_x = arm_sd_x,
    arm_sd_y = arm_sd_y,
    arm_width = arm_width,
    arm_alpha_from = arm_alpha_from,
    arm_alpha_to = arm_alpha_to
  )
  
  stars <- get_stars(
    sprial_arms = sprial_arms,
    star_intensity = star_intensity,
    star_size_from = star_size_from,
    star_size_to = star_size_to,
    star_size_interval = star_size_interval,
    star_color = star_color
  )
  
  gc <- get_galactic_center(
    gc_rho = gc_rho,
    gc_intensity = gc_intensity,
    gc_sd_x = gc_sd_x,
    gc_sd_y = gc_sd_y,
    gc_size_from = gc_size_from,
    gc_size_to = gc_size_to,
    gc_size_interval = gc_size_interval,
    gc_alpha_from = gc_alpha_from,
    gc_alpha_slope = gc_alpha_slope,
    gc_color = gc_color
  )
  
  
  output <- ggplot(sprial_arms, aes(x = x, y = y)) +
    
    # spiral arms
    geom_point(data = stars, size = star_halo_size1, alpha = stars$alpha * star_alpha_adj1, color = "white", shape = 8) +
    geom_point(data = stars, size = star_halo_size2, alpha = stars$alpha * star_alpha_adj2, color = "white", shape = 8) +
    geom_point(data = stars, size = stars$size, alpha = stars$alpha, color = stars$color, shape = 8) +
    
    # galactic center
    geom_point(data = gc, size = gc_halo_size1, alpha = gc_halo_alpha1, color = "gold", shape = 8) +
    geom_point(data = gc, size = gc_halo_size2, alpha = gc_halo_alpha2, color = "gold", shape = 8) +
    geom_point(data = gc, size = gc$size, alpha = gc$alpha, color = gc$color, shape = 8) + 
    
    # cosmetics
    coord_fixed() +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.line = element_blank(),
      axis.text = element_blank(), 
      strip.text = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = background_color),
      plot.background = element_rect(fill = background_color),
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")
    ) +
    labs(x = "", y = "")
  
  if(!is.null(x_axis_range)){
    output <- output +
      xlim(x_axis_range[1], x_axis_range[2]) +
      ylim(y_axis_range[1], y_axis_range[2])
  }
  
  output
}
