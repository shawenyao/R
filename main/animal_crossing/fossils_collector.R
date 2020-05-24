library(tidyverse)
library(ggridges)
library(parallel)
library(beepr)


#===== general setup =====
setwd("C:/Users/Wenyao/Desktop/R/R/")
source("./functions/functions_save_png.R")

set.seed(350)
# number of simulations
n <- 1e5
# max number of trial per simulation
max_trial <- 1300
# number of unique fossils
unique_number <- 73

# expectation
sum(unique_number / (unique_number:1))

one_trial <- function(trial_id){
  count = tibble(
    id = 1:max_trial,
    obs = sample(x = unique_number, size = max_trial, replace = TRUE)
  ) %>% 
    mutate(
      unique = !duplicated(obs),
      unique_count = cumsum(unique)
    ) %>% 
    filter(
      unique_count == unique_number
    ) %>% 
    head(1) %>% 
    pull(id)
}


cl <- makeCluster(spec = detectCores() - 1)
clusterEvalQ(cl, library(dplyr))
clusterExport(cl, c("one_trial", "max_trial", "unique_number"))
results <- parLapply(
  cl, 
  seq_len(n), 
  one_trial
) %>% 
  unlist()

stopCluster(cl)

summary(results)

#===== plot ======
plot <- tibble(
  number_of_trials = results,
  id = ""
) %>% 
  ggplot(aes(x = number_of_trials, y = id, fill = factor(..quantile..))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE, 
    quantiles = c(0.25, 0.5, 0.75),
    scale = 2000
  ) +
  scale_fill_manual(
    name = "Probability Density",
    values = c("#E3F6FB", "#D6EEEF", "#A4CDCF", "#6CB4CC")
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 25),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
    axis.title.y = element_blank(),
    axis.text.y = element_text(angle = 0, hjust = 0.5, margin = margin(r = -20))
  ) +
  scale_x_continuous(limits = c(0, NA),breaks = seq(from = 0, to = 1100, by = 100)) +
  labs(x = "Number of Trials")


#==== save ====
save_png(
  plot,
  file_name = "output/animal_crossing/fossils_collector.png",
  width = 800,
  height = 250
)

# play sound when finished
beep(sound = 2)
