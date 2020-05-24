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
# number of trials per day
trials_per_day <- 4


#==== calculations ====
# the expected number of trials it takes to get a certain number of fossils
simulation1_expectation <- sapply(1:unique_number, function(i){sum(unique_number / (unique_number:(unique_number + 1 - i)))})

# the expected number of unique fossils after certain number of trials
simulation2_expectation <- 1
for(i in 2:356){
  simulation2_expectation <- c(simulation2_expectation, 1 + (1 - 1 / unique_number) * simulation2_expectation[i - 1])
}

# the number of trials it takes to get all 73 fossils
simulation1 <- function(trial_id){
  tibble(
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

# the number of fossils after 356 trials
simulation2 <- function(trial_id){
  tibble(
    id = 1:356,
    obs = sample(x = unique_number, size = 356, replace = TRUE)
  ) %>% 
    mutate(
      unique = !duplicated(obs),
      unique_count = cumsum(unique)
    ) %>% 
    tail(1) %>% 
    pull(unique_count)
}


cl <- makeCluster(spec = detectCores() - 1)
clusterEvalQ(cl, library(dplyr))
clusterExport(cl, c("simulation1", "simulation2", "max_trial", "unique_number"))
simulation1_results <- parLapply(
  cl, 
  seq_len(n), 
  simulation1
) %>% 
  unlist()
simulation2_results <- parLapply(
  cl, 
  seq_len(n), 
  simulation2
) %>% 
  unlist()

stopCluster(cl)

summary(simulation1_results)
summary(simulation2_results)


#===== plot ======
# distribution of number of days it takes to get all 73 fossils
plot1 <- tibble(
  number_of_trials = simulation1_results / trials_per_day,
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
  scale_x_continuous(limits = c(15, NA), breaks = seq(from = 0, to = 1100 / trials_per_day, by = 25)) +
  labs(x = "# of Days") +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 25),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
    axis.title.y = element_blank(),
    axis.text.y = element_text(angle = 0, hjust = 0.5, margin = margin(r = -20))
  )
  

# distribution of number of unique fossils after 356 trials
plot2 <- tibble(
  number_of_fossils = simulation2_results,
  id = ""
) %>% 
  group_by(number_of_fossils) %>% 
  summarise(frequency = n() / n) %>% 
  mutate(
    label = round(frequency * 100, digits = 2) %>% paste0("%")
  ) %>% 
  ggplot(aes(x = as.factor(number_of_fossils), y = frequency)) +
  geom_bar(aes(fill = number_of_fossils, color = number_of_fossils), stat = "identity") +
  geom_label(aes(y = frequency + 0.075, label = label), size = 6, color = "dodgerblue") +
  scale_fill_gradient(low = "#E3F6FB", high = "#6CB4CC") +
  scale_color_gradient(low = "#E3F6FB", high = "#6CB4CC") +
  ylim(NA, 0.7) +
  labs(x = "# of Unique Fossils", y = "Probability") +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 25),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
    axis.text.y = element_text(angle = 0, hjust = 0.5, margin = margin(r = -20))
  )

# expected number of days as a function of unique fossils
plot3 <- tibble(
  x = seq_len(unique_number),
  y = simulation1_expectation / trials_per_day
) %>% 
  mutate(
    label = case_when(
      x %in% c(seq(0, unique_number, by = 10), unique_number) ~ as.character(round(y, 2)),
      TRUE ~ NA_character_
    )
  ) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_bar(aes(fill = x), stat = "identity", width = 0.8) +
  geom_label(aes(y = y + 7.5, label = label), size = 6, color = "dodgerblue") +
  scale_fill_gradient(low = "#E3F6FB", high = "#6CB4CC") +
  scale_x_continuous(breaks = c(seq(from = 0, to = unique_number, by = 10), unique_number)) +
  ylim(NA, max(simulation1_expectation / trials_per_day) + 10) +
  labs(x = "# of Unique Fossils", y = "E(# of Days)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 20),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
    axis.text.y = element_text(angle = 0, hjust = 0.5, margin = margin(r = -20))
  )

# expected number of unique fossils as a function of number of days
plot4 <- tibble(
  x = 1:(356 / trials_per_day),
  y = simulation2_expectation[c(FALSE, FALSE, FALSE, TRUE)]
) %>% 
  mutate(
    label = case_when(
      x %in% c(seq(0, 356 / trials_per_day, by = 10), 356 / trials_per_day) ~ as.character(round(y, 2)),
      TRUE ~ NA_character_
    )
  ) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_bar(aes(fill = x), stat = "identity", width = 0.7) +
  geom_label(aes(y = y + 7.5, label = label), size = 6, color = "dodgerblue") +
  scale_fill_gradient(low = "#E3F6FB", high = "#6CB4CC") +
  scale_x_continuous(breaks = c(seq(from = 0, to = 356 / trials_per_day, by = 10), 356 / trials_per_day)) +
  ylim(NA, max(simulation2_expectation) + 10) +
  labs(x = "# of Days", y = "E(# of Unique Fossils)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 20),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
    axis.text.y = element_text(angle = 0, hjust = 0.5, margin = margin(r = -20))
  )


#==== save ====
save_png(
  plot1,
  file_name = "output/animal_crossing/fossils_collector_1.png",
  width = 800,
  height = 250
)
save_png(
  plot2,
  file_name = "output/animal_crossing/fossils_collector_2.png",
  width = 800,
  height = 250
)
save_png(
  plot3,
  file_name = "output/animal_crossing/fossils_collector_3.png",
  width = 800,
  height = 250
)
save_png(
  plot4,
  file_name = "output/animal_crossing/fossils_collector_4.png",
  width = 800,
  height = 250
)

# play sound when finished
beep(sound = 2)
