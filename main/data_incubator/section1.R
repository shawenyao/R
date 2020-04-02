library(tidyverse)
library(rio)

setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1/data")

#==== load data ====
football <- import("football_2019_2020.csv")


#==== plot ====
plot1 <- football %>% 
  ggplot(aes(x = home_team_score, y = away_team_score)) +
  geom_jitter(size = 0.8, color = rainbow(nrow(football)), width = 0.25, height = 0.25) +
  geom_smooth(method='lm', formula= y~x) +
  labs(x = "Home Team Score", y = "Away Team Score") + 
  theme_minimal() +
  theme(
    text = element_text(size = 15),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(hjust = 0.5, margin = margin(r = 20))
  )

plot2 <- football %>% 
  filter(
    !league %in% c("europa-league", "champions-league")
  ) %>% 
  group_by(league) %>% 
  summarise(
    home_team_score = mean(home_team_score),
    away_team_score = mean(away_team_score)
  ) %>% 
  gather(
    type, score, -league
  ) %>% 
  mutate(
    league = factor(league, levels = c("premier-league", "spanish-la-liga", "italian-serie-a", "german-bundesliga", "french-ligue-one" ), labels = c("English\nPremier League", "Spanish\nLa Liga", "Italian\nSeries A", "German\nBundesliga", "French\nLigue One"))
  ) %>% 
  group_by(league) %>% 
  mutate(
    type = factor(type, levels = c("home_team_score", "away_team_score"), labels = c("Home Team Score", "Away Team Score")),
    label_position = score,
    label = round(score, 2)
  ) %>% 
  ggplot(aes(x = league)) +
  geom_bar(aes(fill = type, weight = score), position = "dodge") +
  geom_text(aes(y = label_position + 0.1, label  = label, group = type), position = position_dodge(width = 1), size = 4) +
  scale_fill_manual(values = c("dodgerblue1", "firebrick2")) + 
  theme_minimal() +
  theme(
    text = element_text(size = 15),
    axis.text.x = element_text(vjust = 0.9),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )


#==== output =====
setwd("C:/Users/Wenyao/Desktop/R/R/output/data_incubator")
svg("output/plot1.svg", width = 8, height = 4)
print(plot1)
dev.off()

svg("output/plot2.svg", width = 8, height = 4)
print(plot2)
dev.off()
