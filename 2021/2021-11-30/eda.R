library(tidyverse)

data <- tidytuesdayR::tt_load("2021-11-30")

d <- data$matches %>%
  mutate(p = ifelse(winner == toss, 1, 0))

summary(glm(p ~ toss, data = d, family = ))

d %>%
  ggplot(aes(lubridate::mdy(match_date), score_team1 + score_team2)) +
  geom_point() +
  geom_smooth()
