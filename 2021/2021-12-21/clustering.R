library(tidyverse)
library(cluster)

data <- tidytuesdayR::tt_load("2021-12-21")
d <- data$starbucks

d_grams <- d %>%
  filter(size == "grande") %>%
  select(total_fat_g:caffeine_mg) %>%
  mutate_all(as.numeric)

scaled_grams <- scale(d_grams)

dist_grams <- dist(scaled_grams)

tot_withinss <- map_dbl(1:10, function(k) {
  model = kmeans(x = dist_grams, centers = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

elbow_df %>%
  ggplot(aes(k, tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)

sil_width <- map_dbl(2:10, function(k){
  model <- pam(x = dist_grams, k = k)
  model$silinfo$avg.width
})

sil_df <- data.frame(
  k = 2:10,
  sil_width = sil_width
)

sil_df %>%
  ggplot(aes(k, sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)

k_clust <- kmeans(dist_grams, centers = 3)

drinks_clustered <- d %>%
  filter(size == "grande") %>%
  mutate(cluster = as_factor(k_clust$cluster))

t <- drinks_clustered %>%
  modify_at(c("fiber_g", "calories"), as.numeric) %>%
  group_by(cluster) %>%
  summarise_if(is.numeric, mean)
