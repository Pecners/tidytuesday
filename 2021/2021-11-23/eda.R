library(tidyverse)

`%+%` <- function(x, y) paste0(x, y)


tuesdata <- tidytuesdayR::tt_load('2021-11-23')
writers <- tuesdata$writers
directors <- tuesdata$directors
episodes <- tuesdata$episodes
imdb <- tuesdata$imdb

# Do episodes with more writers have higher ratings? ====

writers %>%
  group_by(story_number) %>%
  tally() %>%
  right_join(., episodes) %>%
  ggplot(aes(n, rating)) +
  geom_jitter(width = .1)

## No

# How do imdb ratings compare with British ratings ====

episodes %>%
  mutate(new_id = paste(season_number, episode_number, sep = "_")) %>%
  left_join(., imdb %>%
              mutate(new_id = paste(season, ep_num, sep = "_")) %>%
              rename("imdb_rating" = rating),
            by = "new_id") %>%
  ggplot(aes(imdb_rating, rating)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, 100))

## UK ratings are within a much tighter and higher band

# Which season had the most viewers? ====

episodes %>%
  group_by(season_number) %>%
  summarise(total_uk_viewers = mean(uk_viewers, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(UK = (total_uk_viewers - mean(total_uk_viewers)) / sd(total_uk_viewers)) %>%
  left_join(., imdb %>%
              group_by(season) %>%
              summarise(total_ratings = mean(rating_n)) %>%
              ungroup() %>%
              mutate(IMDb = (total_ratings - mean(total_ratings)) / sd(total_ratings)),
            by = c("season_number" = "season")) %>%
  select(season_number,
         UK,
         IMDb) %>%
  pivot_longer(cols = -1, names_to = "group", values_to = "rating") %>%
  mutate(season_number = as.numeric(season_number)) %>%
  filter(season_number != "13") %>%
  ggplot(aes(reorder(season_number, -season_number), rating, fill = group)) +
  geom_col(position = position_dodge(width = .5)) +
  scale_y_continuous(limits = c(-2, 2), breaks = c(-2, 0, 2),
                     labels = c("Lower", "Average", "Higher")) +
  scale_fill_manual(values = c("#FFA62B", "#010078")) +
  theme_minimal(base_family = "serif") +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "bottom",
        axis.line.x = element_line(arrow = arrow(ends = "both", length = unit(.25, "cm"))),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot") +
  coord_flip() +
  labs(x = "Season", y = "Viewership",
       fill = "",
       title = "Dr. Who Viewership",
       caption = str_wrap("Viewership calculated as season average then converted to z-score. " %+%
                            "IMDb viewership based on rating counts.", 150))

