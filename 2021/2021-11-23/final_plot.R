library(tidyverse)

# Operator for writing caption

`%+%` <- function(x, y) paste0(x, y)


tuesdata <- tidytuesdayR::tt_load('2021-11-23')
episodes <- tuesdata$episodes
imdb <- tuesdata$imdb


# Calculate average viewership per season
# and convert to z-score

episodes_z <- episodes %>%
  group_by(season_number) %>%
  summarise(avg_viewership = mean(uk_viewers, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(UK = (avg_viewership - mean(avg_viewership)) / sd(avg_viewership))

imdb_z <- imdb %>%
  group_by(season) %>%
  summarise(total_ratings = mean(rating_n)) %>%
  ungroup() %>%
  mutate(IMDb = (total_ratings - mean(total_ratings)) / sd(total_ratings))

together <- left_join(episodes_z, imdb_z, by = c("season_number" = "season")) %>%
  select(season_number,
         UK,
         IMDb) %>%
  pivot_longer(cols = -1, names_to = "group", values_to = "rating") %>%
  mutate(season_number = as.numeric(season_number)) %>%
  
  # Filter out season 13 since imdb only goes through 12
  
  filter(season_number != "13")


together %>%
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
       title = "Viewership of Doctor Who revival on the decline",
       caption = "Viewership calculated as season average then converted to z-score. " %+%
         "IMDb viewership based on rating counts.\n" %+%
         "Graph by Spencer Schien (@pecners89) | Data from {datardis} R package and IMDb.\n")

