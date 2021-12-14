library(tidyverse)
library(corrplot)
library(corrr)
library(RColorBrewer)
library(showtext)

# Helper function for caption
`%+%` <- function(x, y) paste0(x, y)

# Get fonts
font_add_google("Rock Salt", "rs")
showtext_auto()

# Load data
data <- tidytuesdayR::tt_load("2021-12-14")

# Select numeric variables
c <- data$studio_album_tracks %>%
  select(danceability,
         energy,
         speechiness,
         acousticness,
         instrumentalness,
         liveness,
         valence,
         tempo)

# Calculate correlation, put in df
df_c <- c %>%
  correlate()

# Transform for plotting, then plot
df_c %>%
  pivot_longer(-term, names_to = "group", values_to = "corr") %>%
  filter(term == "danceability" & !is.na(corr)) %>%
  mutate(group = str_to_title(group),
         budge = case_when(corr > 0 ~ -.075,
                           corr < 0 ~ .075, 
                           TRUE ~ 0),
         align = case_when(corr > 0 ~ 1,
                           corr < 0 ~ 0,
                           TRUE ~ 0)) %>%
  ggplot(aes(reorder(group, corr), corr)) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_segment(aes(x = reorder(group, corr), xend = reorder(group, corr),
                   y = 0, yend = corr),
               linetype = 4, size = 1.5, color = "grey30") +
  geom_point(size = 12, aes(fill = corr), shape = 21) +
  geom_text(aes(label = round(corr, digits = 2),
                color = ifelse(abs(corr) < .2, "black", "white")),
            size = 2, family = "rs") +
  geom_text(aes(label = group, 
                y = budge, hjust = align), family = "rs", color = "#340E51") +
  scale_fill_gradient2(low = "#D12424", mid = "white", high = "#f28bd5") +
  scale_color_manual(values = c("black", "white")) +
  scale_y_continuous(breaks = c(-.45, .55), labels = c("Less Danceable", "More Danceable")) +
  theme_minimal() + 
  theme(legend.position = "none",
        text = element_text(family = "rs", color = "#340E51"),
        plot.caption = element_text(hjust = 0, size = 8, lineheight = 1.25),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        axis.line.x = element_line(arrow = arrow(length = unit(.3, "cm"), ends = "both"),
                                   color = "grey30")) +
  labs(x = "", y = "",
       title = "What makes a Spice Girls song danceable?",
       subtitle = "Sugar, spice, and a whole lot of valence",
       caption =  "Valence is a measure of musical positiveness.\n" %+%
         "Numbers represent correlation with Spotify's danceability metric.\n" %+%
         "Graph by Spencer Schien (@MrPecners) | Data from Spotify") +
  coord_flip()

ggsave(filename = "2021/2021-12-14/final_plot.jpeg", device = "jpeg")
