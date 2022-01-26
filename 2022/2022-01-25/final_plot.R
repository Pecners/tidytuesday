library(tidyverse)
library(showtext)
library(emojifont)
library(ggtext)
library(scales)
library(ggbeeswarm)

# Helper function
`%+%` <- function(x, y) paste0(x, y)

# Font to be used in plot
font_add_google("Cinzel Decorative", "cd")

# Load Data
data <- tidytuesdayR::tt_load("2022-01-25")
ratings <- data$ratings
details <- data$details
rm(data)

# Vector of game themes or series to filter by

games <- c(
  "Pandemic",
  "Ticket to Ride",
  "Catan",
  "Stratego",
  "Star Wars",
  "Harry Potter",
  "Game of Thrones",
  "Marvel"
  )

# Create df of only those games (they can overlap)

g_ranks <- map_df(games, function(x) {
  ratings %>%
    filter(str_detect(name, x)) %>%
    transmute(id = id,
              name = name,
              rank = rank,
              br = bayes_average,
              group = x)
})

# General calculations for plot labeling

avg <- mean(ratings$bayes_average)
high <- max(ratings$bayes_average)

# Color scheme, matching palette from Board Game Geeks

g_purple <- "#3F3A5F"
g_orange <- "#FF5000"
g_blue <- "#6ACADF"
  
# I ultimately decided to keep Marvel and Stratego out
# because the plot was cluttered, plut Marvel wasn't an
# entirely clean filter.

gr <- g_ranks %>% 
  group_by(group) %>%
  mutate(o = max(br))

gr_labs <- gr %>%
  filter(br == max(br)) %>%
  arrange(desc(br)) %>%
  filter(!group %in% c("Marvel", "Stratego"))
  
gr %>%
  filter(!group %in% c("Marvel", "Stratego")) %>%
  ggplot(aes(reorder(group, -o), br)) +
  geom_hline(yintercept = c(avg, high), linetype = 3, color = "white") +
  geom_text(data = gr_labs, aes(label = str_wrap(name, 20), y = br + .1),
            color = g_blue, family = "cd", size = 3.5, lineheight = .85, vjust = 0) +
  annotate(geom = "text", x = 6, y = high - .25, 
           label = paste0("Highest Rating\nin Database: ", comma(high, 0.01)), 
           color = "white", lineheight = .85,
           family = "cd") +
  geom_curve(x = 5.55, xend = 5.3, y = high - .25, yend = high - .05,
             arrow = arrow(length = unit(.2, "cm"), type = "closed"), color = "white",
             size = .01, curvature = -.33) +
  annotate(geom = "text", x = 3.1, y = avg - .25, 
           label = paste0("Avg. Rating: ", comma(avg, 0.01)), 
           color = "white",
           family = "cd") +
  geom_curve(x = 3.55, xend = 3.75, y = avg - .25, yend = avg - .05,
             arrow = arrow(length = unit(.2, "cm"), type = "closed"), color = "white",
             size = .01) +
  geom_violin(fill = "#FF5000", color = "#FF5000", alpha = .5, width = 1.25) +
  geom_quasirandom(color = "#6ACADF", fill = "#6ACADF", 
                   width = .2, alpha = .5, 
                   varwidth = TRUE, shape = 22, size = 2) +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  scale_y_continuous(limits = c(5.15, 8.75)) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#3F3A5F"),
        panel.grid = element_blank(),
        text = element_text(color = "white", size = 24, family = "cd"),
        axis.text.x = element_text(color = "white", size = 14, face = "bold"),
        axis.text.y = element_blank(),
        axis.line.y = element_line(color = "white", 
                                   arrow = arrow(length = unit(3, "mm"))),
        plot.title = element_text(color = g_orange, 
                                  face = "bold", hjust = .5, size = 30),
        plot.subtitle = element_textbox_simple(hjust = .5, size = 16, halign = .5,
                                               margin = margin(b = 10)),
        plot.caption.position = "plot",
        plot.caption = element_textbox_simple(size = 10, hjust = 0)) +
  labs(x = "",
       y = "Game Rating",
       title = "One Game to Rule Them All",
       subtitle = "<span style='color:" %+% g_blue %+% "'>" %+%
       "**Pandemic**</span> and <span style='color:" %+% g_blue %+% "'>" %+%
       "**Ticket to Ride**</span> games are mostly rated " %+%
         "above average, while <span style='color:" %+% g_blue %+% "'>" %+%
       "**Star Wars**</span> and <span style='color:" %+% g_blue %+% "'>" %+%
       "**Harry Potter**</span> games are mostly below average.",
       caption = "Top rated game  in each category shown with <span style='color:" %+% 
         g_blue %+%
       "'>text label</span>. Categories represent common game themes or series.<br>" %+%
         "Graph by Spencer Schien (@MrPecners) | " %+%
         "Data from Kaggle by way of Board Game Geeks")

ggsave(filename = "2022/2022-01-25/final_plot.png", device = "png", bg = g_purple)
