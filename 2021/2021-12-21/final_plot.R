library(tidyverse)
library(showtext)
library(ggtext)
library(emojifont)

# Helper function for caption
`%+%` <- function(x, y) paste0(x, y)

# Get fonts
font_add_google("Quattrocento Sans", "qs")
showtext_auto()

data <- tidytuesdayR::tt_load("2021-12-21")
d <- data$starbucks

# Maximize caffeine, minimize calories
# Have whip

x <- d %>%
  filter(whip == 1) %>%
  select(product_name,
         size,
         milk,
         whip,
         calories,
         caffeine_mg) %>%
  # normalize scales
  modify_at(c("calories",
              "caffeine_mg"), function(x) {
                (x - min(x)) / (max(x) - min(x))
              }) %>%
  arrange(desc(caffeine_mg)) %>%
  group_by(product_name) %>%
  # limit to one product name
  filter(caffeine_mg == max(caffeine_mg)) %>%
  filter(calories == min(calories)) %>%
  ungroup() %>%
  # remove drinks without caffeine
  filter(caffeine_mg > 0) %>%
  group_by(product_name,
           size,
           whip,
           calories,
           caffeine_mg) %>%
  tally() %>%
  ungroup()

# Add the whip without adding to the hip

t <- x %>%
  mutate(product_name = str_remove(product_name, " Blended$")) %>%
  select(product_name, calories, caffeine_mg) %>%
  pivot_longer(cols = -product_name, names_to = "group", values_to = "value") %>%
  mutate(value = ifelse(group == "calories", value * -1, value)) %>%
  ungroup() %>%
  group_by(product_name) %>%
  mutate(label = max(value) - ((max(value) - min(value)) / 2))
  
t %>% 
  filter(group == "caffeine_mg") %>%
  ggplot(aes(reorder(product_name, value), value)) +
  geom_segment(aes(xend = reorder(product_name, value), y = 0, yend = value),
               color = "#006241") +
  geom_segment(data = t %>%
                 filter(group == "calories"),
               aes(xend = reorder(product_name, value), y = 0, yend = value),
               color = "#006241") +
  geom_point(shape = 21, fill = "#9d5116", size = 3, color = "#006241") +
  geom_point(data = t %>% 
               filter(group == "calories"),
             shape = 21, fill = "#e44c2c",
             size = 3, color = "#006241") +
  geom_text(aes(label = str_wrap(product_name, 30), y = value + .075), 
            hjust = 0, lineheight = .75, size = 3.75, family = "qs",
            color = "#016241") +
  scale_y_continuous(limits = c(-1, 1.5), breaks = c(-.9, 1.4),
                     labels = c("<span style='color:#e44c2c;'>**More Calories**</span>",
                                "<span style='color:#9d5116'>**More Caffeine**</span>")) +
  geom_hline(yintercept = 0, linetype = 3, color = "#006241", alpha = .75) +
  geom_emoji("coffee", y = .2, x = 7.5, color = "#9d5116", alpha = .15, size = 150) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(color = "#006241", family = "qs"),
        axis.text.x = element_markdown(size = 12, margin = margin(t = 5)),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.subtitle = element_textbox_simple(size = 14, lineheight = 1, halign = .5,
                                               margin = margin(t = 5, b = 10)),
        plot.title = element_text(face = "bold", hjust = .5, size = 24, family = "qs"),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, size = 10),
        axis.line.x = element_line(arrow = arrow(ends = "both", length = unit(.33, "cm")),
                                   color = "#006241"),
        plot.background = element_rect(fill = "#D4E8E1")) +
  labs(x = "", y = "",
       title = "HAVE YOUR COFFEE AND WHIP IT TOO",
       subtitle = "To get the most <span style='color:#9d5116'>**caffeine**</span> " %+%
         "and the fewest <span style='color:#e44c2c;'>**calories**</span> " %+%
         "(while keeping whip),<br>" %+%
         "Starbucks' *Espresso con panna* is the obvious choice",
       caption = str_wrap("Data limited to products that contain caffeine and have whip. " %+%
         "One size selected per product based on maximum caffeine and minimum calories. " %+%
         "Graph by Spencer Schien (@MrPecners) | Data from Starbucks", 100))

ggsave(filename = "2021/2021-12-21/final_plot.jpeg", device = "jpeg")

