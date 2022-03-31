library(tidyverse)
library(glue)
library(showtext)
library(ggtext)
library(scales)

# Load data

data <- tidytuesdayR::tt_load("2022-03-29")
sports <- data$sports
rm(data)

# Calculate proportion of spending on each sport
# per school for 2019-20

avg <- sports %>%
  filter(year == 2019) %>%
  group_by(unitid) %>%
  mutate(perc = total_exp_menwomen / sum(total_exp_menwomen, na.rm = TRUE)) %>%
  ungroup() %>%
  select(sports, perc) %>%
  group_by(sports) %>%
  mutate(avg_perc = mean(perc, na.rm = TRUE)) %>%
  filter(!is.na(perc))
  
# Set theme elements
  
g_purple <- "#3F3A5F"
g_orange <- "#FF5000"
g_blue <- "#6ACADF"
bg <- g_purple

font_add_google("Cinzel Decorative", "cd")
showtext_auto()

avg %>%
  ggplot(aes(reorder(sports, avg_perc), perc)) +
  geom_point(alpha = .075, shape = 22, size = 4, fill = g_orange, color = g_orange) +
  geom_point(data = avg %>%
               select(-perc) %>%
               unique(),
             aes(y = avg_perc), fill = g_blue, color = g_blue, shape = 23,
             alpha = .75, size = 2) +
  geom_text(data = avg %>%
              select(-perc) %>%
              unique(),
            aes(y = avg_perc + .015, label = percent(avg_perc, 0.1)),
            color = g_blue, hjust = 0, size = 3.5, family = "cd",
            vjust = .35) +
  scale_y_continuous(labels = percent, breaks = c(0, .5, 1)) +
  coord_flip() +
  theme_minimal() +
  theme(plot.background = element_rect(fill = bg),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "white", linetype = 3),
        axis.text= element_text(color = "white", family = "cd"),
        plot.title.position = "plot",
        plot.title = element_textbox_simple(family = "cd", color = g_orange, size = 24,
                                  face = "bold", width = unit(8.5, "in")),
        plot.subtitle = element_textbox_simple(width = unit(8.5, "in"), family = "cd",
                                               size = 13, color = "white",
                                               margin = margin(b = 10)),
        plot.caption.position = "plot",
        plot.caption = element_textbox_simple(width = unit(8.5, "in"), family = "cd",
                                              size = 8, color = alpha("white", .5),
                                              margin = margin(b = 5))) +
  labs(y = "", x = "",
       title = "Follow the money",
       subtitle = glue("On <span style='color:{g_blue}'>**average**</span>, ",
                       "colleges allocate the largest proportion of their sports budgets ",
                       "for football, but basketball most frequently comprises 100% of a budget."),
       caption = glue("Data represents the 2019-20 academic year. Analysis and graphic by Spencer Schien (@MrPecners), ",
                      "data from Equity in Athletics."))



ggsave(filename = "2022/2022-03-29/final_plot.png", device = "png", bg = bg,
       w = 9, h = 9)

       