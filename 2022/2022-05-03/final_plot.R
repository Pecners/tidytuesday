library(tidyverse)
library(MetBrewer)
library(glue)
library(ggtext)
library(showtext)

capacity <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/capacity.csv')
wind <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/wind.csv')
solar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/solar.csv')
average_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/average_cost.csv')

c <- met.brewer("Hiroshige")
font_add_google("Allan", "a")
font_add_google("Bangers", "b")
showtext_auto()

solar %>%
  ggplot(aes(date, y = solar_mwh)) +
  geom_step(color = c[2]) +
  geom_step(data = wind, aes(y = wind_mwh), color = c[9]) +
  scale_y_continuous(labels = rev(c("$300/MWh", 200, 100, 0))) +
  theme_minimal() +
  theme(text = element_text(family = "a"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(family = "b", size = 30),
        plot.subtitle = element_textbox(family = "a", size = 20,
                                        width = unit(7, "in")),
        plot.caption = element_text(color = "grey60")) +
  labs(title = "Skyline Decline",
       subtitle = glue("Towering costs of <span style='color:{c[2]}'>**solar**</span> and ",
                       "<span style='color:{c[9]}'>**wind**</span> power are a thing of the past, ",
                       "replaced by a much lower price horizon in recent years."),
       caption = "Graphic by Spencer Schien (@MrPecners) | Data from Berkeley Lab",
       y = "", x = "")

ggsave(filename = "2022/2022-05-03/final_plot.png", bg = "grey90",
       w = 9, h = 5)
