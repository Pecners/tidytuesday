library(tidyverse)
library(showtext)
library(ggtext)
library(gganimate)

# Helper function for long text

`%+%` <- function(x, y) paste0(x, y)

data <- tidytuesdayR::tt_load("2022-02-22")
freedom <- data$freedom
rm(data)

# Volatility

most_v <- freedom %>%
  group_by(country) %>%
  summarise(v_pr = var(PR),
         v_cl = var(CL)) %>%
  arrange(desc(v_pr)) %>%
  head(5)

f_only_v <- freedom %>%
  filter(country %in% most_v$country) %>%
  mutate(country = factor(country, levels = c(most_v$country)),
         PR = PR * -1)
  
# UN Branding based on this guide:
# https://www.un.org/styleguide/

font_add_google("Roboto", "r")
showtext_auto()

blue <- "#009edb"
dark_grey <- "#333333"
grey <- "#4D4D4D"
light_grey <- "#F2F2F2"

f_only_v %>%
  ggplot(aes(year, PR, group = country)) +
  geom_hline(yintercept = c(0, -8),
             linetype = 3, color = alpha(blue, .5)) +
  geom_path(color = blue) +
  geom_point(data = f_only_v %>% filter(year %in% c(1995,2020)),
             color = blue, size = 5) +
  geom_text(data = f_only_v %>% filter(year %in% c(1995, 2020)),
            aes(label = abs(PR)), color = "white", size = 3.5,
            family = "r")  +
  facet_wrap(~ country, ncol = 1) +
  scale_x_continuous(breaks = c(1995, 2020)) +
  theme(plot.title.position = "plot",
        plot.title = element_textbox_simple(family = "r", size = 28,
                                            color = grey, face = "bold",
                                            margin = margin(b = 5)),
        plot.subtitle = element_textbox_simple(family = "r", color = dark_grey,
                                               margin = margin(t = 5, b = 10),
                                               size = 13),
        #plot.caption.position = "plot",
        plot.caption = element_textbox_simple(family = "r", 
                                              color = alpha(grey, .5),
                                              margin = margin(t = 10, b = 5),
                                              halign = .5),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = light_grey, color = NA),
        strip.background = element_blank(),
        strip.text = element_text(color = grey, family = "r", size = 14),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_textbox_simple(color = grey, family = "r",
                                              orientation = "left-rotated",
                                              halign = .5),
        axis.title.x = element_textbox_simple(color = grey, halign = .5,
                                              family = "r"),
        axis.text.x = element_text(color = grey),
        axis.ticks.x = element_line(color = grey),
        panel.spacing = unit(1, "lines"),
        aspect.ratio = .1) +
  labs(title = "Swing States",
       subtitle = "Political rights in a country don't usually shift much year-to-year, " %+%
         "but these five countries have shown the greatest variance " %+%
         "between 1995 and 2020.",
       y = "**Political Rights Score**<br>" %+%
         "<span style='font-size:10pt;'>" %+%
         "Scores range from 1 (best) to 7 (worst)" %+%
         "</span>",
       x = "**Year**",
       caption = "Graphic by Spencer Schien (@MrPecners) | " %+%
         "Data from Freedom House by way of Arthur Cheib")

ggsave(filename = "2022/2022-02-22/final_plot.png", device = "png",
       bg = light_grey, width = unit(8, "in"), height = unit(7.5, "in"))
