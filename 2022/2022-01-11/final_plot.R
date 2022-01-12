library(tidyverse)
library(showtext)
library(maps)
library(sf)
library(emojifont)
library(ggtext)

data <- tidytuesdayR::tt_load("2022-01-11")
colony <- data$colony
stressor <- data$stressor %>%
  mutate(stress_pct = replace_na(stress_pct, 0))

states <- map("state", plot = FALSE, fill = TRUE) %>%
  st_as_sf()

`%+%` <- function(x, y) paste0(x,y)

font_add_google("Indie Flower", "if")

showtext_auto()

# Correlations

s_wide <- stressor %>%
  pivot_wider(names_from = stressor, values_from = stress_pct)

for_cor <- colony %>%
  filter(year != "6/") %>%
  select(year:state, colony_lost_pct) %>%
  left_join(., s_wide) %>%
  select(-c(1:3))

d_corr <- for_cor %>%
  corrr::correlate()

d_plot <- d_corr %>%
  pivot_longer(cols = -term, names_to = "group", values_to = "corr") %>%
  filter(term == "colony_lost_pct" & !is.na(corr))

bee_yellow <- "#f2d638"
bee_black <- "#010600"

d_plot %>%
  mutate(group = ifelse(group == "Other", "Other*", group),
         corr = corr * -10,
         size = corr * -1,
         nudge_y = corr + .09 + .05 * (1 - (max(size) - size) / (max(size) - min(size)))) %>%
  ggplot(aes(reorder(group, -corr), corr, label = emoji("bee"))) +
  geom_segment(y = -0.01, yend = -.01, x = .5, xend = 6.51,
               size = 2, color = bee_black, arrow = arrow(length = unit(.33, "cm"), type = "closed")) +
  geom_segment(y = -0.01, yend = -.01, x = .5, xend = 6.5,
               size = 1.5, color = "#BA8C63", arrow = arrow(length = unit(.3, "cm"), type = "closed")) +
  geom_spring(aes(xend = reorder(group, -corr),
                   y = 0, yend = corr, tension = corr / -1.5),
              diameter = .15, color = bee_black) +
  geom_point(aes(y = corr, size = size),shape = 21, fill = bee_yellow, color = bee_black) +
  geom_text(family = "EmojiOne", aes(size = size * .2, y = nudge_y), color = bee_black) +
  geom_text(inherit.aes = FALSE, aes(x = reorder(group, corr), 
                                     y = corr, 
                                     label = str_wrap(group, 15)),
            nudge_y = -.3, lineheight = .9, family = "if", size = 6) +
  geom_textbox(inherit.aes = FALSE, aes(x = 2, y = -3), family = "if", width = unit(.45, "npc"),
               fill = "#BA8C63", color = "black",
               label = "<span style='font-size:24pt'>Bringing Down the Hive</span><br><br>" %+%
                 "<span style='font-size:16pt'>" %+%
                 "While varroa mites are by far the most common stressor of honey bee colonies in the US, " %+%
                 "other* stressors show the strongest correlation with colony loss." %+%
                 "</span>") +
  scale_y_continuous(limits = c(-4, .1), ) +
  scale_size_continuous(range = c(10, 20)) +
  scale_x_discrete(position = "top") +
  theme_minimal() +
  theme(text = element_text(family = "if", size = 14),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E4CCA8"),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0),
        axis.text = element_blank(),
        axis.title.x.top = element_text(margin = margin(t = 0, b = -15),
                                        size = 18)) +
  labs(caption = "*Other stressors (per USDA) include weather, starvation, insufficient forage, " %+%
    "queen failure, hive damage/destroyed, etc.\n" %+%
    "Graph by Spencer Schien (@MrPecners) | Data from USDA",
       x = "Stressor correlation with colony loss",
       y = "")

ggsave(filename = "2022/2022-01-11/final_plot.png", device = "png")

