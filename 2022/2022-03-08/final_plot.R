library(tidyverse)
library(showtext)
library(ggtext)
library(ggstar)
library(scales)

# Load data

data <- tidytuesdayR::tt_load("2022-03-08")
erasmus <- data$erasmus
rm(data)

# Thanks to @BjnNowak for sharing this table
# which gives full country names

iso <- read_delim("https://raw.githubusercontent.com/BjnNowak/TidyTuesday/main/data/iso.csv",delim=';')

# Get start and end participation counts for countries

start_end <- erasmus %>%
  ungroup() %>%
  filter(academic_year %in% c("2014-2015", "2019-2020")) %>%
  select(academic_year,
         country = sending_country_code,
         participants) %>%
  mutate(group = "sending") %>%
  bind_rows(., erasmus %>%
              filter(academic_year %in% c("2014-2015", "2019-2020")) %>%
              select(academic_year,
                     country = receiving_country_code,
                     participants) %>%
              mutate(group = "receiving"))

# Budge will adjust values to allow space in middle for labels

budge <- 1500

# Calculate totals per country

se_final <- start_end %>%
  group_by(country, group, academic_year) %>%
  summarise(total = sum(participants)) %>%
  ungroup() %>%
  pivot_wider(names_from = academic_year, values_from = total,
              values_fill = 0) %>%
  mutate_at(3:4, function(x) x = ifelse(.$group == "receiving", (x * -1) - budge, x + budge)) %>%
  mutate(all_total = abs(`2014-2015` + `2019-2020`),
         perc_change = (`2019-2020` - `2014-2015` / `2014-2015`),
         diff = `2019-2020` - `2014-2015`) %>%
  left_join(., iso, by = c("country" = "code")) %>%
  filter(!is.na(country_name))

# Find top 25 countries 

tops <- se_final %>%
  group_by(country) %>%
  summarise(total = sum(abs(`2019-2020`))) %>%
  arrange(desc(total)) %>%
  head(25)

# Helper table for adding ad hoc legends

leg <- tibble(
  country_name = "Cyprus",
  `2014-2015` = c(-6000, 6000),
  `2019-2020` = c(-8000, 8000),
  group = c("receiving", "sending"),
  all_total = 0
)

# Plot helpers, theme definitions

`%+%` <- function(x, y) paste0(x,y)

blue <- "#003399"
yellow <- "#FFCC00"

font_add_google("Sonsie One", "so")
font_add_google("Kotta One", "ko")
showtext_auto()

se_final %>%
  filter(country %in% tops$country) %>%
  ggplot(aes(x = reorder(country_name, all_total))) +
  geom_hline(yintercept = c(budge, -budge), linetype = 3, color = yellow) +
  geom_segment(aes(y = `2014-2015`, yend = `2019-2020`,
                   xend = reorder(country_name, all_total)),
               color = yellow, lineend = "butt",
               size = .5, arrow = arrow(length = unit(.2, "cm"), 
                                        type = "closed")) +
  #geom_point(aes(y = `2019-2020`), color = "blue", shape = 23, fill = "blue") +
  geom_point(aes(y = `2014-2015`), color = blue, size = 4) +
  geom_star(aes(y = `2014-2015`), color = yellow, fill = yellow, size = 2) +
  geom_label(aes(label = country_name, y = 0), fill = yellow, color = blue,
             family = "so", size = 3) +
  geom_text(aes(y = `2019-2020` + 100 * sign(`2019-2020`), 
                label = comma(abs(`2019-2020`), 1),
                hjust = ifelse(sign(`2019-2020`) == -1, 1, 0)),
            color = yellow, family = "so", size = 2) +
  # legend elements
  geom_rect(xmin = 2.5, xmax = 4, ymin = -5250, ymax = -9000,
            fill = yellow, color = blue) +
  geom_rect(xmin = 2.5, xmax = 4, ymin = 5250, ymax = 9000,
            fill = yellow, color = blue) +
  geom_segment(data = leg, aes(y = `2014-2015`, yend = `2019-2020`,
                               xend = reorder(country_name, all_total)),
               color = blue, lineend = "butt",
               size = .5, arrow = arrow(length = unit(.2, "cm"), 
                                        type = "closed")) +
  geom_point(data = leg, aes(y = `2014-2015`), color = yellow, size = 4) +
  geom_star(data = leg,
            aes(y = `2014-2015`), color = blue, fill = blue, size = 2) +
  geom_label(data = leg, 
             aes(label = country_name, y = 0), fill = yellow, color = blue,
             family = "so", size = 3) +
  geom_text(data = leg, aes(y = `2019-2020` + 100 * sign(`2019-2020`), 
                            hjust = ifelse(sign(`2019-2020`) == -1, 1, 0)),
            label = "Total",
            color = blue, family = "so", size = 2) +
  geom_text(data = leg, aes(y = `2014-2015`), size = 2, color = blue,
            label = "2014-2015", family = "so", vjust=  -2) +
  geom_text(data = leg, aes(y = `2019-2020` + 200 * sign(`2019-2020`)), 
            size = 2, color = blue,
            label = "2019-2020", family = "so", vjust=  -2) +
  geom_text(inherit.aes = FALSE, x = 4.5, y = -7125,
            label = "Change in Participants\nReceived",
            lineheight = .9, color = yellow, family = "so",
            size = 2.5) +
  geom_text(inherit.aes = FALSE, x = 4.5, y = 7125,
            label = "Change in Participants\nSent",
            lineheight =.9, color = yellow, family = "so",
            size = 2.5) +
  # end legend elements
  scale_y_continuous(limits = c(-9000, 9000), 
                     breaks = c(-9400, -budge, budge, 9300), 
                     labels = c("Participants Received", "0", 
                                "0", "Participants Sent")) +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = "so"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = alpha(yellow, .75), hjust = c(0, .5, .5, 1)),
        axis.line.x = element_line(color = yellow,
                                   arrow = arrow(length = unit(.2, "cm"),
                                                 ends = "both", type = "closed")),
        panel.grid = element_blank(),
        plot.title = element_text(color = yellow, hjust = .5, size = 24,
                                  margin = margin(t = 5)),
        plot.subtitle = element_textbox_simple(fill = blue, color = yellow,
                                               margin = margin(t = 5, b = 10),
                                               family = "ko", size = 14),
        plot.caption = element_textbox_simple(fill = blue, color = alpha(yellow, .75),
                                              family = "so", halign = .5, size = 6,
                                              margin = margin(b = 5)),
        plot.margin = margin(r = 15)) +
  labs(title = "Swelling the Ranks", 
       subtitle = "Participation in the EU's Erasmus program has grown significantly from " %+%
         "2014 to 2020. " %+%
         "Below are the 25 countries with the highest participation " %+%
         " in 2020, among which Germany has shown the greatest increase.",
       caption = "Graphic by Spencer Schien (@MrPecners) | Data from Data.Europa",
       x = "", y = "")

ggsave(filename = "2022/2022-03-08/final_plot.png", device = "png", bg = blue)
