library(tidyverse)
library(glue)
library(showtext)
library(ggtext)
library(geomtextpath)

# Load data

data <- tidytuesdayR::tt_load("2022-04-12")
indoor <- data$indoor_pollution
death_ts <- data$death_timeseries
death_fuel <- data$death_fuel
death_source <- data$death_source
fuel_access <- data$fuel_access
fuel_gdp <- data$fuel_gdp
rm(data)

names(death_source)[4] <- "rate"

names(fuel_gdp)[c(4,6)] <- c("perc", "pop")

change <- death_source %>%
  filter(Year %in% c(1990, 2019) & str_detect(Entity, "World Bank region")) %>%
  mutate(lab = str_remove(Entity, " - World Bank region"),
         lab = factor(lab, levels = c("Sub-Saharan Africa",
                                      "South Asia",
                                      "East Asia & Pacific",
                                      "Latin America & Caribbean",
                                      "Europe & Central Asia"))) 

# Palette
colors <- c(
  "#E5121A", # red
  "#6AA0BB",
  "#0E6AA0",
  "#758C9A",
  "#07A2C2"
)

font_add_google("Mukta", "c")
showtext_opts(dpi = 200)
showtext_auto()

change %>%
  ggplot(aes(Year, rate, group = lab, label = lab, color = lab)) +
  geom_segment(data = change %>%
                 filter(Year == 1990),
               aes(y = min(rate), yend = max(rate)),
               x = 1990, xend = 1990, color = "grey90") +
  geom_segment(data = change %>%
                 filter(Year == 2019),
               aes(y = min(rate), yend = max(rate)),
               x = 2019, xend = 2019, color = "grey90") +
  geom_textline(straight = TRUE, linewidth = 1, text_smoothing = 100,
                family = "c") +
  geom_point(size = 3) +
  geom_text(aes(label = round(rate), family = "c",
                x = ifelse(Year == 2019, Year + .5, Year - .5),
                y = ifelse(lab == "South Asia", rate + 3, rate),
                hjust = ifelse(Year == 2019, 0, 1))) +
  annotate(geom = "text", x = c(1990, 2019), y = c(-7, -7),
           label = c("1990", "2019"),
           family = "c") +
  annotate(geom = "text", x = c(1988, 2021), y = c(-30, -30),
           hjust = c(0, 1), color = "grey70",
           label = c("Source: Our World in Data",
                     "Spencer Schien (@MrPecners)"),
           family = "c") +
  scale_color_manual(values = colors) +
  coord_cartesian(clip = "off", expand = 0) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white", color = NA),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(color = colors[1], size = 24, face = "bold",
                                  margin = margin(b = 25), family = "c"),
        plot.subtitle = element_textbox(size = 14, face = "bold", family = "c",
                                        margin = margin(b = 20)),
        plot.margin = margin(t = 10, r = 10)) +
  labs(y = "", x = "",
       title = "South Asia has shown the largest decrease\nin deaths from household air pollution",
       subtitle = "Regional death rate, deaths per 100k")


ggsave(filename = "2022/2022-04-12/final_plot.png", device = "png")

       