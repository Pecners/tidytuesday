library(tidyverse)
library(glue)
library(showtext)
library(ggtext)
library(MetBrewer)
library(geomtextpath)

# Load data

data <- tidytuesdayR::tt_load("2022-04-05")
news_orgs <- data$news_orgs
rm(data)

all_years <- tibble(
  year_founded = c(min(news_orgs$year_founded, na.rm = TRUE):max(news_orgs$year_founded, na.rm = TRUE))
)

cum_count <- news_orgs %>%
  right_join(., all_years) %>%
  mutate(count = ifelse(is.na(publication_name), 0, 1)) %>%
  arrange(year_founded) %>%
  group_by(year = year_founded) %>%
  summarise(n = sum(count)) %>%
  mutate(total = cumsum(n))

cum_count %>%
  filter(year < 2000) %>%
  summarise(rate = (max(total) - min(total)) / (max(year) - min(year)))

cum_count %>%
  filter(year >= 2000) %>%
  summarise(rate = (max(total) - min(total)) / (max(year) - min(year)))
  
font_add_google("Press Start 2P", "ps")
font_add_google("Orbitron", "ob")
showtext_auto()

# Colors

c <- met.brewer("OKeeffe1")

cum_count %>%
  filter(year < 2021) %>%
  ggplot(aes(year, total)) +
  geom_col(fill = c[11]) +
  geom_text(data = cum_count %>%
              filter(year %in% c(1958, 2020)),
            aes(label = total, y = total + 10), size = 2,
            color = c[1], family = "ps") +
  geom_textsmooth(label = "Cumulative count of digital-native local news outlets",
                  method = "gam", vjust = -1, hjust = .8, linewidth = 0,
                  color = c[1], text_smoothing = 40) +
  # Netflix
  geom_segment(x = 2007, xend = 2007,
               y = -50, yend = -5,
               arrow = arrow(length = unit(.1, "cm"), type = "closed"),
               color = c[8], size = .2) +
  annotate(geom = "text", x = 2007, y = -75, 
               label = glue("Netflix launches streaming service\n(2007)"),
               size = 2, color = c[9]) +
  # HTML
  geom_segment(x = 1989, xend = 1989,
               y = -50, yend = -5,
               arrow = arrow(length = unit(.1, "cm"), type = "closed"),
               color = c[8], size = .2)  +
  annotate(geom = "text", x = 1989, y = -75, 
            label = "World Wide Web is invented\n(1989)",
            size = 2, color = c[9]) +
  # Internet
  geom_segment(x = 1974, xend = 1974,
               y = -50, yend = -5,
               arrow = arrow(length = unit(.1, "cm"), type = "closed"),
               color = c[8], size = .2) +
  annotate(geom = "text", x = 1974, y = -75, 
            label = glue("Term 'Internet' is coined\n(1974)" ),
            size = 2, color = c[9]) +
  # Google
  geom_segment(x = 1998, xend = 1998,
               y = -50, yend = -5,
               arrow = arrow(length = unit(.1, "cm"), type = "closed"),
               color = c[8], size = .2) +
  annotate(geom = "text", x = 1998, y = -75, 
            label = glue("Google is founded\n(1998)"),
            size = 2, color = c[9]) +
  geom_segment(x = 1958, xend = 1958,
               y = -50, yend = -5,
               arrow = arrow(length = unit(.1, "cm"), type = "closed"),
               color = c[8], size = .2) +
  annotate(geom = "text", x = 1958, y = -65, 
           label = "1958",
           size = 2, color = c[9]) +
  geom_segment(x = 2020, xend = 2020,
               y = -50, yend = -5,
               arrow = arrow(length = unit(.1, "cm"), type = "closed"),
               color = c[8], size = .2) +
  annotate(geom = "text", x = 2020, y = -65, 
           label = "2020",
           size = 2, color = c[9]) +
  geom_textbox(x = min(cum_count$year), y = 725, hjust = 0, size = 6,
               label = "Riding the Digital Wave", halign = .5,
               family = "ps", width = unit(5.75, "in"),
               color = c[1], fill = c[8], box.r = unit(2, "pt")) +
  geom_textbox(x = min(cum_count$year), y = 550, hjust = 0, size = 5,
               label = glue("The number of small, independent online news organizations ",
                            "in the US and Canada has been growing along with the internet, ",
                            "showing an **18x** increase between 2000 and 2020."),
               color = c[1], fill = c[7], width = unit(7, "in"), box.size = 0) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(-100, NA), expand = expansion(mult = c(0.01, .025))) +
  coord_cartesian(clip = "off") +
  theme(panel.background = element_blank(),
        plot.background = element_rect(color = NA, fill = c[7]),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(color = c[11]),
        plot.caption = element_textbox(hjust = 1, color = c[8], halign = 1,
                                       width = unit(8.5, "in"))) +
  labs(y = "", x = "Outlet Founding Year",
       caption = glue("Analysis and graphic by Spencer Schien (@MrPecners) | ",
                      "Data from Project Oasis"))

ggsave(filename = "2022/2022-04-05/final_plot.png", device = "png",
       w = 9, h = 5)

       