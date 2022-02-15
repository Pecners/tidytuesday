library(tidyverse)
library(showtext)
library(ggtext)
library(lubridate)

# Helper function for long text

`%+%` <- function(x, y) paste0(x, y)

data <- read_csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge05/data.csv")


d <- data %>%
  pivot_longer(cols = 2:3, names_to = "group", values_to = "value") %>%
  mutate(dates = ymd(paste0(Year, "-01-01"))) %>%
  select(-Year) %>%
  mutate(value = case_when(dates == ymd("1870-01-01") & group == "Slave" ~ 94,
                           dates == ymd("1870-01-01") & group == "Free" ~ 6,
                           TRUE ~ value),
         label = case_when(dates == ymd("1870-01-01") & group == "Free" ~ "100%",
                           value == 1.3 ~ "1.3%",
                           TRUE ~ as.character(value)))

r <- seq.Date(min(d$dates), max(d$dates), by = 125)

cuts <- rnorm(length(r), mean = 97.1, sd = .05)

cuts_df <- tibble(dates = r,
                  cuts = cuts)



dates <- seq.Date(min(d$dates), max(d$dates), by = 100)

top <- 97.1
bottom <- 96.9
c <- 97.0

for (i in 1:(length(dates) - 1)) {
  c[i+1] <- c[i] + rnorm(1, mean = 0, sd = .01)
  if (c[i+1] > top) {
    c[i+1] <- top - .01
  } else if (c[i+1] < bottom) {
    c[i+1] <- bottom + .01
  }
}

seed <- tibble(dates = dates, value = c) %>%
  filter(dates < ymd("1870-01-01")) %>%
  bind_rows(., tibble(
    dates = ymd("1870-01-01"),
    value = c[length(c)]
  )) %>%
  mutate(group = "seed")

# Define colors, based on WEB DuBois Plot:
# https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2022

red <- "#AF012E"
bg <- "#DDCFC0"

# Font to be used in plot

#font_add_google("Coda", "c", bold.wt = 800)
font_add_google("Bai Jamjuree", "bj", bold.wt = 700)
showtext_auto()

breaks <- ymd(paste0((seq(1790, 1870, by = 10)), "-01-01"))

# Custom function to darken caption text
# credit: https://gist.github.com/Jfortin1/72ef064469d1703c6b30

darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

f <- bind_rows(d, seed)


t <- f %>%
  filter(group != "seed") %>%
  ggplot(aes(dates, value, fill = group)) +
  geom_area(color = bg, size = .2, outline.type = "full") +
  scale_fill_manual(values = c(red, "black")) +
  geom_area(data = f %>% 
              filter(group == "seed" & dates < ymd("1870-01-02")), 
            fill = bg, color = "black") +
  geom_vline(xintercept =  breaks[2:8],
             color = bg) +
  geom_text(data = f %>%
              filter(group == "Free"), aes(y = 100.4, label = label),
            hjust = 0, size = 4, family = "bj") +
  coord_flip(ylim = c(97.0, 101), xlim = c(ymd("1870-01-01"), ymd("1790-01-01")),
             clip = "off") +
  annotate(geom = "text", x = ymd("1786-01-01"), y = 100.6,
           family = "bj", label = "PERCENT\nOF\nFREE NEGROES", lineheight = .75) +
  scale_x_date(breaks = breaks, date_labels = "%Y", expand = c(0, 0)) +
  scale_y_continuous(position = "right", breaks = c(97:100),
                     labels = c("3%", "2%", "1%", "")) +
  theme(aspect.ratio = 2/1,
        legend.position = "none",
        axis.line.y = element_line(color = bg),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = alpha("black", .5)),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 16, margin = margin(r = 15)),
        axis.text = element_text(family = "bj"),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.caption.position = "plot",
        plot.caption = element_textbox(margin = margin(t = 30), hjust = .5,
                                       family = "bj", color = darken(bg, 1.6)),
        plot.title = element_text(family = "bj", face = "bold",
                                  size = 26, margin = margin(b = 40, t = 20)),
        plot.title.position = "plot") +
  labs(x = "", y = "",
       title = "SLAVES AND FREE NEGROES.",
       caption = "GRAPHIC BY SPENCER SCHIEN (@MRPECNERS) | " %+%
         "DATA FROM #DUBOISCHALLENGE2022")

ggsave(t, filename = "2022/2022-02-15/final_plot.png", device = "png",
       bg = bg, width = unit(8.5, "in"), height = unit(11, "in"))

