library(tidyverse)
library(glue)
library(showtext)
library(ggtext)
library(patchwork)
library(ggthemes)

# Load data

data <- tidytuesdayR::tt_load("2022-03-22")
babes <- data$babynames
rm(data)

letters <- babes %>%
  mutate(start_letter = str_sub(name, 1, 1)) %>%
  group_by(year, sex, start_letter) %>%
  summarise(total = sum(n)) %>%
  ungroup() %>%
  group_by(year, sex) %>%
  mutate(perc = total / sum(total))

font_add_google("DM Serif Display", "sd")
showtext_auto()

blue <- "#89cff0"
pink <- "#f5acbc"
bg <- "grey90"

plots <- map(LETTERS[2:26], function(x) {
  letters %>%
    filter(start_letter == x) %>%
    ggplot(aes(year, perc, fill = sex)) +
    geom_area() +
    annotate(geom = "text", x = 1948.5, y = .4, label = x,
             family = "sd", size = 6) +
    scale_y_continuous(limits = c(0, .5), expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = c(pink, blue)) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", linetype = 0),
          panel.background = element_rect(fill = bg, linetype = 0),
          legend.position = "none",
          plot.margin = margin(rep(1, 4)),
          aspect.ratio = .75)
  
})


names(plots) <- LETTERS[2:26]

blank <- letters %>%
  filter(start_letter == "A") %>%
  ggplot(aes(year, y = 0)) +
  geom_area() +
  scale_y_continuous(limits = c(0, .5), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_void() +
  theme(plot.background = element_rect(fill = bg, color = NA),
        panel.background = element_rect(fill = bg, color = NA),
        legend.position = "none",
        plot.margin = margin(rep(1, 4)),
        aspect.ratio = .75)

legend <- letters %>%
  filter(start_letter == "A") %>%
  ggplot(aes(year, perc, fill = sex)) +
  geom_area() +
  annotate(geom = "text", x = 1948.5, y = .4, label = "A",
           family = "sd", size = 6) +
  scale_y_continuous(limits = c(0, .5), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c(pink, blue)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", linetype = 0),
        panel.background = element_rect(fill = bg, linetype = 0),
        legend.position = "none",
        plot.margin = margin(rep(1, 4)),
        aspect.ratio = .75)

title <- ggplot() +
  theme_map() +
  geom_text(aes(x = 0, y = 0),
            "Alphabetical Order", family = "sd") +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

layout <- c(
  area(1,1,2,3),
  area(3, 4), area(3,5), area(3,6),
  area(4,1), area(4,2), area(4,3), area(4,4), area(4,5), area(4,6),
  area(5,1), area(5,2), area(5,3), area(5,4), area(5,5), area(5,6),
  area(6,1), area(6,2), area(6,3), area(6,4), area(6,5), area(6,6),
  area(7,1), area(7,2), area(7,3), area(7,4), area(7,5)
  
)


wrap_plots(title, plots$A,  plots$B,  plots$C,  plots$D,  
  plots$E,  plots$F,  plots$G,  plots$H,  plots$I,  
  plots$J,  plots$K,  plots$L,  plots$M,  
  plots$N,  plots$O,  plots$P,  plots$Q,  
  plots$R,  plots$S,  plots$T,  
  plots$U,  plots$V ,  plots$W,  
  plots$X,  plots$Y,  plots$Z,
  design = layout)



ggsave(filename = "2022/2022-03-22/final_plot.png", device = "png", bg = "white")

