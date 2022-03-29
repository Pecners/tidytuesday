library(tidyverse)
library(glue)
library(showtext)
library(ggtext)
library(patchwork)
library(scales)
library(ggthemes)

# Load data

data <- tidytuesdayR::tt_load("2022-03-22")
babes <- data$babynames
rm(data)

# Calculate letter frequency by year

letters <- babes %>%
  mutate(start_letter = str_sub(name, 1, 1)) %>%
  group_by(year, sex, start_letter) %>%
  summarise(total = sum(n)) %>%
  ungroup() %>%
  group_by(year, sex) %>%
  mutate(perc = total / sum(total))

# Set plot palette, font

font_add_google("DM Serif Display", "sd")
showtext_auto()

blue <- "#3bbbf7"
pink <- "#ff96ad"
bg <- "grey97"

# Make plot for each letter except A,
# which will serve as a guide

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
    theme(plot.background = element_rect(fill = bg, linetype = 0),
          panel.background = element_rect(fill = glue("grey90"), linetype = 0),
          legend.position = "none",
          plot.margin = margin(rep(1, 4)),
          aspect.ratio = .75)
  
})


names(plots) <- LETTERS[2:26]

# Set up a blank plot to use as a spacer.
# This worked better than plot_spacer() because
# it allows me to control margins

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

# Set up labels for A, the guide plot

labs <- tibble(
  x = c(1880, 2017),
  y = -.04,
  l = c("1880", "2017"),
  h = c(0, 1)
)

ylabs <- tibble(
  x = c(1879),
  y = c(0, .5),
  l = c("0%", "50%"),
  h = c(1),
  v = c(0, 1)
)

yright <- tibble(
  x = 2018,
  y = c(.04, .18),
  l = c(glue("<span style='color:{blue}'>**% of male names**</span>"), 
        glue("<span style='color:{pink}'>**% of female names**</span>")),
  h = 0
)

# Create the A guide plot

legend <- letters %>%
  filter(start_letter == "A") %>%
  ggplot(aes(year, perc, fill = sex)) +
  geom_area() +
  geom_rect(xmin = 1880, xmax = 2017,
            ymin = -.1, ymax = 0, fill = bg) +
  geom_rect(xmin = 1860, xmax = 1880,
            ymin = -.1, ymax = .5, fill = bg) +
  geom_rect(xmin = 2017, xmax = 2027,
            ymin = -.1, ymax = .5, fill = bg) +
  geom_text(data = labs, aes(label = l, x = x, y = y, hjust = h),
            inherit.aes = FALSE, size = 3) +
  geom_text(data = ylabs, aes(label = l, x = x, y = y, hjust = h, vjust = v),
            size = 3, inherit.aes = FALSE) +
  geom_richtext(data = yright, aes(label = l, x = x, y = y, hjust = h),
            size = 3, inherit.aes = FALSE, fill = NA, label.color = NA) +
  annotate(geom = "text", x = 1948.5, y = .4, label = "A",
           family = "sd", size = 6) +
  scale_y_continuous(limits = c(-.1, .5), expand = c(0, 0),
                     breaks = c(0, .5), labels = percent) +
  scale_x_continuous(limits = c(1860, 2027), expand = c(0, 0)) +
  scale_fill_manual(values = c(pink, blue)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(fill = bg, linetype = 0),
        panel.background = element_rect(fill = glue("grey90"), linetype = 0),
        legend.position = "none",
        plot.margin = margin(1,1,1,-10),
        aspect.ratio = .75) +
  labs(y = "", x = "")

# Set up the plot title

title <- ggplot() +
  theme_map() +
  geom_richtext(aes(x = 0, y = 25), 
                label = glue("Alphabetical Order"),
            family = "sd", size = 10, fill = NA, label.color = NA,
            hjust = 0) +
  geom_textbox(aes(x = 0, y = 7), size = 5,
                label = glue("The most popular starting letter ",
                             "for <span style='color:{pink}'>**female**</span> ",
                             "baby names in the US was **M** in 1880 ",
                             "(22% of female names) ",
                             "and **A** in 2017 (18%). For <span style='color:{blue}'>",
                             "**males**</span>, it was **J** ",
                             "in 1880 (20%) and in 2017 (13%)."),
                fill = NA, width = unit(3.5, "in"), hjust = 0, box.size = 0) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(limits = c(0, 35), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 25), expand = c(0,0)) +
  theme(plot.margin= margin(c(0,0,0,0), "mm"))

# Set up plot caption

cap <- ggplot() +
  theme_map() +
  #geom_vline(xintercept = 25) +
  geom_textbox(aes(x = 25, y = 0), size = 1.75, vjust = 0, hjust = 1, halign = 1,
               label = paste0("Graphic by Spencer Schien (@MrPecners), ",
                            "data from the {babynames} R package."),
               fill = NA, width = unit(3, "in"), box.size = 0,
               box.margin = margin(0), box.padding = margin(0),
               color = "grey50") +
  coord_cartesian(clip = "off") +
  scale_y_continuous(limits = c(0, 35), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 25), expand = c(0,0)) +
  theme(plot.margin= margin(c(0,0,0,0), "mm"),
        plot.background = element_rect(fill = bg, color = NA))

# Set up layout for patchwork

s <- 2

layout <- c(
  area(1,1,s,3), area(1,4,2,5),
  area(s+1, 4), area(s+1,5), area(s+1,6),
  area(s+2,1), area(s+2,2), area(s+2,3), area(s+2,4), area(s+2,5), area(s+2,6),
  area(s+3,1), area(s+3,2), area(s+3,3), area(s+3,4), area(s+3,5), area(s+3,6),
  area(s+4,1), area(s+4,2), area(s+4,3), area(s+4,4), area(s+4,5), area(s+4,6),
  area(s+5,1), area(s+5,2), area(s+5,3), area(s+5,4),
  area(s+5, 6)
)

# Create pat
wrap_plots(title, legend,
          plots$B,  plots$C,  plots$D,  
  plots$E,  plots$F,  plots$G,  plots$H,  plots$I,  
  plots$J,  plots$K,  plots$L,  plots$M,  
  plots$N,  plots$O,  plots$P,  plots$Q,  
  plots$R,  plots$S,  plots$T,  
  plots$U,  plots$V ,  plots$W,  
  plots$X,  plots$Y,  plots$Z, cap,
  design = layout) &
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = bg, color = NA)
  ))




ggsave(filename = "2022/2022-03-22/final_plot.png", device = "png", bg = bg,
       w = 8, h = 7)
