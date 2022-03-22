##############################################################
# This plot was inspired by Erin Davis' viral plot           #
# https://twitter.com/erindataviz/status/1489009794245541888 #
#                                                            #
# I used her explainer blog post as a guide, as well:        #
# https://erdavis.com/2022/02/09/how-i-made-the-viral-map/   #
##############################################################

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

# Calculate starting letter props per year

letters <- babes %>%
  mutate(start_letter = str_sub(name, 1, 1)) %>%
  group_by(year, sex, start_letter) %>%
  summarise(total = sum(n)) %>%
  ungroup() %>%
  group_by(year, sex) %>%
  mutate(perc = total / sum(total))

# Set up plot aesthetics 

font_add_google("DM Serif Display", "sd")
showtext_auto()

blue <- "#63b9f2"
pink <- "#fc9fb1"
bg <- "grey90"
bg2 <- "grey97"

# Create plots for each letter

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
    theme(plot.background = element_rect(fill = bg2, linetype = 0),
          panel.background = element_rect(fill = bg, linetype = 0),
          legend.position = "none",
          plot.margin = margin(rep(1, 4)),
          aspect.ratio = .75)
  
})


names(plots) <- LETTERS[2:26]

# Create blank plot for spacer.
# Works better than plot_spacer() because
# I can control margins

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

# Use A as legend, set up labels

labs <- tibble(
  x = c(1879, 1879,
        1880, 2017),
  y = c(0, .5,
        -.01, -.01),
  l = c("0%", "50%",
        "1880", "2017"),
  h = c(1, 1,
        0, 1),
  v = c(0, 1,
        1, 1)
)

tlabs <- tibble(
  x = 2017,
  y = c(.05, .18),
  l = c(glue("<span style='color:{blue}'>**% of male names**<span>"),
        glue("<span style='color:{pink}'>**% of female names**<span>"))
)

# Create A legend plot

legend <- letters %>%
  filter(start_letter == "A") %>%
  ggplot(aes(year, perc, fill = sex)) +
  geom_area() +
  annotate(geom = "text", x = 1948.5, y = .4, label = "A",
           family = "sd", size = 6) +
  # bottom white rect
  geom_rect(xmin = 1860, xmax = 2027,
            ymin = -.05, ymax = 0, fill = bg2) +
  # left white rect
  geom_rect(xmin = 1860, xmax = 1880,
            ymin = -.05, ymax = .5, fill = bg2) +
  # right white rect
  geom_rect(xmin = 2017, xmax = 2027,
            ymin = -.05, ymax = .5, fill = bg2) +
  # axis labels
  geom_text(data = labs, aes(x = x, y = y, vjust = v,
                             label = l, hjust = h),
            inherit.aes = FALSE, size = 3) +
  # right text
  geom_richtext(data = tlabs, aes(x = x, y = y, label = l),
               hjust = 0, label.color = NA, fill = NA,
               inherit.aes = FALSE, size = 3) +
  
  scale_y_continuous(limits = c(-.05, .5), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1860, 2027)) +
  scale_fill_manual(values = c(pink, blue)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(fill = bg2, linetype = 0),
        panel.background = element_rect(fill = bg, linetype = 0),
        legend.position = "none",
        plot.margin = margin(rep(1, 4)),
        aspect.ratio = .75)

# Create title

title <- ggplot() +
  theme_map() +
  geom_text(aes(x = 0, y = 20), size = 10,
            label = "Alphabetical Order", 
            family = "sd") +
  geom_textbox(aes(x = 0, y = 5),
               label = glue("The most popular starting letter for ",
                            "<span style='color:{pink}'>**female**</span> ",
                            "baby names in the US was **M** in 1880 ",
                            "(22% of female names) and **A** in 2017 (18%). ",
                            "For <span style='color:{blue}'>**males**</span>, ",
                            "it was **J** in 1880 (20%) and in 2017 (13%)."),
               width = unit(3.5, "in"),
               size = 5, fill = NA, box.size = 0) +
  scale_y_continuous(limits = c(0, 25)) +
  coord_cartesian(clip = "off") +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

# Create caption

cap <- ggplot() +
  theme_map() +
  geom_textbox(aes(x = 10, y = 0),
               label = paste0("Analysis and graphic by Spencer Schien (@MrPecners)<br>",
                            "Data from {babynames} R package"),
               width = unit(3.5, "in"), hjust = 1, halign = 1, vjust = 0,
               size = 2.5, fill = NA, box.size = 0, color = "grey50",
               box.margin = margin(0,0,0,0),
               box.padding = margin(0,0,0,0)) +
  scale_x_continuous(limits = c(0,10)) +
  scale_y_continuous(limits = c(0, 10)) +
  coord_cartesian(clip = "off") +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

# Set layout for patchwork

layout <- c(
  area(1,1,2,3), # Title
  area(1,4,2,5), # A guide
  area(3, 4), area(3,5), area(3,6), # Row 1: B-D
  area(4,1), area(4,2), area(4,3), area(4,4), area(4,5), area(4,6), # Row 2: E-J
  area(5,1), area(5,2), area(5,3), area(5,4), area(5,5), area(5,6), # Row 3: K-P
  area(6,1), area(6,2), area(6,3), area(6,4), area(6,5), area(6,6), # Row 4: Q-V
  area(7,1), area(7,2), area(7,3), area(7,4), # Row 5: W-Z
  area(7, 6) # caption
)

# Create patchwork

wrap_plots(title, legend,
  plots$B,  plots$C,  plots$D,  
  plots$E,  plots$F,  plots$G,  plots$H,  plots$I,  
  plots$J,  plots$K,  plots$L,  plots$M,  
  plots$N,  plots$O,  plots$P,  plots$Q,  
  plots$R,  plots$S,  plots$T,  
  plots$U,  plots$V ,  plots$W,  
  plots$X,  plots$Y,  plots$Z,
  cap,
  design = layout) &
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = bg2, color = NA)
  ))



ggsave(filename = "2022/2022-03-22/final_plot.png", device = "png", bg = bg2,
       height = 7, width = 8)

