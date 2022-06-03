library(tidyverse)
library(packcircles)
library(patchwork)
library(MetBrewer)
library(glue)
library(ggtext)
library(showtext)

data <- tidytuesdayR::tt_load("2022-05-31")
reputation <- data$reputation
poll <- data$poll
rm(data)

areas <- unique(reputation$name)
areas <- areas[order(areas)]

reputation <- reputation %>%
  group_by(company) %>%
  mutate(group = case_when(score == min(score) ~ "red",
                           score == max(score) ~ "green",
                           TRUE ~ "grey60"),
         dummie = 1)

pals <- met.brewer("Renoir")
font_add_google("Love Ya Like A Sister", "sis")
font_add_google("Neucha", "n")
showtext_auto()

plots <- map(areas, function(x) {
  map(c("green", "red"), function(y) {
    dd <- reputation %>%
      filter(name == x & group == y)
    if (y == "red") {
      title <- ""
      bg <- pals[6]
      cfill <- pals[7]
      ccol <- pals[8]
    } else {
      title <- x
      bg <- pals[11]
      cfill <- pals[11]
      ccol <- pals[12]
    }

    if (nrow(dd) == 0) {
      qplot(x = c(-10:10), y = c(-10:10), geom = "blank") +
        theme_void() +
        theme(text = element_text(family = "n"),
              aspect.ratio = 1,
              #panel.background = element_rect(fill = bg, color = bg),
              plot.title = if (title != "") {
                element_text(hjust = .5)
              } else { element_blank() },
              plot.margin = margin(0,0,0,0)) +
        labs(title = str_to_title(title))
    } else {
      res <- circleRepelLayout(dd$dummie)
      dat.gg <- circleLayoutVertices(res$layout, sizetype = "radius")
      n <- nrow(res$layout)
      if (n > 20) {
        tcol <- "white"
        yl <- 0
      } else {
        tcol <- ccol
        yl <- 5
      }
      
      ggplot(data = dat.gg, aes(x, y, group = id)) +
        geom_polygon(colour=ccol, fill=cfill) +
        xlim(-10, 10) +
        ylim(-10, 10) +
        coord_cartesian(clip = "off") +
        theme_void() +
        theme(text = element_text(family = "n"),
              aspect.ratio = 1,
              #panel.background = element_rect(fill = bg, color = bg),
              plot.title = if (title != "") {
                element_text(hjust = .5)
                } else { element_blank() },
              plot.margin = margin(0,0,0,0)) +
        labs(title = str_to_title(title))
    }
  })

})

high_lab <- qplot(x = c(0:10), y = c(0:10), geom = "blank") +
  geom_text(aes(label = "Company's\nHighest\nRating", x = 5, y = 5), size = 5,
            color = pals[12], lineheight = .9, family = "n") +
  scale_x_continuous(limits = c(0, 10), expand = c(0,0)) +
  theme_void() +
  theme(plot.margin = margin(0,0,0,0),
        plot.title = element_blank()) +
  #theme(panel.background = element_rect(fill = pals[11], color = pals[11])) +
  coord_cartesian(clip = "off")

low_lab <- qplot() +
  geom_text(aes(label = "Company's\nLowest\nRating", x = 1, y = 1), size = 5,
            color = pals[8], lineheight = .9, family = "n") +
  scale_x_continuous(expand = c(0,0)) +
  theme_void() +
  theme(plot.margin = margin(0,0,0,0)) +
  #theme(panel.background = element_rect(fill = pals[6], color = pals[6])) +
  coord_cartesian(clip = "off")

reputation %>%
  group_by(company) %>%
  filter(score == min(score)) %>%
  ungroup() %>%
  group_by(name) %>%
  tally()

title <- qplot() +
  geom_text(aes(x = 0, y = 1.5), label = "CITIZENS WANTED", size = 15,
            hjust = 0, family = "sis") +
  geom_textbox(aes(x = 0, y = .75), 
            label = glue("When asked to rate the 100 most visible companies in the country, ",
                         "Americans most often rate them ",
                         "<span style='color:{pals[12]}'>highest</span> in Vision, ",
                         "Growth, and Product & Service (P&S), ",
                         "while they rate them <span style='color:{pals[8]}'>lowest</span> in Citizenship."), 
            size = 5, width = unit(8.5, "in"), family = "n",
            hjust = 0, vjust = 1, box.size = 0, color = "grey50") +
  scale_y_continuous(limits = c(-1,2), expand = c(0,0)) +
  scale_x_continuous(limits = c(0, 1), expand = c(0,0)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(t = 10, l = 5, r = 0, b = 0))

caption <- qplot(x = c(0:10), y = c(0:10), geom = "blank") +
  geom_text(aes(x = 9, y = 1), 
            label = "Graphic by Spencer Schien (@MrPecners) | Data from the 2022 Axios Harris Poll",
            family = "n", color = "grey70", hjust = 1) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(0, 10), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 10), expand = c(0,0)) +
  theme_void() +
  theme(plot.margin = margin(0,0,10,0),
        aspect.ratio = .1)

layout <- c(
  area(1,1,1,8),
  area(2,1), area(2,2), area(2,3), area(2,4), area(2,5), area(2,6), area(2,7), area(2,8),
  area(3,1), area(3,2), area(3,3), area(3,4), area(3,5), area(3,6), area(3,7), area(3,8),
  area(4,8)
)

wrap_plots(
  title,
  # Row 1
  high_lab, plots[[1]][[1]], plots[[2]][[1]], plots[[3]][[1]], plots[[4]][[1]], 
  plots[[5]][[1]], plots[[6]][[1]], plots[[7]][[1]],
  # Row 2
  low_lab, plots[[1]][[2]], plots[[2]][[2]], plots[[3]][[2]], plots[[4]][[2]],
  plots[[5]][[2]], plots[[6]][[2]], plots[[7]][[2]], 
  caption,
  design = layout
)



ggsave(filename = "2022/2022-05-31/final_plot.png", bg = "white",
       w = 9, h = 4.5)
