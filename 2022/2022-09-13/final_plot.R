library(tidyverse)
library(tidytext)
library(grid)
library(wordcloud2)
library(colorspace)
library(cowplot)
library(ggtext)
library(showtext)
library(glue)
library(magick)
library(MetBrewer)
library(patchwork)
library(scales)

# library(topicmodels)


# load data ------------------------------------------

d <- "2022-09-13"
data <- tidytuesdayR::tt_load(d)
bigfoot <- data$bigfoot
rm(data)

# prep data for text analysis  ------------------------------------------

tidy <- bigfoot %>%
  transmute(line = 1:nrow(bigfoot),
            text = observed) %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords())

tidy_by_line <- bigfoot %>%
  transmute(line = 1:nrow(bigfoot),
            text = observed,
            line = row_number()) %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords()) |> 
  group_by(line) |> 
  # this is how I'm defining reliance on senses  ------------------------------------------
  summarise(seeing = sum(word %in% c("see", "saw", "seen", "seeing", "sight", "spot")),
            hearing = sum(word %in% c("hear", "heard", "hearing", "sound"))) |> 
  left_join(bigfoot |> 
              transmute(line = row_number(),
                        classification))

x <- tidy %>%
  group_by(word) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) 

# I didn't use this but don't want to lose it.
#
# bf_dtm <- tidytext::cast_dtm(x, line, word, freq)
# 
# bf_lda <- LDA(bf_dtm, k = 2, control = list(seed = 1234))
# bf_topics <- tidy(bf_lda, matrix = "beta")
# 
# bf_top_terms <- bf_topics |> 
#   group_by(topic) |> 
#   slice_max(beta, n = 10) |> 
#   ungroup() |> 
#   arrange(topic, -beta)
# 
# bf_top_terms |> 
#   mutate(term = reorder_within(term, beta, topic)) |> 
#   ggplot(aes(beta, term, fill = factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ topic, scales = "free") +
#   scale_y_reordered()
# 
# gamma_lda <- tidy(bf_lda, matrix = "gamma")


# prep png for wordcloud mask
png(filename = "2022/2022-09-13/bf.png", width = 7, height = 7, units = "in", res = 500, bg = "white", )

grid.draw(bf_svg)

dev.off()

# color palette
colors <- MetBrewer::met.brewer("Moreau")

# need a vector as long as there are unique words in the 
# wordcloud, and want to bias it to get the affect I want.
# I played with this to dial it in.

pal <- colorRampPalette(colors, bias = 4)(nrow(x))

swatchplot(pal)

# Make wordcloud

wordcloud2(x, figPath = "2022/2022-09-13/bf.png", color = pal,
           widgetsize = c(1200,1200),
           fontFamily = "Julee")

# After creating the wordcloud, I open it in chrome and then 
# save it here in the repo as bfwc.png

img <- image_read("2022/2022-09-13/bfwc.png")
img <- image_crop(img, geometry = "900x1200")
g <- rasterGrob(img, interpolate=TRUE)

font_add_google("Julee", "julee")
showtext_auto()

# wordcloud plot ------------------------------------------
wc_plot <- qplot(0:10, 0:10, geom="blank") +
  draw_image(img, x = 0, y = 0, width = 10.5, height = 10.5) +
  theme_void() +
  theme(plot.margin = margin(0,0,0,-200),
        text = element_text(family = "julee", size = 30),
        plot.title = element_textbox(fill = colors[7], color = "white",
                                     padding = margin(4,4,2,4), r = unit(2, "points"),
                                     margin = margin(b = 5))) +
  labs(title = "Wordcloud of incident reports")

wc_plot

# prep data for other plots    ------------------------------------------

plot_prep <- tidy_by_line |> 
  mutate(ratio = seeing - hearing,
         sense = case_when(ratio > 0 ~ "sight",
                           ratio < 0 ~ "sound",
                           TRUE ~ "none"),
         sense = factor(sense, levels = c("sight",
                                          "sound",
                                          "none"))) 

# bar plot   ------------------------------------------
bars <- plot_prep |> 
  group_by(classification, sense) |> 
  summarise(total = sum(ratio),
            n = n()) |> 
  ungroup() |> 
  group_by(classification) |> 
  mutate(perc = n / sum(n) * sign(total),
         classification = factor(classification,
                                 levels = c("Class C",
                                            "Class B",
                                            "Class A"))) |> 
  filter(sense != "none") |> 
  mutate(lab = mean(perc)) |> 
  ggplot(aes(classification, perc, fill = classification)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_col(width = .5, aes(alpha = sense)) +
  geom_label(aes(x = classification, y = lab, 
                 label = classification),
             fill = "grey90", size = 12, family = "julee") +
  geom_text(aes(
    label = case_when(classification == "Class A" &
                        sense == "sight" ~ paste0(label_percent()(abs(perc)),
                                                  " rely more\non sight"),
                      classification == "Class A" &
                        sense == "sound" ~ paste0(label_percent()(abs(perc)),
                                                  " rely more\non hearing"),
                      TRUE ~ label_percent()(abs(perc))),
    y = perc + .05 * sign(perc),
    hjust = ifelse(sense == "sight", 0, 1)
    ), family = "julee",
            size = 10, lineheight = .25) +
  scale_fill_met_d(name = "Moreau") +
  scale_alpha_manual(values = c(1, .5)) +
  scale_y_continuous(labels = function(x) label_percent()(abs(x)),
                     limits = c(-.75, 1),
                     breaks = c(seq(from = -.5, to = 1, by = .5))) +
  coord_flip(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(r = 20),
        legend.position = "none",
        text = element_text(family = "julee", size = 30),
        plot.title.position = "plot",
        plot.title = element_textbox(fill = colors[7], color = "white", hjust = .5,
                                     padding = margin(4,4,2,4), r = unit(2, "points"),
                                     margin = margin(b = 5))) +
  labs(x = "", y = "",
       title = "Percent of reports relying more on one sense")

bars

# raincloud plot   ------------------------------------------

rain <- plot_prep |> 
  mutate(classification = factor(classification,
                                 levels = c("Class C",
                                            "Class B",
                                            "Class A"))) |> 
  ggplot(aes(classification, ratio)) +
  # raincloud code taken from Cedric Scherer's blog:
  # https://www.cedricscherer.com/2021/06/06/visualizing-distributions-with-raincloud-plots-and-how-to-create-them-with-ggplot2/
  ggdist::stat_halfeye(
    aes(fill = classification),
    adjust = 1, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  gghalves::geom_half_boxplot(
    side = "l", outlier.color = NA, center = TRUE, errorbar.draw = FALSE,
    width = .5, nudge = .1, alpha = .25,
    aes(fill = classification,
        color = classification)
  ) +
  geom_point(
    aes(fill = classification,
        color = classification),
    shape = 21,
    alpha = .1,
    position = position_jitter(
      seed = 1, width = .075
    )
  ) +
  stat_summary(fun.data = function(x) data.frame(y = median(x),
                                                 label = paste0("n = ",
                                                                label_comma()(length(x)))), 
               geom = "text", aes(x = classification, y = -30, color = classification),
               family = "julee", size = unit(10, "points"),
               position = position_nudge(x = -.25))  +
  scale_color_met_d(name = "Moreau") +
  scale_fill_met_d(name = "Moreau") +
  scale_y_continuous(labels = c("+40 instances\nof hearing",
                                "+20",
                                "Neutral",
                                "+20 instances\nof sight"),
                     breaks =  c(-40, -20, 0, 20)) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "julee", size = 30),
        plot.title.position = "plot",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(lineheight = .25),
        plot.title = element_textbox(fill = colors[7], color = "white", hjust = .5,
                                     padding = margin(4,4,2,4), r = unit(2, "points"),
                                     margin = margin(b = 5))) +
  labs(x = "", y = "",
       title = "Net sense reliance in reports")

rain

# title plot  ------------------------------------------

title <- qplot() +
  geom_text(aes(x = 0, y = 1.5), label = "SEEING IS BELIEVING", size = 45,
            hjust = 0, family = "julee", color = colors[7]) +
  geom_textbox(aes(x = 0, y = .75), 
               label = glue("Class A sasquatch incidents (defined as clear sightings) unsurprisingly ",
                            "show much greater reliance on  ",
                            "sight than sound, while Class B incidents (credible but no clear sighting) ", 
                            "show greater reliance on sound. Class C incidents are second- or third-hand and ",
                            "are considered to have high potential for inaccuracy."), 
               size = 16, width = unit(8.5, "in"), family = "julee",
               hjust = 0, vjust = 1, box.size = 0, color = darken(colors[4], .25),
               lineheight = .4) +
  scale_y_continuous(limits = c(-1,2), expand = c(0,0)) +
  scale_x_continuous(limits = c(0, 1), expand = c(0,0)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(t = 10, l = 5, r = 0, b = 10))

title

# caption plot ------------------------------------------
caption <- qplot(x = c(0:10), y = c(0:10), geom = "blank") +
  geom_text(aes(x = 0, y = 1), 
            label = "Graphic by Spencer Schien (@MrPecners) | Data from Bigfoot Field Researchers Organization",
            family = "julee", color = "grey70", hjust = 0,
            size = 8) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(0, 10), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 10), expand = c(0,0)) +
  theme_void() +
  theme(plot.margin = margin(10,0,10,0),
        aspect.ratio = .1)
caption

# set up patchwork layout   ------------------------------------------
layout <- c(
  area(1,1,3,4),
  area(4,1,9,2), 
  area(4,3,6,4),area(7,3,9,4),
  area(10,1)
)

plot(layout)

# plot patchwork   ------------------------------------------

wrap_plots(
  title,
  wc_plot,
  rain, bars,
  caption,
  design = layout
)

# save file  ------------------------------------------

ggsave(filename = glue("2022/{d}/final_plot.png"), bg = "white",
       w = 9, h = 6)

