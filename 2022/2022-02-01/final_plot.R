library(tidyverse)
library(showtext)
library(emojifont)
library(ggtext)
library(scales)
library(ggbeeswarm)
library(corrr)
library(camcorder)

gg_record(
  dir = file.path(tempdir(),"recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 10, # width of saved image
  height = 8, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# Helper function
`%+%` <- function(x, y) paste0(x, y)

# Load Data
repo <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01"
breed_rank <- read_csv(paste(repo, "breed_rank.csv", sep = "/"))
breed_traits <- read_csv(paste(repo, "breed_traits.csv", sep = "/"))
trait_description <- read_csv(paste(repo, "trait_description.csv", sep = "/"))

# Font to be used in plot
font_add_google("Merriweather", "m")
font_add_google("Gochi Hand", "gh")


# Because joining wasn't working for some reason,
# bind_cols instead

full <- bind_cols(
  breed_rank %>%
    select(-c(links, Image)),
  breed_traits %>%
    select(-Breed)
)

best_worst <- map_df(2:9, function(x) {
  y <- str_extract(names(full)[x], "\\d*")
  t <- full %>%
    select(1, year_rank = x, 10:25) %>%
    filter(!is.na(year_rank)) %>%
    arrange(year_rank) %>%
    mutate(year = y)
  
  m <- nrow(t)
  
  t[c(1:5, (m-4):m), ]
})

t <- best_worst %>%
  mutate(cat = ifelse(year_rank < 6, "best", "worst")) %>%
  select(-c(`Coat Type`, `Coat Length`, year_rank, Breed)) %>%
  group_by(cat, year) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  ungroup() %>%
  pivot_longer(cols = 3:last_col(), names_to = "trait", values_to = "score") %>%
  pivot_wider(names_from = cat, values_from = score) %>%
  group_by(trait) %>%
  mutate(w = as.numeric(year) - 2012) %>%
  summarise(wm_best = weighted.mean(best, w),
            wm_worst = weighted.mean(worst, w)) %>%
  ungroup() %>%
  mutate(diff = wm_best - wm_worst,
         o = abs(diff),
         perc = diff / 4,
         hj = ifelse(diff < 0, 1, 0),
         hl = ifelse(diff < 0, "Lower", "Higher"),
         l = paste(percent(abs(perc), 1), hl)) %>%
  arrange(desc(o))

# Color palette
dark_blue <- "#013593"
light_blue <- "#B1D4ED"
orange <- "#FE5047"
brown <- "#50403D"

t %>%
  head(5) %>%
  left_join(., trait_description %>%
              select(1:3), by = c("trait" = "Trait")) %>%
  mutate(trait = factor(trait, levels = t %>%
                          head(5) %>%
                          .[["trait"]])) %>%
  ggplot(aes(1, wm_best)) +
  geom_text(aes(y = .95, label = str_wrap(Trait_1, 15)), hjust = 1,
            size = 4, family = "gh", color = dark_blue) +
  geom_text(aes(y = 5.05, label = str_wrap(Trait_5, 15)), hjust = 0,
            size = 4, family = "gh", color = dark_blue) +
  geom_segment(y = 1, yend = 5, x = 1, xend = 1, alpha = .25,
               arrow = arrow(ends = "both", length = unit(.15, "cm")),
               color = dark_blue) +
  ###################
  # bracket segments#
  ###################
  
  geom_segment(aes(y = wm_worst, yend = wm_worst, x = 1, xend = 1.5),
               color = brown, lineend = "round") +
  geom_segment(aes(x = 1.5, xend = 1.5, y = wm_worst, 
                   yend = wm_best - sign(diff) * .2),
               arrow = arrow(length = unit(.25, "cm"), type = "closed"),
               color = brown, lineend = "round") +
  geom_point(color = orange, aes(x = 1.5), size = 10, shape = 23,
             fill = orange, alpha = .75) +
  geom_point(aes(y = wm_worst), color = brown, size = 10, shape = 15) +
  geom_text(aes(label = comma(wm_best, .1), y = wm_best, x = 1.5),
            color = "white", family = "gh") +
  geom_text(aes(label = comma(wm_worst, .1), y = wm_worst),
            color = "white", family = "gh") +
  scale_x_discrete(labels = function(x) str_wrap(x, 20)) +
  scale_y_continuous(limits = c(.5, 5.5), breaks = c(1, 5)) +
  coord_flip() +
  facet_wrap(~ trait, ncol = 1) +
  theme_minimal() +
  theme(text = element_text(family = "gh"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = light_blue, color = NA),
        panel.grid = element_line(color = dark_blue),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_textbox_simple(hjust = .5, size = 22,
                                            margin = margin(b = 10)),
        plot.subtitle = element_textbox_simple(size = 14,
                                               margin = margin(b = 10),
                                               lineheight = 1),
        plot.caption = element_textbox_simple(margin = margin(t = 10)),
        strip.text = element_text(color = dark_blue, size = 16)) +
  labs(y = "Dog Breed Average Ratings",
       x = "Dog Breed Traits",
       title = "What differentiates " %+%
       "<span style='color:" %+% orange %+% "'>**popular**</span> " %+%
       "and <span style='color:" %+% brown %+% "'>**unpopular**</span> " %+%
       "dog breeds?",
       subtitle = "No barking, but drooling is okay. <span style='color:" %+%
         orange %+% "'>**Popular**</span> breeds bark A LOT less than " %+%
         "<span style='color:" %+% brown %+% "'>**unpopular**</span> breeds, " %+%
         "but they drool more!",
       caption = "<span style='color:" %+% orange %+% "'>**Popular**</span> " %+%
         "and <span style='color:" %+% brown %+% "'>**unpopular**</span> " %+%
         "breeds defined here as the highest and lowest five ranked breeds in a year. " %+%
         "Data spans from 2013 to 2020, and values represent an overall weighted average, " %+%
         "with recent years weighted more. " %+%
         "Graph by Spencer Schien (@MrPecners) | " %+%
         "Data from American Kennel Club")


ggsave(filename = "2022/2022-02-01/final_plot.png", device = "png", bg = light_blue)

gg_playback(
  name = "make_of_dogs.gif",
  first_image_duration = 8,
  last_image_duration = 12,
  frame_duration = .25,
  background = "white"
)
