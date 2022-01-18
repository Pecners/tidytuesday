library(tidyverse)
library(showtext)
library(emojifont)
library(ggtext)

# Helper function
`%+%` <- function(x, y) paste0(x, y)

# Font to be used in plot
font_add_google("Mountains of Christmas", "moc")

# Load Data
data <- tidytuesdayR::tt_load("2022-01-18")
chocolate <- data$chocolate
rm(data)

# Categorize ratings based on Flavors of Cacao
# Rating System

rc <- chocolate %>%
  mutate(ratcat = case_when(rating >= 4.0 ~ "Outstanding",
                            rating >= 3.5 ~ "Highly Recommend",
                            rating >= 3.0 ~ "Recommended",
                            rating >= 2.0 ~ "Disappointing",
                            rating >= 1.0 ~ "Unpleasant",
                            TRUE ~ "WHOOPSY"),
         recommended = ifelse(rating >= 3.0, 1, 0))


# Clean most memorable characteristics
c_char <- map_df(1:nrow(rc), function(x) {
  c_ <- rc[[x, "most_memorable_characteristics"]] %>%
    str_replace_all(., ",|\\(|\\)|\"|\\/", " ") %>%
    str_replace_all(., "  ", " ") %>%
    str_split(., pattern = " ")
  
  tibble(
    characteristic = c_[[1]],
    rating = rc[[x, "rating"]],
    recommended = rc[[x, "recommended"]],
    country = rc[[x, "company_location"]],
    id = x
    )
  
})

words <- c_char %>%
  group_by(characteristic) %>%
  summarise(n = n(),
            avg = mean(recommended),
            avg_rate = mean(rating)) %>%
  arrange(desc(n))

# Set up color palette

choc <- "#7b3f00"
nauteous <- "#D1E471"
bg <- "#fce8cf"
dchoc <- "#4c281a"

# Pull top and bottom words

top4 <- words %>%
  filter(n >= 100) %>%
  arrange(desc(avg)) %>%
  head(5) %>%
  mutate(e = "chocolate_bar",
         c = choc)

bottom4 <- words %>%
  filter(n >= 10) %>%
  arrange(avg) %>%
  head(5) %>%
  mutate(e = "nauseated_face",
         c = choc)

all <- bind_rows(top4, bottom4) %>%
  mutate(avg = avg - .5)
  
all %>%
  ggplot(aes(reorder(characteristic, avg), avg)) +
  geom_segment(aes(xend = reorder(characteristic, avg), y = 0, yend = avg - sign(avg) * .035), 
               linetype = 3) +
  geom_point(aes(color = ifelse(avg < 0, nauteous, bg)), size = 15) +
  geom_text(aes(label = str_to_upper(characteristic), y = 0 - .025 * sign(avg),
                hjust = ifelse(sign(avg) > 0, 1, 0)),
            family = "moc", fontface = "bold", size = 5, color = choc) +
  scale_color_identity() +
  geom_emoji(alias = all$e, x = reorder(all$characteristic, all$avg), y = all$avg, size = 14,
             color = all$c) +
  geom_textbox(inherit.aes = FALSE, aes(x = 8.5, y = -.35), family = "moc", width = unit(.3, "npc"),
               fill = choc, color = bg,
               label = "<span style='font-size:16pt'>" %+%
                 "Chocolate described with these words " %+%
                 "is likely to be recommended..." %+%
                 "</span>") +
  geom_textbox(inherit.aes = FALSE, aes(x = 3.5, y = .35), family = "moc", width = unit(.25, "npc"),
               fill = nauteous, color = choc,
               label = "<span style='font-size:16pt'>" %+%
                 "...while chocolate described with these words " %+%
                 "is likely to be thrown in the trash." %+%
                 "</span>") +
  geom_hline(yintercept = 0, linetype = 4) +
  scale_y_continuous(limits = c(-.5, .5), breaks = c(-.5, .5), 
                     labels = c("Trash Can\nFood", "Chocolaty\nHeaven")) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = bg),
        axis.line.x = element_line(arrow = arrow(ends = "both", length = unit(3, "mm")),
                                   color = dchoc),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = dchoc),
        axis.title = element_blank(),
        plot.title = element_text(size = 28, hjust = .5),
        text = element_text(family = "moc", size = 20, color = dchoc),
        plot.caption = element_text(size = 10, hjust = 0, lineheight = 1.1)) +
  labs(title = "What chocolate would you recommend?",
       caption = str_wrap("Because of the higher incidence of recommended chocolate (i.e. rated 3.0+) in the data, " %+% 
         "results here are limited to words that occurred at least 100 times " %+%
         "for recommended chocolate and 10 times for not recommended." %+%
         "\nGraph by Spencer Schien (@MrPecners) | Data from Flavors of Cacao", 150))



ggsave(filename = "2022/2022-01-18/final_plot.png", device = "png", bg = bg)

