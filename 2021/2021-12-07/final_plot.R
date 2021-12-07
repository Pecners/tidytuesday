library(tidyverse)

`%+%` <- function(x, y) paste0(x, y)

data <- tidytuesdayR::tt_load("2021-12-07")

d <- data$spiders

# Country distribution

## The `distribution` column is pretty messy,
## so my goal is to extract individual countries
## and then count their occurrence in the data.

lone_countries <- d %>%
  filter(!str_detect(distribution, ", ")) %>%
  # one row has a typo listing Brazil as Brazi
  # fix this since Brazil is one of the top countries
  mutate(distribution = ifelse(distribution == "Brazi", "Brazil", distribution),
         distribution = str_remove_all(str_trim(distribution), " to.*| \\(.*| or.*|;|\\?")) %>%
  .[["distribution"]] %>%
  unique()

# For each unique country, how many rows is it detected?
# i.e. how many species are located within country?
country_count <- map_dbl(lone_countries, function(x) {
  nrow(d %>%
         filter(str_detect(distribution, x))
       )
})

cc <- tibble(
  location = lone_countries,
  count = country_count
)

cc %>%
  arrange(desc(count)) %>%
  head(10) %>%
  ggplot(aes(reorder(location, count), count)) +
  geom_col(width = .75, fill = "#00763C") +
  geom_text(aes(label = scales::comma(count, 1)), vjust = 1.5,
            color = "grey90") +
  labs(title = "Spider Country", x = "", y = "",
       subtitle = "Top 10 countries by spider species count",
       caption = "Graph by Spencer Schien (@pecners89) | Data from the World Spider Database") +
  scale_y_continuous(breaks = 4000, labels = "More Spiders") +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  theme_minimal(base_family = "serif") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(family = "serif"),
        axis.line.y = element_line(color = "grey30",
                                   arrow = arrow(length = unit(.3, "cm"))),
        axis.line.x = element_line(color = "grey30"),
        axis.text.y = element_text(angle = 90, size = 14),
        axis.text.x = element_text(size = 12),
        plot.title.position = "plot",
        plot.title = element_text(size = 24, hjust = .5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust = .5),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0),
        plot.background = element_rect(fill = "grey90"))

ggsave(filename = "2021/2021-12-07/final_plot.jpeg", device = "jpeg")
