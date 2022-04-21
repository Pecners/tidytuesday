library(tidyverse)
library(tidytext)
library(wordcloud)
library(lubridate)
library(zoo)
library(showtext)
library(MetBrewer)
library(glue)

data <- tidytuesdayR::tt_load("2022-04-19")
#big_dave <- data$big_dave
times <- data$times
rm(data)

tidy_books <- times %>%
  transmute(line = 1:nrow(times),
            text = answer,
            date = puzzle_date) %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords())

bing <- get_sentiments("bing")

sent <- tidy_books %>%
  inner_join(bing) %>%
  count(date, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

# Word cloud

tidy_books %>%
  count(word) %>%
  with(wordcloud(word, n, min.freq = 1,
                 max.words=200, random.order=FALSE, rot.per=0.35, 
                 colors=brewer.pal(8, "Dark2")))

# Sentiment over time

c <- met.brewer("OKeeffe1")
font_add_google("Vollkorn", "v")
showtext_auto()

sent %>%
  filter(!is.na(date) & year(date) > 2013) %>%
  ggplot(aes(date, sentiment, color = sentiment)) +
  geom_point(alpha = .5, size = 3) +
  geom_line(aes(y = rollmean(sentiment, 14, na.pad = TRUE)),
            size = .75, color = "black") +
  scale_color_gradient2(midpoint = 0, low = c[1], mid = c[6],
                        high = c[11], space = "Lab" ) +
  scale_x_date(expand = c(.02, 0)) +
  scale_y_continuous(breaks = seq(from = 5, to = -15, by = -5),
                     labels = c("+5 net\npositive\nwords",
                                seq(from = 0, to = -15, by = -5))) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text = element_text(family = "v"),
        plot.title.position = "plot",
        plot.title = element_text(family = "v", size = 26, face = "bold"),
        plot.subtitle = element_text(family = "v", size = 16),
        plot.caption.position = "plot",
        plot.caption = element_text(family = "v", hjust = 0, color = "grey70")) +
  labs(x = "", y = "",
       title = "Negative Answers from the New York Times",
       subtitle = glue("Net sentiment of NYT Cryptic Crossword answers ",
                       "tend to be more negative than positive."),
       caption = "Graphic by Spencer Schien | Data from Cryptic Crossword Clues")

ggsave("2022/2022-04-19/final_plot.png", bg = "white",
       width = 9, height = 4)

       