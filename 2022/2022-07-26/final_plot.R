library(tidyverse)
library(epubr)
library(tidytext)
library(zoo)
library(showtext)
library(ggtext)
library(glue)
library(geomtextpath)

orig <- epub("data/murderbot1.epub")

txt <- orig$data[[1]] %>%
  filter(str_detect(text, "^Chapter")) 

scored <- txt %>%
  unnest_tokens(word, text) %>%
  left_join(sentiments) %>%
  left_join(get_sentiments("afinn")) %>%
  mutate(sentiment = case_when(is.na(sentiment) ~ 0,
                               sentiment == "negative" ~ -1,
                               TRUE ~ 1),
         cumsent = cumsum(sentiment),
         ind = row_number(),
         rollm = rollmean(sentiment, 100, na.pad = TRUE),
         value = replace_na(value, 0),
         afinn_cum = cumsum(value))

font_add_google("Orbitron", "orb")
font_add_google("Changa", "cha")
showtext_auto()


sec_text <- "grey50"
p_text <- "grey70"

p <- scored %>%
  ggplot(aes(ind, cumsent)) +
  geom_area(fill = alpha("red", .6),
            color = "grey50") +
  geom_segment(x = 0, xend = 0, y = 0, yend = -300,
               arrow = arrow(length = unit(.15, "cm"), type = "closed"),
               color = sec_text) +
  annotate(geom = "text", label = "Cumulative\nsentiment", y = -90,
           x = 5000, family = "orb", color = sec_text, lineheight = .85,
           size = 3.5, fontface = "bold") +
  annotate(geom = "curve", x = 5000, xend = 12250, y = -110, yend = -140,
           arrow = arrow(length = unit(.1, "cm"), type = "closed"),
           color = "grey50", size = .5, angle = 40) +
  annotate(geom = "text", x = 0, y = -315, label = "More\nNegative",
           family = "orb", lineheight = .8, color = sec_text,
           size = 4, fontface = "bold") +
  geom_hline(yintercept = 0, linetype = 2, color = sec_text) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(breaks = c(0),
                     labels = c("Neutral\nSentiment")) +
  coord_cartesian(clip = "off") +
  theme(text = element_text(family = "orb", color = sec_text),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "grey20", color = NA),
        axis.line.y = element_blank(),
        axis.line.x = element_line(arrow = arrow(length = unit(.15, "cm"), 
                                                 type = "closed"),
                                   color = sec_text),
        panel.grid = element_blank(),
        axis.text.y = element_text(color = sec_text, face = "bold", size = 12),
        axis.text.x = element_blank(),
        plot.title.position = "plot",
        plot.title = element_textbox(face = "bold", size = 30, hjust = .5,
                                     color = p_text),
        plot.subtitle = element_textbox(width = unit(7, "in"), hjust = .5,
                                        lineheight = 1.2, family = "cha",
                                        size = 15, margin = margin(t = 5, b = 15),
                                        color = p_text),
        plot.caption =  element_text(hjust = 0.03, color = alpha(p_text, .5),
                                     family = "cha", margin = margin(t = -5)),
        plot.caption.position = "plot") +
  labs(title = glue("All Systems <span style='color:{alpha('red', .75)}'>Red</span>"),
       subtitle = glue("Sentiment analysis of the novella text confirms that everything is in fact going wrong ",
                       "for Sec Unit in this first installment of the Murderbot Diaries."),
       x = "Story Progression",
       y = "",
       caption = "Graphic by Spencer Schien (@MrPecners)")

ggsave(p, filename = "2022/2022-07-26/final_plot.png", bg = "black",
       w = 9, h = 6)
