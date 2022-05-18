library(tidyverse)
library(svgparser)
library(grid)
library(wordcloud2)
library(tidytext)
library(stopwords)
library(showtext)
library(MetBrewer)
library(glue)
library(png)
library(patchwork)
library(ggtext)

# Get data

eurovision <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')

# Get countries for stop words
all_countries <- stopwords_getlanguages(source = "snowball")

# Get stop words for all languages available
stops <- map_df(all_countries, function(x) get_stopwords(language = x))

# Get words from titles, remove stop words
t <- eurovision %>%
  transmute(line = 1:nrow(eurovision),
            text = song) %>%
  unnest_tokens(word, text) %>%
  anti_join(stops) %>%
  mutate(word = str_remove(word, "^\\w'"))

# Get frequencies
tidy <- t %>%
  group_by(word) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq))

# Pulled svg from here: https://commons.wikimedia.org/wiki/File:Wiki_Eurovision_Heart_(Infobox).svg
# I edited it to remove the inside paths so the whole heart would fill,
# which is necessary for wordcloud2 to mask

heart <- "<svg xmlns='http://www.w3.org/2000/svg' width='238' height='250'><path d='M180.938 1c-25.317 0-55.258 18.698-73.381 49.771-4.89-11.222-22.313-23.451-43.024-23.451-16.689 0-63.533 20.858-63.533 88.178 0 86.88 87.901 104.725 105.671 131.729 1.221 1.857 5.154 3.26 6.655-1.177 14.179-41.845 124.125-89.125 124.125-174.279-0.001-47.756-31.197-70.771-56.513-70.771z'/></svg>"

png(filename = "2022/2022-05-17/logo.png", width = 7, height = 7, units = "in", res = 500, bg = "white", )
grid.draw(read_svg(heart))
dev.off()

# Get fun font

font_add_google("Julee", "julee")
showtext_auto()

# Set a count for creating colors.
# In the end, I could have just used two colors,
# but technically here I'm using a continuous scale

count <- nrow(tidy) * 3

colors <- met.brewer("Benedictus", type = "continuous", n = count)
ind <- c(1:50, count:(count-(nrow(tidy) - 50)))
c_trim <- colors[ind]

# Create wordcloud

wordcloud2(tidy, figPath = "2022/2022-05-17/logo.png", size = 3,
           widgetsize = c(1200,1200), ellipticity = .9, gridSize = 10,
           fontFamily = "Julee", color = c_trim, backgroundColor = "white")

# After creating the wordcloud, I open it in chrome and then 
# save it here in the repo as heart.png

img <- readPNG("2022/2022-05-17/heart.png")
g <- rasterGrob(img, interpolate=TRUE)

# Now I'm just making a blank plot with the image.
# I want a plot so I can add the annotations through ggplot.

qplot(0:10, 0:10, geom="blank") +
  cowplot::draw_image(img, x = 0, y = 0, width = 10, height = 10) +
  geom_textbox(data = tibble(1), minwidth = unit(4.75, "in"),
               x = .1, y = 10.75, hjust = 0, vjust = 1, fill = c_trim[51],
           size = unit(12, "pt"), lineheight = .7, color = "white",
           label = "Sing about it, Eurovision", family = "julee",
           box.padding = margin(5,0,2,5)) +
  geom_textbox(data = tibble(1), color = c_trim[51], fill = c_trim[length(c_trim)],
                x = 9.9, y = .1, hjust = 1, vjust = 0, box.size = 0,
               label = glue("This word cloud shows the prevalence ",
                            "of words in the titles of Eurovision songs, ",
                            "with the top 50 words in ",
                            "<span style='color:{c_trim[1]}'>**pink**</span>."), 
                family = "julee", size = 7, minwidth = unit(3.5, "in"),
               box.padding = margin(5,0,2,5)) +
  scale_y_continuous(limits = c(0, 11), expand = c(0,0)) +
  scale_x_continuous(limits = c(0, NA), expand = c(0,0)) +
  theme_void() +
  annotate(geom = "text", x = .1, y = .1, hjust = 0, vjust = 0,
           family = "julee", color = "grey60", size = 3,
           label = glue("Graphic by Spencer Schien (@MrPecners) | ",
                        "Data from Eurovision"))


ggsave(filename = "2022/2022-05-17/final_plot.png", bg = "white",
       w = 9, h = 9)
