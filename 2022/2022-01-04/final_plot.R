library(tidyverse)
library(tigris)
library(sf)
library(showtext)
library(tidycensus)
library(ggpubr)
library(auk)

ebird_data <- read_ebd("2022/2022-01-04/data/snowys.txt")

`%+%` <- function(x, y) paste0(x,y)

font_add_google("Kaushan Script", "ks")

showtext_auto()

wi <- counties(state = "WI") %>%
  st_transform(., crs = st_crs(4326))

l <- rnaturalearth::ne_download(type = "lakes", category = "physical", scale = "large")  %>%
  st_as_sf(., crs = st_crs(4326))

gl <- l %>% 
  filter(name %in% c("Lake Michigan", "Lake Superior")) %>%
  st_union()

wi_trim <- st_difference(wi, gl)

# Get census data

pop <- get_decennial(geography = "county",
                     state = "WI",
                     variables = "P1_001N", 
                     year = 2020) %>%
  mutate(county = str_remove_all(NAME, " County, Wisconsin$"))

# Adjust to group by sighting by day

top10 <- ebird_data %>%
  filter(lubridate::year(observation_date) == 2021) %>%
  group_by(county) %>%
  summarise(count = sum(as.numeric(observation_count))) %>%
  ungroup() %>%
  left_join(., pop) %>%
  mutate(n = count / value * 100000) %>%
  arrange(desc(n)) %>%
  head(10)

b <- top10 %>%
  ggplot(aes(reorder(county, n), n)) +
  geom_segment(aes(x = reorder(county, n), xend = reorder(county, n),
                   y = 0, yend = n),
               linetype = 2, size = 1) +
  geom_point(size = 8, color = "red") +
  scale_y_continuous(limits = c(0, 175)) +
  #geom_text(aes(label = n), color = "white") +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "red", size = 14),
        text = element_text(family = "ks"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, size = 12, lineheight = 1.25),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14)) + 
  labs(x = "", y = "")

# ggsave("graphics/top_10_counties.jpeg", device = "jpeg")


a <- ebird_data %>%
  filter(lubridate::year(observation_date) == 2020) %>%
  group_by(locality, latitude, longitude) %>%
  summarise(n = sum(as.numeric(observation_count))) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326)) %>%
  ggplot() +
  geom_sf(data= wi_trim, color = "white", size = .1, fill = "grey75") +
  geom_sf(data = wi_trim %>% filter(NAME %in% top10$county),
          color = "red", size = .5, fill = NA) +
  geom_sf(aes(size = n), alpha = .25, color = "red") +
  theme_void() +
  theme(legend.position = "bottom",
        text = element_text(family = "ks"),
        plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = 1, margin = margin(t = 0, b = -10)),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = .5, margin = margin(t = 10, b = 5),
                                    size = 12, lineheight = 1.25)) + 
  labs(size = "Locality Total 2021 Sightings")

# ggsave("graphics/snowy_sightings_map_2020.jpeg", device = "jpeg")

ggarrange(a, b, ncol = 2, widths = c(1.5, 1))  %>%
  annotate_figure(top = text_grob("Where to see snowy owls in Wisconsin\n" %+%
                                    "(2021 top 10 counties in red)", 
                                  size = 20, family = "ks"),
                  bottom = text_grob("Top counties determined by sightings per 100,000 residents. " %+%
                                       "Data spans Jan. 1-Dec. 13th, 2021.\n" %+%
                                       "Graph by Spencer Schien (@MrPecners) | Data from eBird Basic Dataset",
                                     size = 12, family = "ks"))

ggsave("2022/2022-01-04/final_plot.png", device = "png", bg = "white", width = 8.5, height = 7.5)
