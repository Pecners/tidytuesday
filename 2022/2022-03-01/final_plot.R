library(tidyverse)
library(tigris)
library(sf)
library(tidycensus)
library(ggpubr)

data <- tidytuesdayR::tt_load("2022-03-01")
stations <- data$stations
rm(data)

`%+%` <- function(x, y) paste0(x,y)

font_add_google("Bungee Shade", "bs")
font_add_google("ZCOOL QingKe HuangYou", "zc")
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



s_sf <- stations %>%
  filter(FUEL_TYPE_CODE == "ELEC" & STATE == "WI") %>%
  select(X, Y) %>%
  st_as_sf(., coords = c("X", "Y"), crs = 4326)

both <- st_join(s_sf, wi_trim) %>%
  as_tibble() %>%
  group_by(NAME) %>%
  tally()

per_cap <- left_join(pop, both, by = c("county" = "NAME")) %>%
  mutate(per_cap = n / (value / 100000)) %>%
  arrange(desc(per_cap)) %>%
  head(10)

blue <- "#33455d"
yellow <- "#c9af1c"

top10 <- per_cap %>%
  ggplot(aes(per_cap, reorder(county, per_cap))) +
  geom_segment(aes(x = 0, xend = per_cap,
                   yend = county),
               linetype = 2) +
  geom_point(size = 6, color = "yellow") +
  geom_text(aes(label = round(per_cap)), color = blue,
            size = 3, family = "zc") +
  scale_x_reverse() +
  scale_y_discrete(position = "right") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text = element_text(family = "zc"),
        axis.text.x = element_blank(),
        plot.title = element_text(size = 20, color = "#222d3d", hjust = 0,
                                  margin = margin(t = 10, b = 10)),
        plot.subtitle = element_text(color = "#222d3d"),
        plot.title.position = "plot",
        axis.text.y = element_text(color = blue, size = 16)) +
  labs(y = "", x = "",
       title = "Top 10 Counties",
       subtitle = "(Stations per 100k residents)")

map <- wi_trim %>%
  ggplot() +
  geom_sf(fill = alpha(blue, .7), color = "white", size = .1) +
  geom_sf(data = wi_trim %>%
            filter(NAME %in% per_cap$county),
          color = "yellow",
          fill = blue) +
  geom_sf(data = s_sf, alpha = .5, color = "yellow", size = 1) +
  theme_void() +
  theme(plot.margin = margin(r = -10),
        plot.title = element_text(family = "mo", color = "yellow",
                                  hjust = .5))


ggarrange(map, top10, ncol = 2, widths = c(1, .75)) %>%
  annotate_figure(top = text_grob("Electric Charging Stations in Wisconsin", 
                                  size = 22, family = "bs", lineheight = .8, color = "#222d3d"),
                  bottom = text_grob("Graphic by Spencer Schien (@MrPecners) | Data from US DOT",
                                     size = 12, family = "zc", vjust = -1,
                                     color = alpha(blue, .75))) 

ggsave(filename = "2022/2022-03-01/final_plot.png", device = "png",
       bg = alpha("white", .5))
