# This script will work as is, no need to run
# messy_geocoding.R if you have the
# geocoded_hometowns.rda file. If you don't have it,
# work though the messy_geocoding script.

library(tidyverse)
library(sf)
library(tigris)
library(rnaturalearth)
library(osmdata)
library(showtext)
library(ggtext)

# Helper function for long text

`%+%` <- function(x, y) paste0(x, y)

# Get data

data <- tidytuesdayR::tt_load("2022-02-08")
airman <- data$airmen

# Fix certain states

airman <- airman %>%
  mutate(state = case_when(state == "KN" ~ "KY",
                           state == "CN" ~ "CT",
                           #state == "VI" ~ "VA",
                           is.na(state) ~ "Unknown",
                           state == "Unk" ~ "Unknown",
                           state == "Haiti" ~ "HT",
                           TRUE ~ state)) 

# Load geocoded data

d <- read_rds("2022/2022-02-08/geocoded_hometowns.rda")

f_geo <- d %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)

# Group by place for points in map

ga <- f_geo %>%
  mutate(place = case_when(is.na(state) ~ "Unknown",
                           state == "Unk" ~ "Unknown",
                           state == "Unknown" ~ "Unknown",
                           state == "HT" ~ "Port au Prince, HT",
                           TRUE ~ paste(m, state, sep = ", "))) %>%
  group_by(place) %>%
  summarise(n = n())

# Group by state 

a_states <- airman %>%
  mutate(state = str_to_upper(state)) %>%
  group_by(state) %>%
  tally()

# Get states sf objects

states <- states()

# Remove states we won't be plotting

not_states <- c("Commonwealth of the Northern Mariana Islands",
                "Alaska",
                "Puerto Rico",
                "Hawaii",
                "American Samoa",
                "Guam")

states_skinny <- states %>%
  filter(!NAME %in% not_states) 

# Get Great Lakes and oceans so we can erase the water from map

gl <- ne_download(type = "lakes", category = "physical", scale = "large")  %>%
  st_as_sf() %>%
  st_transform(., crs = st_crs(states_skinny))

l <- ne_download(type = "land", category = "physical", scale = "large")  %>%
  st_as_sf() %>%
  st_transform(., crs = st_crs(states_skinny))

# gl now has a lot of lakes, need to specify our 
# Great Lakes

lakes <- c("Lake Erie",
           "Lake Michigan",
           "Lake Superior",
           "Lake Huron",
           "Lake Ontario")

gl <- gl %>%
  filter(name %in% lakes) %>%
  st_transform(crs = st_crs(states)) 

# I had trouble using API services to get Haiti boundaries,
# so I downloaded the shapefile

haiti_sf <- st_read("2022/2022-02-08/shapefiles/haiti/haiti_boundaries.shp")

# I decided to keep the Dominican Republic so the island doesn't 
# look weird when plotted

h <- haiti_sf %>%
  filter(admin_leve == 2) %>%
  mutate(STUSPS = c("DR", "HT")) %>%
  select(geometry, STUSPS) %>%
  group_by(STUSPS) %>%
  summarise()

# Also need Trinidad and Tobago

tb  <- opq(bbox = "Trinidad") %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  osmdata_sf()

tb_sf <- tb$osm_multipolygons %>%
  filter(name == "Trinidad and Tobago") %>%
  select(geometry) %>%
  mutate(STUSPS = "TD")

not_us <- bind_rows(h, tb_sf)

with_others <- states_skinny %>%
  bind_rows(., not_us)



# This intersection will limit borders to land

ll <- st_intersection(with_others, l)

missing <- anti_join(a_states, ll, by = c("state" = "STUSPS")) %>%
  .[["state"]]

ll_w_a <- left_join(ll, a_states, by = c("STUSPS" = "state"))

# Need to remove Great Lakes

f <- st_difference(ll_w_a, st_union(gl))

# Set up binning

ff <- f %>%
  mutate(n = replace_na(n, 0)) %>%
  mutate(b = case_when(n > 100 ~ "100+",
                       n > 50 ~ "51 - 100",
                       n > 10 ~ "11 - 50",
                       n > 0 ~ "1 - 10",
                       TRUE ~ "0"),
         b = factor(b, levels = c("0",
                                  "1 - 10",
                                  "11 - 50",
                                  "51 - 100",
                                  "100+")))

# Define colors, based on WEB DuBois Plot:
# https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2022

brown <- "#6B503B"
yellow <- "#DCA816"
red <- "#B2223F"
greige <- "#DDCFBE"
pink <- "#D5AFA7"
bg <- "#DDCFC0"

# Font to be used in plot

font_add_google("Coda", "c")
showtext_auto()

t <- ff %>%
  ggplot(aes(fill = b)) +
  geom_sf(size = .1, color = "black") +
  geom_sf(data = ga, aes(size = n), inherit.aes = FALSE, alpha = .5) +
  scale_fill_manual(values = c(greige, yellow, pink, red, brown)) +
  geom_sf_text(data = ga %>% filter(place == "Unknown"), aes(geometry = geometry),
               inherit.aes = FALSE, nudge_x = 200000,
               label = str_wrap("TWELVE AIRMEN WITH UNKNOWN HOMETOWNS", 20),
               lineheight = 1, hjust = 0, size = 4, family = "c") +
  geom_sf_text(data = ga %>% filter(place == "Port au Prince, HT"), aes(geometry = geometry),
               inherit.aes = FALSE, nudge_y = -200000,
               label = "HAITI", size = 3, family = "c") +
  geom_sf_text(data = ga %>% filter(place == "St. Croix, VI"), aes(geometry = geometry),
               inherit.aes = FALSE, nudge_x = 100000, hjust = 0, family = "c",
               label = str_wrap("US VIRGIN ISLANDS", 15), lineheight = 1, size = 3) +
  geom_sf_text(data = ga %>% filter(place == "Port of Spain, TD"), aes(geometry = geometry),
               inherit.aes = FALSE, nudge_x = -100000, family = "c",
               label = str_wrap("TRINIDAD AND TOBAGO", 15), 
               hjust = 1, lineheight = 1, size = 3) +
  coord_sf(crs = st_crs(3347)) +
  theme_void() +
  theme(legend.position = c(.3, .15),
        legend.direction = "horizontal",
        legend.title.align = 1,
        legend.spacing = unit(1, "cm"),
        legend.text = element_text(family = "c"),
        legend.title = element_text(size = 8, margin = margin(r = 10),
                                    family = "c"),
        plot.title = element_textbox_simple(hjust = .5, halign = .5,
                                  size = 26, family = "c", margin = margin(b = 10)),
        plot.subtitle = element_textbox_simple(size = 12, family = "c",
                                               halign = .5,
                                               width = unit(20, "cm"),
                                               margin = margin(b = -20)),
        plot.caption = element_textbox_simple(halign = .5, family = "c",
                                              color = alpha(brown, .5))) +
  labs(size = str_wrap("COUNT OF AIRMEN FROM CITY", 15),
       fill = str_wrap("COUNT OF AIRMEN FROM STATE", 15),
       title = "HOMETOWNS OF THE TUSKEGEE AIRMEN",
       subtitle = "TUSKEGEE AIRMEN HAILED FROM COAST TO COAST AND BEYOND. " %+%
         "ILLINOIS WAS HOME TO THE MOST, BUT 38 OTHER STATES PRODUCED " %+%
         "AT LEAST ONE AIRMAN, AS DID HAITI, THE US VIRGIN ISLANDS, AND " %+%
         "TRINIDAD AND TOBAGO.",
       caption = "GRAPHIC BY SPENCER SCHIEN (@MRPECNERS) | DATA FROM TUSKEGEE AIRMAN CHALLENGE") 


ggsave(t, filename = "2022/2022-02-08/final_plot.png", device = "png",
       bg = bg)

