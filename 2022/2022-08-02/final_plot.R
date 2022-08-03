library(tidyverse)
library(epubr)
library(tidytext)
library(zoo)
library(showtext)
library(ggtext)
library(glue)
library(geomtextpath)
library(lubridate)
library(packcircles)
library(patchwork)
library(NatParksPalettes)
library(sf)
library(rnaturalearth)

# load data ------------------------------------------

d <- "2022-08-02"
data <- tidytuesdayR::tt_load(d)
frogs <- data$frogs
rm(data)

# add dummie col for circlepacking
frogs <- frogs %>%
  mutate(dummie = 1)

# set color palettes, fonts ------------------------------------------

c1 <- natparks.pals("Glacier")
c2 <- natparks.pals("Arches2")

pals <- c(rev(c1[2:5]), c2[2:5])
font_add_google("Love Ya Like A Sister", "sis")
font_add_google("Gaegu", "n")
showtext_auto()

# I'm using patchwork for a laying akin to faceting, 
# set up vectors for facet rows and cols

group1 <- unique(frogs$HabType)
group2 <- unique(frogs$Structure)

# Build individual plots of circle packs, which will be 
# combined using patchwork. This results in a list
# of plots.

plots <- map(group1, function(x) {
  map(group2[order(group2)], function(y) {
    dd <- frogs |>
      filter(HabType == x & Structure == y) |>
      mutate(ccol = ifelse(Female == 1, c2[1], c1[1]),
             cfill = ifelse(Female == 1, c2[4], c1[4])) |>
      arrange(Female)
    
    if (x != group1[1]) {
      title <- ""
    } else {
      title <- y
    }
    
    if (nrow(dd) == 0) {
      # Add blank plot if no data for facet
      qplot(x = c(-10:10), y = c(-10:10), geom = "blank") +
        theme_void() +
        theme(text = element_text(family = "n"),
              aspect.ratio = 1,
              #panel.background = element_rect(fill = bg, color = bg),
              plot.title = if (title != "") {
                element_text(hjust = .5)
              } else { element_blank() },
              plot.margin = margin(0,0,0,0)) +
        labs(title = str_to_title(title))
    } else {
      # calculate circle polygons for packing
      res <- circleProgressiveLayout(dd$dummie)
      # join with data, need this for coloring by sex
      dat.gg <- circleLayoutVertices(res, npoints = 50) |>
        left_join(dd %>% mutate(id = row_number()))
      
      # build plot of packed circles
      ggplot(data = dat.gg, aes(x, y, group = id)) +
        geom_polygon(aes(colour=ccol, fill=cfill), size = .1) +
        xlim(-10, 10) +
        ylim(-10, 10) +
        coord_cartesian(clip = "off") +
        scale_color_identity() +
        scale_fill_identity() +
        theme_void() +
        theme(text = element_text(family = "n"),
              aspect.ratio = 1,
              #panel.background = element_rect(fill = bg, color = bg),
              plot.title = if (title != "") {
                element_text(hjust = .5)
              } else { element_blank() },
              plot.margin = margin(0,0,0,0),
              legend.position = "none") +
        labs(title = str_to_title(title))
    }
  })
  
})

# set up column title facets ------------------------------------------

pond_lab <- qplot(x = c(0:10), y = c(0:10), geom = "blank") +
  geom_text(aes(label = "Pond", x = 5, y = 5), size = 5,
            lineheight = .9, family = "n") +
  scale_x_continuous(limits = c(0, 10), expand = c(0,0)) +
  theme_void() +
  theme(plot.margin = margin(0,0,0,0),
        plot.title = element_blank()) +
  #theme(panel.background = element_rect(fill = pals[11], color = pals[11])) +
  coord_cartesian(clip = "off")

res_lab <- qplot() +
  geom_text(aes(label = "Resevoir", x = 1, y = 1), size = 5,
            lineheight = .9, family = "n") +
  scale_x_continuous(expand = c(0,0)) +
  theme_void() +
  theme(plot.margin = margin(0,0,0,0)) +
  #theme(panel.background = element_rect(fill = pals[6], color = pals[6])) +
  coord_cartesian(clip = "off")

river_lab <- qplot() +
  geom_text(aes(label = "River", x = 1, y = 1), size = 5,
            lineheight = .9, family = "n") +
  scale_x_continuous(expand = c(0,0)) +
  theme_void() +
  theme(plot.margin = margin(0,0,0,0)) +
  #theme(panel.background = element_rect(fill = pals[6], color = pals[6])) +
  coord_cartesian(clip = "off")

# set up title and caption ------------------------------------------

title <- qplot() +
  geom_text(aes(x = 0, y = 1.5), label = "OREGON SPOTTED FROGS", size = 15,
            hjust = 0, family = "sis") +
  geom_textbox(aes(x = 0, y = .75), 
               label = glue("Radio-telemetry data shows habitat use of ",
                            "<span style='color:{c1[2]}'>**male**</span> and ",
                            "<span style='color:{c2[2]}'>**female**</span> frogs ",
                            "at Crane Prairie Reservoir in Oregon. Herbaceous vegetation ",
                            "was the most popular structure for both sexes, and the river ",
                            "was the only habitat where males outnumbered females."), 
               size = 6, width = unit(8.75, "in"), family = "n",
               hjust = 0, vjust = 1, box.size = 0, color = "grey50") +
  scale_y_continuous(limits = c(-1,2), expand = c(0,0)) +
  scale_x_continuous(limits = c(0, 1), expand = c(0,0)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(t = 10, l = 5, r = 0, b = 10))

caption <- qplot(x = c(0:10), y = c(0:10), geom = "blank") +
  geom_text(aes(x = 9, y = 1), 
            label = "Graphic by Spencer Schien (@MrPecners) | Data from USGS",
            family = "n", color = "grey70", hjust = 1) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(0, 10), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 10), expand = c(0,0)) +
  theme_void() +
  theme(plot.margin = margin(0,0,10,0),
        aspect.ratio = .1)

# set up inset world ortho map ------------------------------------------

# based code below on this gist:
# https://gist.github.com/fzenoni/ef23faf6d1ada5e4a91c9ef23b0ba2c1

world <- ne_countries(scale = "small", returnclass = "sf")

lat <- 43.7
long <- -121.8

prj <- glue("+proj=ortho +lat_0={lat} +lon_0={long} +x_0=0 +y_0=0 +a=6375000 +b=6375000 +units=m +no_defs")

# create water polygon that will be plot background
water <- st_sfc(st_point(c(0, 0)), crs = prj) %>%
  st_buffer(., 6371000) %>%
  st_transform(crs = 4326)

# pull circle coords so we can translate our prj
circle_coords <- st_coordinates(water)[, c(1,2)]
circle_coords <- circle_coords[order(circle_coords[, 1]),]
circle_coords <- circle_coords[!duplicated(circle_coords),]

# create flat circle so we can intersect with world
rectangle <- list(rbind(circle_coords,
                        c(X = 180, circle_coords[nrow(circle_coords), 'Y']),
                        c(X = 180, Y = 90),
                        c(X = -180, Y = 90),
                        c(X = -180, circle_coords[1, 'Y']),
                        circle_coords[1, c('X','Y')])) %>% 
  st_polygon() %>% st_sfc(crs = 4326)


# plot illustrates what we're doing
rectangle %>%
  ggplot()+
  geom_sf(data = world) +
  geom_sf(color = "red", fill = alpha("red", .5)) +
  theme_void()

# This solves certain errors that get thrown, don't really understand why
sf::sf_use_s2(FALSE)

# clip world to our flat circle
w <- st_intersection(world, rectangle)

# get spot for the location of our data
spot <- st_sfc(st_point(c(long, lat)), crs = 4326)

# plot with ortho prj
loc_plot <- ggplot(data = w) +
  geom_sf(data = water, color = NA, fill = c1[3]) +
  geom_sf(fill = c2[2], size = .05, color = "white") +
  geom_sf(data = spot, color = c1[1], fill = "white",
          size = 2, shape = 21, stroke = 1.25) +
  coord_sf(crs = prj) +
  theme_void() +
  theme(plot.margin = margin(t = -10, b = 20))
loc_plot

# set up legend plots ------------------------------------------

female_plot <- qplot() +
  geom_point(aes(x = 0, y = 0),
             shape = 21, color = c2[1], fill = c2[4], size = 5) +
  annotate(geom = "text", x = 0, y = 0, label = "One recorded\nfemale",
           family = "n", vjust = -1, size = 5, lineheight = .9) +
  coord_sf(clip = "off") +
  theme_void()

male_plot <- qplot() +
  geom_point(aes(x = 0, y = 0),
             shape = 21, color = c1[1], fill = c1[4], size = 5) +
  annotate(geom = "text", x = 0, y = 0, label = "One recorded\nmale",
           family = "n", vjust = -1, size = 5, lineheight = .9) +
  coord_sf(clip = "off") +
  theme_void()

# set patchwork layout and plot ------------------------------------------

layout <- c(
  area(1,1,1,6),
  area(2,2),
  area(2,3),
  area(2,5,2,6),
  area(3,1), area(3,2), area(3,3), area(3,4), area(3,5), area(3,6), 
  area(4,1), area(4,2), area(4,3), area(4,4), area(4,5), area(4,6), 
  area(5,1), area(5,2), area(5,3), area(5,4), area(5,5), area(5,6), 
  area(6,6)
)

wrap_plots(
  title,
  # legends,
  female_plot,
  male_plot,
  loc_plot,
  # Row 1
  pond_lab, plots[[1]][[1]], plots[[1]][[2]], plots[[1]][[3]], 
  plots[[1]][[4]], plots[[1]][[5]],
  # Row 2
  res_lab, plots[[2]][[1]], plots[[2]][[2]], plots[[2]][[3]], 
  plots[[2]][[4]], plots[[2]][[5]],
  # Row 3
  river_lab, plots[[3]][[1]], plots[[3]][[2]], plots[[3]][[3]], 
  plots[[3]][[4]], plots[[3]][[5]],
  caption,
  design = layout
)

# save file  ------------------------------------------

ggsave(filename = glue("2022/{d}/final_plot.png"), bg = "white",
       w = 9, h = 9)
