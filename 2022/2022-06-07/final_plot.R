library(tidyverse)
library(circlepackeR)
library(data.tree)
library(packcircles)
library(ggiraph)
library(glue)
library(scales)
library(scico)
library(MetBrewer)


data <- tidytuesdayR::tt_load("2022-06-07")
#static_list <- data$static_list
contribs <- data$contribution_data_all_states
rm(data)

saveRDS(contribs, "../portfolio_website/content/interactive/anti_lgbtq/data.rds")

nona <- contribs %>%
  filter(!is.na(Company)) %>%
  group_by(Company) %>%
  summarise(total = sum(Amount, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(total)) %>%
  mutate(id = row_number())


res <- circleProgressiveLayout(nona$total, sizetype = "area")

dat.gg <- circleLayoutVertices(res, npoints = 100) %>%
  left_join(nona) %>%
  filter(!is.na(x))

labs <- dat.gg %>%
  group_by(Company) %>%
  summarise(x = mean(x),
            y = mean(y),
            amount = mean(total),
            size = mean(total)) %>%
  ungroup() %>%
  arrange(desc(size)) %>%
  mutate(rn = row_number(),
         Company = ifelse(rn < 26, Company, ""),
         amount = ifelse(rn < 26, label_dollar()(amount), ""),
         size = size / max(size))

colors <- met.brewer("Tam")


gg <- ggplot(data = dat.gg) +
  geom_polygon_interactive(
    aes(x, y, group = id, data_id = id,
        tooltip = glue("{Company}\n{label_dollar()(total)}\n"), 
        fill = total, color = total), 
    show.legend = FALSE, size = .05) +
  geom_text_interactive(data = labs, 
                        aes(x = x, y = y, 
                            label = glue("{str_wrap(Company, 15)}\n",
                                         "{amount}"), 
                            size = 10 * size),
                        show.legend = FALSE, color = "white") +
  scale_fill_gradientn(colors = grDevices::colorRampPalette(colors = colors, bias = 1.5)(256)) +
  scale_y_reverse() +
  labs(title = "Donations to Anti-LGBTQ Campaigns from Fortune 500 Companies and Pride Sponsors",
       caption = "Graphic by Spencer Schien (@MrPecners) | Data from Data for Progress") +
  coord_equal() +
  theme_void() +
  theme(plot.title = element_text(size = 16),
        plot.caption = element_text(color = "grey70",
                                    margin = margin(b = 10)))

girafe(
  ggobj = gg, width_svg = 10, height_svg = 10,
  options = list(
    opts_tooltip(
      opacity = .95, offx = 20, offy = 25,
      delay_mouseover = 0, delay_mouseout = 0, use_fill = TRUE,
      css = glue("background-color:gray;color:white;padding:10px;border-radius:2px;",
                 "font-size:24pt;")
    ),
    opts_hover_inv(css = "opacity:0.25"),
    opts_hover(css = "opacity:1")
  )
)

