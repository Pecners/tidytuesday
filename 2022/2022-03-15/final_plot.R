library(tidyverse)
library(lubridate)
library(glue)
library(showtext)
library(ggtext)
library(zoo)

# Load data

data <- tidytuesdayR::tt_load("2022-03-15")
cran <- data$cran
#bioc <- data$bioc
rm(data)

# Clean Dates

cran_dated <- cran %>%
  filter(date != "0") %>%
  mutate(clean_date = case_when(!is.na(ymd(str_extract(date, "^.{10}"))) ~ str_extract(date, "^.{10}"),
                                TRUE ~ str_remove(
                                  str_remove(date, "\\d{2}\\:.*(?= )"),
                                  "^.{4}"
                                )),
         done_date = case_when(str_detect(clean_date, "-") ~ ymd(clean_date),
                               TRUE ~ mdy(clean_date)))

# Check for missing dates

cran %>%
  group_by(none = date == "0") %>%
  tally() %>%
  ungroup() %>%
  mutate(perc = n / sum(n))

# Calculate daily totals

cran_tallied <- cran_dated %>%
  arrange(done_date) %>%
  mutate(dummie = 1) %>%
  filter(year(done_date) > "2000")

daily_subs <- cran_tallied %>%
  group_by(date = done_date) %>%
  summarise(n = sum(dummie)) %>%
  filter(n < 500) %>%
  group_by(year = year(date)) %>%
  mutate(is_max = ifelse(n == max(n), TRUE, FALSE))

# Set up branding

light_blue <- "#B5C2E2"
blue <- "#09689B"

font_add_google("IBM Plex Sans Condensed", "ibm")
showtext_auto()

# Set up annotations 

annos <- c(ymd("2007-06-01"),
           ymd("2009-11-09"),
           ymd("2014-01-16"),
           ymd("2016-09-09"),
           ymd("2020-03-11"))

anno_labs <- c("First **ggplot2**<br>release",
               "First **stringr**<br>release",
               "First **dplyr**<br>release",
               "First **tidyverse**<br>release",
               "WHO declares<br>covid-19 pandemic")
lab_hjust = c(0, 0, 1, 1, 1)

labs_df <- tibble(annos = annos, 
                  anno_labs = glue("<span style='color:black'>{anno_labs}</span>"),
                  lab_hjust = lab_hjust)




daily_subs %>%
  ggplot(aes(date, n)) +
  geom_segment(data = labs_df, aes(x = annos, xend = annos, y = -20, yend = 100),
             linetype = 2, alpha = .5) +
  geom_richtext(data = labs_df, family = "ibm", size = 2.5,
                fill = NA, color = NA,
                aes(x = annos, label = anno_labs,y = -15,
                    hjust = lab_hjust)) +
  annotate(geom = "text", x = ymd("2012-06-01"), y = 51, size = 2.5,
           label = "Two-week\nmoving average", hjust = 0, vjust = 0,
           lineheight = .9, family = "ibm", color = "grey40") +
  geom_point(color = light_blue) +
  geom_line(aes(y = rollmean(n, 14, na.pad = TRUE)),
            color = blue, size = .75) +
  annotate(geom = "curve", x = ymd("2012-05-01"), xend = ymd("2011-03-01"),
             y = 53, yend = 20, arrow = arrow(length = unit(.1, "cm")),
           color = "grey40") +
  scale_y_continuous(limits = c(-20, 100), breaks = c(0, 50, 100), 
                     position = "right") +
  scale_x_date(limits = c(ymd("2005-01-01", Inf)), expand = c(.02, 0)) +
  theme_minimal() +
  theme(text = element_text(family = "ibm"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.caption = element_textbox_simple(width = unit(8.5, "in"), hjust = 0,
                                       lineheight = 1.25, size = 9,
                                       color = "grey40")) +
  labs(title = "Daily package releases on the Comprehensive R Archive Network (CRAN)",
       y = "", x = "",
       caption = glue("Data shown spans from Jan. 1st, 2005 to Oct. 9, 2021. Two dates were ",
                      "removed: Jan. 7th, 2020 (123 total submissions) is clipped from the plot ",
                      "but included in the moving average calculation, whereas ",
                      "Oct. 29th, 2012 (551 total submissions) is omitted entirely. ",
                      "About 2.5% of releases were omitted because they lacked dates. ",
                      "Analysis and graphic by Spencer Schien (@MrPecners), data from ",
                      "Robert Flight."))

ggsave(filename = "2022/2022-03-15/final_plot.png", device = "png", bg = "white",
       width = 9, h = 4)

