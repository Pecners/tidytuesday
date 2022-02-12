library(tidyverse)
library(tidygeocoder)
library(ggmap)

# Load Data
data <- tidytuesdayR::tt_load("2022-02-08")
airman <- data$airmen

#m <- airman %>%
#  filter(state %in% missing)

airman <- airman %>%
  mutate(state = case_when(state == "KN" ~ "KY",
                           state == "CN" ~ "CT",
                           #state == "VI" ~ "VA",
                           is.na(state) ~ "Unknown",
                           state == "Unk" ~ "Unknown",
                           state == "Haiti" ~ "HT",
                           TRUE ~ state)) 


geo_a <- airman %>%
  rename(m = military_hometown_of_record) %>%
  # Fix town names for geocoding
  mutate(m = case_when(m == "Gadston" ~ "Gadsden",
                       m == "Puntagorda" ~ "Punta Gorda",
                       m == "Cincinnatti" ~ "Cincinnati",
                       m == "Ashville" ~ "Asheville",
                       m == "Bridgetown" ~ "Bridgeton",
                       TRUE ~ m)) %>%
  filter(!is.na(m) & !is.na(state)) %>%
  geocode(city = m, state = state)

geo_a_handed <- geo_a %>%
  # Easier to enter coords by hand for these because
  # Nominatim API wasn't working for them
  mutate(lat = case_when(state == "HT" ~ 18.533333,
                         m == "Lincoln Univ" ~ 39.808333, 
                         m == "Mars" ~40.696667,
                         m == "St. Croix" ~ 17.733509, 
                         m == "Christiansted" ~ 17.75,
                         m == "Port of Spain" ~ 10.666667,
                         m == "Williams AFB" ~ 33.30898,
                         m == "Tuskegee Inst." ~ 32.43078,
                         TRUE ~ lat),
         long = case_when(state == "HT" ~ -72.333333,
                          m == "Lincoln Univ" ~ -75.927778,
                          m == "Mars" ~ -80.012222,
                          m == "St. Croix" ~ -64.783864,
                          m == "Christiansted" ~  -64.75,
                          m == "Port of Spain" ~ -61.516667,
                          m == "Williams AFB" ~ -111.6559,
                          m == "Tuskegee Inst." ~ -85.70733,
                          TRUE ~ long))


no_a <- geo_a_handed %>%
  filter(is.na(long)) %>%
  select(-c(lat, long)) 


# geocode(city = military_hometown_of_record, state = state)

# I'm using the Google API here, which requires you
# to enter your API key. I don't use Google for everything
# because of limits.

n <- map_df(1:nrow(no_a), function(x) {
  no_a[x,] %>%
    mutate(p = paste(m, state, sep = ", ")) %>%
    mutate_geocode(., location = p) 
})

worked_first <- geo_a_handed %>%
  filter(!is.na(lat)) %>%
  mutate(p = paste(m, state, sep = ", "))

worked_second <- n %>%
  rename(long = lon)

together <- bind_rows(worked_first, worked_second)

with_orig <- together %>%
  filter(!is.na(lat))  %>%
  bind_rows(., airman %>%
              filter(!name %in% together$name) %>%
              rename(m = military_hometown_of_record))

final <- with_orig %>%
  mutate(lat = case_when(state == "HT" ~ 18.533333,
                         m == "Lincoln Univ" ~ 39.808333, 
                         m == "Mars" ~40.696667,
                         m == "St. Croix" ~ 17.733509, 
                         m == "Christiansted" ~ 17.75,
                         m == "Port of Spain" ~ 10.666667,
                         m == "Williams AFB, AZ" ~ 33.30898,
                         m == "Tuskegee Inst., AL" ~ 32.43078,
                         state == "Unknown" ~ 31.423331,
                         TRUE ~ lat),
         long = case_when(state == "HT" ~ -72.333333,
                          m == "Lincoln Univ" ~ -75.927778,
                          m == "Mars" ~ -80.012222,
                          m == "St. Croix" ~ -64.783864,
                          m == "Christiansted" ~  -64.75,
                          m == "Port of Spain" ~ -61.516667,
                          m == "Williams AFB, AZ" ~ -111.6559,
                          m == "Tuskegee Inst., AL" ~ -85.70733,
                          state == "Unknown" ~ -74.752245,
                          TRUE ~ long))

# Make sure everything is geocoded
# should return 0 x 2 tibble

final %>%
  filter(is.na(lat)) %>%
  select(m, state)

saveRDS(final, "2022/2022-02-08/geocoded_hometowns.rda")
