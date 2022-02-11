geo_a <- airman %>%
  filter(!is.na(military_hometown_of_record) & !is.na(state)) %>%
  geocode(city = military_hometown_of_record, state = state)

no_a <- geo_a %>%
  filter(is.na(long)) %>%
  select(-c(lat, long)) %>%
  rename(m = military_hometown_of_record) %>%
  mutate(m = case_when(m == "Gadston" ~ "Gadsden",
                       m == "Puntagorda" ~ "Punta Gorda",
                       m == "Cincinnatti" ~ "Cincinnati",
                       m == "Ashville" ~ "Asheville",
                       m == "Bridgetown" ~ "Bridgeton",))
geocode(city = military_hometown_of_record, state = state)

n <- no_a %>%
  mutate(p = paste(m, state, sep = ", ")) %>%
  mutate_geocode(., location = p)

nn <- n %>%
  filter(is.na(lon)) %>%
  select(-c(lon, lat)) %>%
  mutate_geocode(., location = p)

n3 <- nn %>%
  filter(is.na(lon)) %>%
  select(-c(lon, lat)) %>%
  mutate_geocode(., location = p)

n4 <- n3 %>%
  filter(is.na(lon)) %>%
  select(-c(lon, lat)) %>%
  mutate_geocode(., location = p)

n5 <- n4 %>%
  filter(is.na(lon)) %>%
  select(-c(lon, lat)) %>%
  mutate_geocode(., location = p)

n6 <- n5 %>%
  filter(is.na(lon) & !p %in% c("Williams AFB, AZ", "Tuskegee Inst., AL")) %>%
  select(-c(lon, lat)) %>%
  mutate_geocode(., location = p)

n_list <- list(n, nn, n3, n4, n5, n6)

n_df <- map_df(n_list, function(x) {
  x %>%
    select(m, state,
           long = lon, lat = lat) %>%
    filter(!is.na(lat))
}) %>%
  unique()

added <- left_join(no_a, n_df)

together <- geo_a %>%
  rename(m = military_hometown_of_record) %>%
  filter(!is.na(lat))  %>%
  bind_rows(., added) %>%
  bind_rows(., airman %>%
              filter(!name %in% together$name) %>%
              rename(m = military_hometown_of_record))

final <- together %>%
  mutate(lat = case_when(state == "HT" ~ 18.533333,
                         m == "Lincoln Univ" ~ 39.808333, 
                         m == "Mars" ~40.696667,
                         m == "St. Croix" ~ 17.733509, 
                         m == "Christiansted" ~ 17.75,
                         m == "Port of Spain" ~ 10.666667,
                         TRUE ~ lat),
         long = case_when(state == "HT" ~ -72.333333,
                          m == "Lincoln Univ" ~ -75.927778,
                          m == "Mars" ~ -80.012222,
                          m == "St. Croix" ~ -64.783864,
                          m == "Christiansted" ~  -64.75,
                          m == "Port of Spain" ~ -61.516667,
                          TRUE ~ long))

final %>%
  filter(is.na(lat)) %>%
  select(m, state)

saveRDS(final, "2022/2022-02-08/geocoded_hometowns.rda")

