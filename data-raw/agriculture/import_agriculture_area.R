library(dplyr)
devtools::load_all(".")

lc <- bind_rows(
  natural_systems_data$inventory$ctu,
  natural_systems_data$projections$ctu,
  natural_systems_data$inventory$county,
  natural_systems_data$projections$county
)

agriculture_area <- lc %>%
  group_by(geog_name, geog_id, inventory_year) %>%
  mutate(percent_area = area / sum(area)) %>%
  ungroup() %>%
  filter(
    land_cover_type == "Cropland",
    inventory_year >= 2005
  ) %>%
  select(-potential_wetland_area)

usethis::use_data(agriculture_area, overwrite = T)
