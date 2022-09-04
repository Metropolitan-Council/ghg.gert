# COUNTY DEMOGRAPHIC BASELINE ----

## ----- get average floor area for single family from ZTRAX in 'Emissions' -----
p_county_average_floor_area_single_family <-
  t_ztrax_sqft_summary_county %>%
  select(co_name, property_land_use, mean_sqft) %>%
  filter(property_land_use == "Single family residential") %>%
  # filter(co_name == p_county)  %>%
  group_by(co_name) %>%
  mutate(
    var = "single_family_average_floor_area_sqft_county",
    year = 2018
  ) %>%
  rename(value = mean_sqft) %>%
  select(co_name, year, var, value)

## ----- get average multifamily floor area per county from ZTRAX in 'Emissions' ----
p_county_average_floor_area_multifamily <-
  t_ztrax_sqft_summary_county %>%
  select(co_name, property_land_use, mean_sqft) %>%
  filter(property_land_use == "Condominium") %>%
  mutate(
    var = "multifamily_average_floor_area_sqft_county",
    year = 2018
  ) %>%
  rename(value = mean_sqft) %>%
  select(co_name, year, var, value)

## ----- obtain commercial/industrial workers from 'Emissions' ----
p_county_workers <-
  t_led_industry_county %>%
  # filter(co_name == p_county) %>%
  filter(year == 2018) %>%
  filter(jw_indicator == "W") %>%
  mutate(
    var =
      case_when(
        (industry %in% naics_codes$led_commercial) ~ "commercial_workers_county",
        (industry %in% naics_codes$led_industrial) ~ "industrial_workers_county"
      )
  ) %>%
  select(co_name, year, var, count) %>%
  group_by(co_name, year, var) %>%
  summarise(value = sum(count, na.rm = T), .groups = "keep")

## ---- compile county baseline demographic characteristic -----
p_county_characteristics <-
  bind_rows(
    p_county_average_floor_area_single_family,
    p_county_average_floor_area_multifamily,
    p_county_workers
  )
