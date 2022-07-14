# import tables
## -------------------------------------------------------------------------------------------
t_ztrax_sqft_summary_county <- import_from_emissions("metro_energy.ztrax_sqft_summary_county")
t_led_industry_county <- import_from_emissions("metro_demographic.vw_led_industry_county")
t_ctu_population <- import_from_emissions("metro_demographic.vw_ctu_population")
t_ctu_qcew_ctu <- import_from_emissions("metro_demographic.vw_qcew_ctu")
t_forecast_lu_ctu <- import_from_emissions("metro_demographic.vw_forecast_lu_ctu")
t_ztrax_sqft_summary_ctu <- import_from_emissions("metro_energy.vw_ztrax_sqft_summary_ctu")
t_ctu_county <- import_from_emissions("metro_demographic.vw_ctu_county")

# baseline demographics
## -------------------------------------------------------------------------------------------
p_county_average_floor_area_single_family <-
  t_ztrax_sqft_summary_county %>%
  select(co_name, property_land_use, mean_sqft) %>%
  filter(property_land_use == "Single family residential") %>%
  # filter(co_name == p_county)  %>%
  group_by(co_name) %>%
  mutate(
    metric = "single_family_average_floor_area_sqft_county",
    year = 2018
  ) %>%
  rename(value = mean_sqft) %>%
  select(co_name, year, metric, value)

## -------------------------------------------------------------------------------------------
p_county_average_floor_area_multifamily <-
  t_ztrax_sqft_summary_county %>%
  select(co_name, property_land_use, mean_sqft) %>%
  filter(property_land_use == "Condominium") %>%
  mutate(
    metric = "multifamily_average_floor_area_sqft_county",
    year = 2018
  ) %>%
  rename(value = mean_sqft) %>%
  select(co_name, year, metric, value)

## -------------------------------------------------------------------------------------------
p_county_workers <-
  t_led_industry_county %>%
  # filter(co_name == p_county) %>%
  filter(year == 2018) %>%
  filter(jw_indicator == "W") %>%
  mutate(
    metric =
      case_when(
        (industry %in% naics_codes$led_commercial) ~ "commercial_workers_county",
        (industry %in% naics_codes$led_industrial) ~ "industrial_workers_county"
      )
  ) %>%
  select(co_name, year, metric, count) %>%
  group_by(co_name, year, metric) %>%
  summarise(value = sum(count, na.rm = T), .groups = "keep")

## -------------------------------------------------------------------------------------------
p_county_characteristics <-
  bind_rows(
    p_county_average_floor_area_single_family,
    p_county_average_floor_area_multifamily,
    p_county_workers
  )


# CTU ----

## -------------------------------------------------------------------------------------------
# TODO resolve CTUs that fall in multiple counties
p_ctu_population <-
  t_ctu_population %>%
  select(ctu_name, year, population, households) %>%
  filter(year == 2018) %>%
  group_by(ctu_name, year) %>%
  pivot_longer(
    cols = c("population", "households"),
    names_to = "metric",
    values_to = "value"
  ) %>%
  group_by(ctu_name, year, metric) %>%
  summarise(value = sum(value, na.rm = T), .groups = "keep")


## -------------------------------------------------------------------------------------------
p_ctu_jobs <-
  t_ctu_qcew_ctu %>%
  filter(naicstitle == "Total, All Industries") %>%
  filter(year == 2018) %>%
  select(ctu_name, year, emp) %>%
  rename(value = emp) %>%
  mutate(metric = "total_jobs") %>%
  select(ctu_name, year, metric, value)


## -------------------------------------------------------------------------------------------
p_ctu_industrial_jobs <-
  t_ctu_qcew_ctu %>%
  filter(
    naicstitle %in% c(
      "Natural Resources and Mining",
      "Construction",
      "Trade, Transportation and Utilities"
    )
  ) %>%
  filter(year == 2018) %>%
  select(ctu_name, year, naicstitle, emp) %>%
  group_by(ctu_name, year) %>%
  summarise(value = sum(emp, na.rm = TRUE), .groups = "keep") %>%
  mutate(metric = "industrial_jobs") %>%
  select(ctu_name, year, metric, value)


## -------------------------------------------------------------------------------------------
p_ctu_commercial_jobs <-
  t_ctu_qcew_ctu %>%
  filter(
    naicstitle %in% c(
      "Financial Activities",
      "Professional and Business Services",
      "Education and Health Services",
      "Leisure and Hospitality",
      "Public Administration"
    )
  ) %>%
  filter(year == 2018) %>%
  select(ctu_name, year, naicstitle, emp) %>%
  group_by(ctu_name, year) %>%
  summarise(value = sum(emp, na.rm = TRUE), .groups = "keep") %>%
  mutate(metric = "commercial_jobs") %>%
  select(ctu_name, year, metric, value)


## -------------------------------------------------------------------------------------------
p_ctu_housing_stock <-
  t_forecast_lu_ctu %>%
  filter(metric %in% c("SFD_Units", "MF_Units")) %>%
  filter(year == 2018) %>%
  group_by(ctu_name, year)


## -------------------------------------------------------------------------------------------
p_ctu_average_floor_area_single_family <-
  t_ztrax_sqft_summary_ctu %>%
  select(ctu_name, property_land_use, mean_sqft) %>%
  unique() %>%
  filter(property_land_use == "Single family residential") %>%
  group_by(ctu_name) %>%
  mutate(
    metric = "single_family_average_floor_area_sqft_ctu",
    year = 2018,
    value = mean(mean_sqft, na.rm = T)
  ) %>%
  select(ctu_name, year, metric, value)


## -------------------------------------------------------------------------------------------
p_ctu_average_floor_area_multifamily <-
  t_ztrax_sqft_summary_ctu %>%
  select(ctu_name, property_land_use, mean_sqft) %>%
  filter(stringr::str_detect(property_land_use, "Condominium")) %>%
  mutate(
    metric = "multifamily_average_floor_area_sqft_ctu",
    year = 2018
  ) %>%
  rename(value = mean_sqft) %>%
  select(ctu_name, year, metric, value)


## -------------------------------------------------------------------------------------------

## ----------
# TODO resolve CTUs that fall in more than one county
p_ctu_county <- p_county_characteristics %>%
  left_join(t_ctu_county, by = "co_name") %>%
  filter(metric == "multifamily_average_floor_area_sqft_county") %>%
  group_by(ctu_name, year, metric) %>%
  select(ctu_name, year, metric, value) %>%
  dplyr::group_by(ctu_name, year, metric) %>%
  dplyr::summarise(value = mean(value), .groups = "keep")


#----------
p_ctu_characteristics <-
  bind_rows(
    p_ctu_population,
    p_ctu_jobs,
    p_ctu_commercial_jobs,
    p_ctu_industrial_jobs,
    p_ctu_housing_stock,
    p_ctu_average_floor_area_single_family,
    p_ctu_average_floor_area_multifamily,
    p_ctu_county
  ) %>%
  unique()
