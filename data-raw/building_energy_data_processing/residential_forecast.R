# import tables
## -------------------------------------------------------------------------------------------



# residential forecast
## -------------------------------------------------------------------------------------------
p_ctu_residential_electricity_forecast <-
  p_ctu_residential_energy_baseline %>%
  filter(metric == "kwh_per_floor_area") %>%
  ungroup() %>%
  select(-c(year)) %>%
  bind_rows(p_ctu_characteristics_forecast %>%
    select(-c(year)) %>%
    filter(metric %in% c(
      "SFD_Units",
      "single_family_average_floor_area_sqft_ctu",
      "MF_Units",
      "multifamily_average_floor_area_sqft_county"
    ))) %>%
  pivot_wider(names_from = "metric", values_from = "value") %>%
  mutate(residential_kwh_per_floor_area_forecast = kwh_per_floor_area * 0.8) %>%
  mutate(total_residential_kwh_forecast = (((
    SFD_Units * single_family_average_floor_area_sqft_ctu
  ) + (
    MF_Units * multifamily_average_floor_area_sqft_county
  )
  ) * residential_kwh_per_floor_area_forecast)) %>%
  mutate(year = 2040) %>%
  select(
    ctu_name, year,
    residential_kwh_per_floor_area_forecast, total_residential_kwh_forecast
  ) %>%
  pivot_longer(cols = c(
    "residential_kwh_per_floor_area_forecast",
    "total_residential_kwh_forecast"
  ), names_to = "metric")


## -------------------------------------------------------------------------------------------
p_residential_natural_gas_forecast_ctu <-
  p_ctu_residential_energy_baseline %>%
  filter(metric == "therms_per_floor_area") %>%
  ungroup() %>%
  select(-c(year)) %>%
  bind_rows(., p_ctu_characteristics_forecast %>%
    select(-c(year))) %>%
  pivot_wider(names_from = "metric", values_from = "value") %>%
  # assumption that natural gas per floor area stays static
  mutate(residential_therms_per_floor_area_forecast = therms_per_floor_area * 1) %>%
  mutate(total_residential_therms_forecast = (((
    single_family_units * single_family_average_floor_area_sqft_ctu
  ) + (
    multifamily_units * multifamily_average_floor_area_sqft_county
  )
  ) * residential_therms_per_floor_area_forecast)) %>%
  mutate(year = 2040) %>%
  select(ctu_name, year, residential_therms_per_floor_area_forecast, total_residential_therms_forecast) %>%
  pivot_longer(cols = c("residential_therms_per_floor_area_forecast", "total_residential_therms_forecast"), names_to = "metric")


## -------------------------------------------------------------------------------------------
p_ctu_residential_energy_forecast <-
  bind_rows(
    p_ctu_residential_electricity_forecast,
    p_residential_natural_gas_forecast_ctu
  )
