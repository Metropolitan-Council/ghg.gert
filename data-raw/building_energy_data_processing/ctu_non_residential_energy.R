# residential ----


p_ctu_nonresidential_energy_forecast %>%
  select(-ctu_name, -value) %>%
  # filter(stringr::str_detect(metric, "_forecast")) %>%
  unique()

# residential_kwh_per_floor_area_forecast
# residential_kwh_forecast

# residential_therms_per_floor_area_forecast
# residential_therms_forecast


p_ctu_nonresidential_energy_baseline %>%
  ungroup() %>%
  select(-ctu_name, -value) %>%
  # filter(stringr::str_detect(metric, "therm")) %>%
  unique()
# residential_mhw





# common variables
"kwh_per_floor_area" = "residential_kwh_per_floor_area_forecast"
"therms_per_floor_area" = "residential_therms_per_floor_area_forecast"

# mutate(residential_mwh = residential_kwh_forecast / 1000 )


ctu_non_residential_energy <-
  p_ctu_nonresidential_energy_baseline  %>%
  pivot_wider(names_from = metric,
              values_from = value) %>%
  bind_rows(
    p_ctu_nonresidential_energy_forecast %>%
      pivot_wider(names_from = metric,
                  values_from = value)
  )
