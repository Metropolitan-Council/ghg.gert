# residential ----

p_ctu_nonresidential_energy_forecast %>%
  select(-ctu_name, -value) %>%
  # filter(stringr::str_detect(var, "_forecast")) %>%
  unique()

p_ctu_nonresidential_energy_baseline %>%
  ungroup() %>%
  select(-ctu_name, -value) %>%
  # filter(stringr::str_detect(var, "therm")) %>%
  unique()

# common variables
"kwh_per_floor_area" <- "residential_kwh_per_floor_area_forecast"
"therms_per_floor_area" <- "residential_therms_per_floor_area_forecast"

ctu_non_residential_energy <-
  p_ctu_nonresidential_energy_baseline %>%
  pivot_wider(
    names_from = var,
    values_from = value
  ) %>%
  bind_rows(
    p_ctu_nonresidential_energy_forecast %>%
      pivot_wider(
        names_from = var,
        values_from = value
      )
  )
