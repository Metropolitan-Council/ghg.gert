ctu_residential_energy <-
  p_ctu_residential_energy_forecast %>%
  pivot_wider(
    names_from = var,
    values_from = value
  ) %>%
  select(ctu_name, year,
    "kwh_per_floor_area" = "residential_kwh_per_floor_area_forecast",
    "therms_per_floor_area" = "residential_therms_per_floor_area_forecast"
  ) %>%
  bind_rows(
    p_ctu_residential_energy_baseline %>%
      pivot_wider(
        names_from = var,
        values_from = value
      ) %>%
      select(ctu_name, year, kwh_per_floor_area, therms_per_floor_area)
  )
