# RESIDENTIAL ENERGY FORECAST ----

## ----- estimate residential electricity use from floor area energy intensity -----
p_ctu_residential_electricity_forecast <-
  p_ctu_residential_energy_baseline %>%
  filter(var == "kwh_per_floor_area") %>%
  ungroup() %>%
  select(-c(year)) %>%
  bind_rows(p_ctu_characteristics_forecast %>%
              select(-c(year)) %>%
              filter(
                var %in% c(
                  "SFD_Units",
                  "single_family_average_floor_area_sqft_ctu",
                  "MF_Units",
                  "multifamily_average_floor_area_sqft_county"
                )
              )) %>%
  pivot_wider(names_from = "var",
              values_from = "value",
              values_fn = mean) %>%
  mutate(residential_kwh_per_floor_area_forecast = kwh_per_floor_area * 0.8) %>%
  mutate(total_residential_kwh_forecast = (((SFD_Units * single_family_average_floor_area_sqft_ctu) +
                                              (MF_Units * multifamily_average_floor_area_sqft_county)
  ) * residential_kwh_per_floor_area_forecast)) %>%
  mutate(year = 2040) %>%
  select(
    ctu_name,
    year,
    residential_kwh_per_floor_area_forecast,
    total_residential_kwh_forecast
  ) %>%
  pivot_longer(
    cols = c(
      "residential_kwh_per_floor_area_forecast",
      "total_residential_kwh_forecast"
    ),
    names_to = "var"
  )


## ----- estimate residential natural gas use from floor area energy intensity -----
p_residential_natural_gas_forecast_ctu <-
  p_ctu_residential_energy_baseline %>%
  filter(var == "therms_per_floor_area") %>%
  ungroup() %>%
  select(-c(year)) %>%
  bind_rows(., p_ctu_characteristics_forecast %>%
              select(-c(year))) %>%
  dplyr::group_by(ctu_name, var) %>%
  dplyr::distinct() %>%
  pivot_wider(names_from = "var", values_from = "value") %>%
  # assumption that natural gas per floor area stays static
  mutate(residential_therms_per_floor_area_forecast = therms_per_floor_area * 1) %>%
  mutate(
    total_residential_therms_forecast = (((SFD_Units * single_family_average_floor_area_sqft_ctu) +
                                            (MF_Units * multifamily_average_floor_area_sqft_county)
    ) * residential_therms_per_floor_area_forecast)
  ) %>%
  mutate(year = 2040) %>%
  select(
    ctu_name,
    year,
    residential_therms_per_floor_area_forecast,
    total_residential_therms_forecast
  ) %>%
  pivot_longer(
    cols = c(
      "residential_therms_per_floor_area_forecast",
      "total_residential_therms_forecast"
    ),
    names_to = "var"
  )


## ---- compile residential energy use forecast ----
p_ctu_residential_energy_forecast <-
  bind_rows(p_ctu_residential_electricity_forecast,
            p_residential_natural_gas_forecast_ctu)

