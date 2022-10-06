## -------------------------------------------------------------------------------------------
p_baseline <-
  bind_rows(
    p_ctu_characteristics,
    p_ctu_residential_energy_baseline,
    p_ctu_nonresidential_energy_baseline
  ) %>% unique()

## -------------------------------------------------------------------------------------------
p_forecast <-
  bind_rows(
    p_ctu_characteristics_forecast,
    p_ctu_residential_energy_forecast,
    p_ctu_nonresidential_energy_forecast
  ) %>%
  unique()


## -------------------------------------------------------------------------------------------
# rm(list=setdiff(ls(), c("p_baseline", "p_forecast")))


## -------------------------------------------------------------------------------------------
v_kg_co2e_per_mwh_baseline_bau <- 566.4
v_kg_co2e_per_mwh_forecast_bau <- 566.4

v_kg_co2e_per_therm_baseline_bau <- 5.31
v_kg_co2e_per_therm_forecast_bau <- 5.31


## -------------------------------------------------------------------------------------------
p_baseline_fin <-
  p_baseline %>%
  pivot_wider(names_from = "metric", values_from = "value") %>%
  mutate(residential_floor_area_per_capita = ((
    single_family_average_floor_area_sqft_ctu * single_family_units
  ) + (
    multifamily_average_floor_area_sqft_county * multifamily_units
  )
  ) / population) %>%
  # electricity
  mutate(
    electricity_emissions_kg_co2e =
      ((
        population *
          residential_floor_area_per_capita *
          kwh_per_floor_area / 1000
      ) +
        (commercial_jobs * commercial_mwh_per_worker) +
        (industrial_jobs * industrial_mwh_per_worker)
      ) * v_kg_co2e_per_mwh_baseline_bau
  ) %>%
  # natural gas
  mutate(
    natural_gas_emissions_kg_co2e =
      ((
        population * residential_floor_area_per_capita * therms_per_floor_area
      ) +
        (commercial_jobs * commercial_ng_therm_per_worker) +
        (industrial_jobs * industrial_ng_therm_per_worker)
      ) * v_kg_co2e_per_therm_baseline_bau
  )


## -------------------------------------------------------------------------------------------
p_forecast %>%
  dplyr::group_by(ctu_name, year, metric) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)

p_forecast_fin <-
  p_forecast %>%
  pivot_wider(names_from = "metric", values_from = "value") %>%
  mutate(residential_floor_area_per_capita = ((
    single_family_average_floor_area_sqft_ctu * single_family_units
  ) + (
    multifamily_average_floor_area_sqft_county * multifamily_units
  )
  ) / population) %>%
  # electricity
  mutate(
    electricity_emissions_kg_co2e =
      ((total_residential_kwh_forecast / 1000) +
        ((commercial_mwh_forecast +
          industrial_mwh_forecast) / 1000
        )) * v_kg_co2e_per_mwh_forecast_bau
  ) %>%
  # natural gas
  mutate(
    natural_gas_emissions_kg_co2e =
      ((total_residential_therms_forecast) +
        (
          commercial_ng_therms_forecast +
            industrial_ng_therms_forecast
        )
      ) * v_kg_co2e_per_therm_forecast_bau
  )



elmo_comp_baseline <- readRDS("data-raw/building_energy_data_processing/lake_elmo_p_baseline.RDS")
elmo_comp_forecast <- readRDS("data-raw/building_energy_data_processing/lake_elmo_p_forecast.RDS")

arsenal::comparedf(
  elmo_comp_forecast,
  p_forecast_fin %>%
    filter(ctu_name == "Lake Elmo")
) %>%
  summary()

arsenal::comparedf(
  elmo_comp_baseline,
  p_baseline_fin %>%
    filter(ctu_name == "Lake Elmo")
) %>%
  summary()


names(p_forecast_fin) <- names(p_forecast_fin) %>%
  stringr::str_remove("_forecast")

building_data <- bind_rows(
  p_baseline_fin,
  p_forecast_fin
) %>%
  mutate(
    kg_co2e_per_floor_area =
      (residential_kwh_per_floor_area_forecast *
        v_kg_co2e_per_mwh_forecast_bau / 1000) +
        residential_therms_per_floor_area_forecast * v_kg_co2e_per_therm_forecast_bau
  ) %>%
  group_by(ctu_name, year) %>%
  pivot_longer(
    3:44,
    names_to = "var",
    values_to = "value"
  )


usethis::use_data(building_data, overwrite = T)
