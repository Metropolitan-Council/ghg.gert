# ctu_residential_characteristics


p_ctu_characteristics %>%
  ungroup() %>%
  select(-ctu_name, -year, -value) %>%
  # filter(stringr::str_detect(metric, "_forecast")) %>%
  unique()


p_ctu_characteristics_forecast %>%
  select(-ctu_name, -year, -value) %>%
  as_tibble() %>%
  # filter(stringr::str_detect(metric, "_forecast")) %>%
  unique()


ctu_characteristics <- p_ctu_characteristics_forecast %>%
  pivot_wider(names_from = metric,
              values_from = value) %>%
  select(ctu_name,
         year,
         population,
         households,
         total_jobs = jobs,
         commercial_jobs = commercial_emp_forecast,
         industrial_jobs = industrial_emp_forecast,
         everything()) %>%
  bind_rows(p_ctu_characteristics %>%
              pivot_wider(names_from = metric,
                          values_from = value))


# join with residential -----

# kg_co2e_per_mwh


# v_kg_co2e_per_mwh_baseline_bau


ctu_char_emission <- ctu_characteristics %>%
  left_join(ctu_residential_energy, c("ctu_name", "year")) %>%
  mutate(residential_floor_area_per_capita = (
    (single_family_average_floor_area_sqft_ctu * single_family_units) +
      (multifamily_average_floor_area_sqft_county * multifamily_units))
    / population) %>%
  mutate(
    residential_mwh = population *
      residential_floor_area_per_capita *
      (kwh_per_floor_area / 1000),
    residential_electricity_emissions_kg_co =
      residential_mwh  * enviro_factors$KG_CO2E_PER_MHW_BASELINE
  ) %>%
  mutate(
    residential_therms = population * residential_floor_area_per_capita * therms_per_floor_area,
    residential_natural_gas_emissions_kg_co =
      residential_therms * enviro_factors$KG_CO2E_PER_THERM_BASELINE )

filter(ctu_char_emission, is.na(residential_floor_area_per_capita))

## join with non-residential energy -----


ctu_char_emission %>%
  left_join(ctu_non_residential_energy,
            by = c("ctu_name", "year")) %>%
  mutate(non_residential_mwh = commercial_mwh +
           industrial_mwh,
         non_residential_electricity_emissions_kg_co = non_residential_mwh * enviro_factors$KG_CO2E_PER_MHW_BASELINE) %>%
  mutate(non_residential_therms = commercial_therms +
           industrial_therms,
         non_residential_natural_gas_emissions_kg_co = non_residential_therms * enviro_factors$KG_CO2E_PER_THERM_BASELINE) %>%
  arrange(ctu_name)
  group_by(ctu_name, year) %>%
  pivot_longer(cols = 3:32,
               names_to = "metric")
