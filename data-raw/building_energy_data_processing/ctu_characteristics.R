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


# join with energy -----

# kg_co2e_per_mwh


# v_kg_co2e_per_mwh_baseline_bau


ctu_characteristics %>%
  left_join(ctu_residential_energy, c("ctu_name", "year")) %>%
  mutate(residential_floor_area_per_capita = ((
    single_family_average_floor_area_sqft_ctu * single_family_units
  ) + (
    multifamily_average_floor_area_sqft_county * multifamily_units
  )
  ) / population) %>%
  mutate(
    elec_kg_co =
    population *
      residential_floor_area_per_capita *
      kwh_per_floor_area / 1000
  ) %>% View

