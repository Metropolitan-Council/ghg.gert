# non residential forecast

## -------------------------------------------------------------------------------------------
p_nonresidential_energy_forecast_1 <-
  bind_rows(p_ctu_characteristics_forecast,
            p_ctu_nonresidential_energy_baseline) %>%
  select(-c("year")) %>%
  pivot_wider(names_from = "metric", values_from = "value") %>%
  rowwise() %>%
  mutate(
    commercial_mwh = commercial_mwh_per_worker * commercial_emp_forecast,
    commerical_therms = commercial_therm_per_worker * commercial_emp_forecast,
    industrial_mwh = industrial_mwh_per_worker * industrial_emp_forecast,
    industrial_therms = industrial_therm_per_worker * industrial_emp_forecast,
    year = 2040
  ) %>%
  select(
    ctu_name,
    year,
    commercial_mwh,
    commerical_therms,
    industrial_mwh,
    industrial_therms
  ) %>%
  pivot_longer(
    cols = c(
      "commercial_mwh",
      "commerical_therms",
      "industrial_mwh",
      "industrial_therms"
    ),
    names_to = "metric"
  )


## -------------------------------------------------------------------------------------------
p_ctu_nonresidential_energy_forecast <-
  bind_rows(
    p_nonresidential_energy_forecast_1
  )


