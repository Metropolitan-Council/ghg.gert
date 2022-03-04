# non residential forecast

## -------------------------------------------------------------------------------------------
p_nonresidential_energy_forecast_1 <-
  bind_rows(p_ctu_characteristics_forecast,
            p_ctu_nonresidential_energy_baseline) %>%
  select(-c("year")) %>%
  pivot_wider(names_from = "metric", values_from = "value") %>%
  mutate(
    commercial_mwh_forecast = commercial_mwh_per_worker * commercial_emp_forecast,
    commerical_ng_therms_forecast = commercial_ng_therm_per_worker * commercial_emp_forecast,
    industrial_mwh_forecast = industrial_mwh_per_worker * industrial_emp_forecast,
    industrial_ng_therms_forecast = industrial_ng_therm_per_worker * industrial_emp_forecast,
    year = 2040
  ) %>%
  select(
    ctu_name,
    year,
    commercial_mwh_forecast,
    commerical_ng_therms_forecast,
    industrial_mwh_forecast,
    industrial_ng_therms_forecast
  ) %>%
  pivot_longer(
    cols = c(
      "commercial_mwh_forecast",
      "commerical_ng_therms_forecast",
      "industrial_mwh_forecast",
      "industrial_ng_therms_forecast"
    ),
    names_to = "metric"
  )


## -------------------------------------------------------------------------------------------
p_ctu_nonresidential_energy_forecast <-
  bind_rows(
    p_nonresidential_energy_forecast_1
  )


