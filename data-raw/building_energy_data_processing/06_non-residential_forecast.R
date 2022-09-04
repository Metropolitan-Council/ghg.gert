# NREL data: used in instances where there is not enough data to calculate using employment energy intensity
p_nrel_nonresidential_energy_forecast <-
  t_nrel_energy_consumption_ctu %>%
  mutate(unit = case_when(source == "elec" ~ "mwh_nrel",
                          source == "ng" ~ "therms_nrel")) %>%
  mutate(value = case_when(source == "elec" ~ consumption_mmbtu * 0.293071,
                           source == "ng" ~ consumption_mmbtu * 10)) %>%
         unite("var", c(sector, unit), remove = FALSE) %>%
  filter(year %in% c(2040)) %>%
  select(ctu_name, year, var, value)

# non residential forecast
## -------------------------------------------------------------------------------------------
p_ctu_nonresidential_energy_forecast <-
  bind_rows(
    p_ctu_characteristics_forecast,
    p_ctu_nonresidential_energy_baseline,
    p_nrel_nonresidential_energy_forecast
  ) %>%
  select(-c("year")) %>%
  distinct() %>%
  pivot_wider(names_from = "var", values_from = "value") %>%
  rowwise() %>%
  mutate(
    commercial_mwh = if_else(is.na(commercial_mwh_per_worker * commercial_emp_forecast)==FALSE, commercial_mwh_per_worker * commercial_emp_forecast, commercial_mwh_nrel),
    commercial_therms = if_else(is.na(commercial_therm_per_worker * commercial_emp_forecast)==FALSE, commercial_therm_per_worker * commercial_emp_forecast, commercial_therms_nrel),
    industrial_mwh = if_else(is.na(industrial_mwh_per_worker * industrial_emp_forecast)==FALSE, industrial_mwh_per_worker * industrial_emp_forecast, industrial_mwh_nrel),
    industrial_therms = if_else(is.na(industrial_therm_per_worker * industrial_emp_forecast)==FALSE, industrial_therm_per_worker * industrial_emp_forecast, industrial_therms_nrel),
    year = 2040
  ) %>%
  select(
    ctu_name,
    year,
    commercial_mwh,
    commercial_therms,
    industrial_mwh,
    industrial_therms,
    industrial_therm_per_worker,
    industrial_mwh_per_worker,
    commercial_therm_per_worker,
    commercial_mwh_per_worker) %>%
  pivot_longer(
    cols = c(
      "commercial_mwh",
      "commercial_therms",
      "industrial_mwh",
      "industrial_therms",
      "industrial_therm_per_worker",
      "industrial_mwh_per_worker",
      "commercial_therm_per_worker",
      "commercial_mwh_per_worker"
    ),
    names_to = "var"
  )
