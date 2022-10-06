# CTU DEMOGRAPHIC FORECAST -----

## ----get forecast population from 'Emissions' -----
p_ctu_population_forecast <-
  t_ctu_forecast %>%
  filter(year == 2040)


## ---- get industrial/commercial workers forecast from 'Emissions' -----
p_ctu_emp_forecast <-
  t_emp_forecast_industry_ctu %>%
  filter(year == 2040) %>%
  group_by(ctu_name, year, indlabel) %>%
  mutate(
    var =
      case_when(
        (indlabel %in% naics_codes$industrial ~ "industrial_emp_forecast"),
        (indlabel %in% naics_codes$commercial ~ "commercial_emp_forecast")
      )
  ) %>%
  group_by(ctu_name, year, var) %>%
  summarise(value = sum(emp, na.rm = T), .groups = "keep")


## ---- get housing stock forecast from 'Emissions' ----
p_housing_stock_ctu_forecast <- t_forecast_lu_ctu %>%
  filter(
    year == 2040,
    ctu_name %in% unique(t_ctu_forecast$ctu_name),
    var %in% (c("SFD_Units", "MF_Units"))
  )


## ---- estimate average growth of building area for single family ----
p_ctu_average_floor_area_single_family_forecast <-
  p_ctu_average_floor_area_single_family %>%
  left_join(t_ctu_county, by = "ctu_name") %>%
  left_join(p_county_average_annual_growth_single_family_sqft, by = "co_name") %>%
  select(-co_name) %>%
  group_by(ctu_name) %>%
  mutate(mean_growth_rate = mean(mean_growth_rate, na.rm = T)) %>%
  unique() %>%
  group_by(ctu_name) %>%
  mutate(
    value =
      case_when(
        mean_growth_rate * (2040 - 2018) > 0.15 ~ value + (value * 0.15),
        mean_growth_rate * (2040 - 2018) < 0.15 ~ value + (value * mean_growth_rate)
      ),
    year = 2040
  ) %>%
  unique() %>%
  select(ctu_name, year, var, value)


## -----multifamily floor area growth --------------------------------------------------------
p_ctu_average_floor_area_multifamily_forecast <-
  p_ctu_average_floor_area_multifamily %>%
  left_join(t_ctu_county, by = "ctu_name") %>%
  left_join(p_county_average_annual_growth_multifamily_sqft, by = "co_name") %>%
  select(-co_name) %>%
  group_by(ctu_name) %>%
  mutate(mean_growth_rate = mean(mean_growth_rate, na.rm = T)) %>%
  unique() %>%
  group_by(ctu_name) %>%
  mutate(
    value =
      case_when(
        mean_growth_rate * (2040 - 2018) > 0.15 ~ value + (value * 0.15),
        mean_growth_rate * (2040 - 2018) < 0.15 ~ value + (value * mean_growth_rate)
      ),
    year = 2040
  ) %>%
  unique() %>%
  select(ctu_name, year, var, value)


## ----get county forecast for avg multifamily floor area for when ctu equivalent is missing ----
p_ctu_county_forecast <- p_county_characteristics_forecast %>%
  left_join(t_ctu_county, by = "co_name") %>%
  filter(var == "multifamily_average_floor_area_sqft_county") %>%
  group_by(ctu_name, year, var) %>%
  summarize(value = mean(value), .groups = "keep") %>%
  select(ctu_name, year, var, value)


## ----- compile forecast of ctu demographic characteristics -----
p_ctu_characteristics_forecast <-
  bind_rows(
    p_ctu_population_forecast,
    p_housing_stock_ctu_forecast,
    p_ctu_average_floor_area_single_family_forecast,
    p_ctu_average_floor_area_multifamily_forecast,
    p_ctu_emp_forecast,
    p_ctu_county_forecast
  )
