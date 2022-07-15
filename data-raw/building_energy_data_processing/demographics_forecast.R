# import tables
## -------------------------------------------------------------------------------------------
t_ztrax_building_sqft <- import_from_emissions("metro_sp_mod_2.vw_ztrax_building_sqft")
t_emp_forecast_county <- import_from_emissions("metro_demographic.vw_emp_forecast_county")
t_ctu_forecast <- import_from_emissions("metro_sp_mod_1.vw_ctu_forecast")
t_emp_forecast_ctu <- import_from_emissions("metro_demographic.vw_emp_forecast_ctu")

# baseline demographics
## -------------------------------------------------------------------------------------------
p_county_average_annual_growth_single_family_sqft <-
  t_ztrax_building_sqft %>%
  filter(year_built > 1991) %>%
  filter(residential_type %in% c("single_family_residential")) %>%
  group_by(co_name) %>%
  arrange(co_name, year_built) %>%
  mutate(
    diff_year = year_built - lag(year_built),
    # Difference in time (just in case there are gaps)
    diff_growth = average_building_sqft - lag(average_building_sqft),
    # Difference in route between years
    rate_percent = (diff_growth / diff_year) / average_building_sqft
  ) %>% # growth rate in percent
  summarise(mean_growth_rate = mean(rate_percent, na.rm = TRUE), .groups = "keep")


## -------------------------------------------------------------------------------------------
p_county_average_annual_growth_multifamily_sqft <-
  t_ztrax_building_sqft %>%
  filter(year_built > 1991) %>%
  filter(residential_type %in% c("condominium")) %>%
  group_by(co_name) %>%
  arrange(co_name, year_built) %>%
  mutate(
    diff_year = year_built - lag(year_built),
    # Difference in time (just in case there are gaps)
    diff_growth = average_building_sqft - lag(average_building_sqft),
    # Difference in route between years
    rate_percent = (diff_growth / diff_year) / average_building_sqft
  ) %>% # growth rate in percent
  summarise(mean_growth_rate = mean(rate_percent, na.rm = TRUE), .groups = "keep")


## -------------------------------------------------------------------------------------------
p_county_average_floor_area_multifamily_forecast <-
  p_county_average_floor_area_multifamily %>%
  left_join(p_county_average_annual_growth_multifamily_sqft, by = "co_name") %>%
  mutate(
    value =
      case_when(
        mean_growth_rate * (2040 - 2018) > 0.15 ~ value + (value *
                                                             0.15),
        mean_growth_rate * (2040 - 2018) < 0.15 ~ value + (value * mean_growth_rate)
      ),
    year = 2040
  )


## -------------------------------------------------------------------------------------------
p_county_emp_forecast <-
  t_emp_forecast_county %>%
  filter(year == 2040) %>%
  dplyr::group_by(co_name, year, indlabel) %>%
  dplyr::mutate(
    metric =
      dplyr::case_when(
        (indlabel %in% naics_codes$industrial ~ "industrial_emp_forecast_county"),
        (indlabel %in% naics_codes$commerical ~ "commercial_emp_forecast_county")
      )
  ) %>%
  group_by(co_name, year, metric) %>%
  summarise(value = sum(emp, na.rm = T), .groups = "keep")


## -------------------------------------------------------------------------------------------
p_county_characteristics_forecast <-
  bind_rows(
    p_county_average_floor_area_multifamily_forecast,
    p_county_emp_forecast
  )


# CTU -----

## ----population-----------------------------------------------------------------------------
p_ctu_population_forecast <-
  t_ctu_forecast %>%
  filter(year == 2040)


## ----industrial workers---------------------------------------------------------------------
p_ctu_emp_forecast <-
  t_emp_forecast_ctu %>%
  filter(year == 2040) %>%
  dplyr::group_by(ctu_name, year, indlabel) %>%
  dplyr::mutate(
    metric =
      dplyr::case_when(
        (indlabel %in% naics_codes$industrial ~ "industrial_emp_forecast"),
        (indlabel %in% naics_codes$commerical ~ "commercial_emp_forecast")
      )
  ) %>%
  group_by(ctu_name, year, metric) %>%
  summarise(value = sum(emp, na.rm = T), .groups = "keep")


## ----housing stock--------------------------------------------------------------------------
p_housing_stock_ctu_forecast <- t_forecast_lu_ctu %>%
  filter(
    year == 2040,
    ctu_name %in% unique(t_ctu_forecast$ctu_name),
    metric %in% (c("SFD_Units", "MF_Units"))
  )


## ----average building area single family----------------------------------------------------
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
  select(ctu_name, year, metric, value)

# count(p_ctu_average_floor_area_single_family_forecast, ctu_name) %>% filter(n > 1)

## -------------------------------------------------------------------------------------------
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
  select(ctu_name, year, metric, value)


## --------
p_ctu_county_forecast <- p_county_characteristics_forecast %>%
  left_join(t_ctu_county, by = "co_name") %>%
  filter(metric == "multifamily_average_floor_area_sqft_county") %>%
  group_by(ctu_name, year, metric) %>%
  summarize(value = mean(value), .groups = "keep") %>%
  select(ctu_name, year, metric, value)


## -------------------------------------------------------------------------------------------
p_ctu_characteristics_forecast <-
  bind_rows(
    p_ctu_population_forecast,
    p_housing_stock_ctu_forecast,
    p_ctu_average_floor_area_single_family_forecast,
    p_ctu_average_floor_area_multifamily_forecast,
    # p_county_characteristics_forecast %>%
    # rename("ctu_name" = "co_name") %>%
    # mutate(ctu_name = params$ctu_name),
    p_ctu_emp_forecast,
    p_ctu_county_forecast
  )
