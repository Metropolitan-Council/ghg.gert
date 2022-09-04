# COUNTY DEMOGRAPHIC FORECAST ----

## ---- estimate avg growth in single family floor area -----
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


## ----- estimate avg growth in single family floor area ----
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


## ----- estimate avg floor area for multifamily ----
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


## ----- get commercial/industrial workers forecast from 'Emissions' ----
p_county_emp_forecast <-
  t_emp_forecast_industry_county %>%
  filter(year == 2040) %>%
  group_by(co_name, year, indlabel) %>%
  mutate(
    var =
      case_when(
        (indlabel %in% naics_codes$industrial ~ "industrial_emp_forecast_county"),
        (indlabel %in% naics_codes$commercial ~ "commercial_emp_forecast_county")
      )
  ) %>%
  group_by(co_name, year, var) %>%
  summarise(value = sum(emp, na.rm = T), .groups = "keep")


## ----- compile county forecast of demographic characteristics ----
p_county_characteristics_forecast <-
  bind_rows(
    p_county_average_floor_area_multifamily_forecast,
    p_county_emp_forecast
  )

