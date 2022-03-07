# baseline demographics

## -------------------------------------------------------------------------------------------
p_average_annual_growth_single_family_sqft <-
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
  summarise(mean_growth_rate = mean(rate_percent, na.rm = TRUE))
# filter(co_name == p_county)


## -------------------------------------------------------------------------------------------
p_average_annual_growth_multifamily_sqft <-
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
  summarise(mean_growth_rate = mean(rate_percent, na.rm = TRUE))
# filter(co_name == p_county)

p_average_floor_area_multifamily_county_forecast <-
  p_average_floor_area_multifamily_county %>%
  left_join(p_average_annual_growth_multifamily_sqft) %>%
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
p_emp_forecast_county <-
  t_emp_forecast_county %>%
  filter(year == 2040) %>%
  dplyr::group_by(co_name, year, indlabel) %>%
  dplyr::mutate(metric =
                  dplyr::case_when(
                    (indlabel %in% naics_codes$industrial ~ "industrial_emp_forecast_county"),
                    (indlabel %in% naics_codes$commerical ~ "commercial_emp_forecast_county")
                  )) %>%
  group_by(co_name, year, metric) %>%
  summarise(value = sum(emp, na.rm = T))
# filter(co_name == p_county)


## -------------------------------------------------------------------------------------------
p_county_characteristics_forecast <-
  bind_rows(
    p_average_floor_area_multifamily_county_forecast,
    p_emp_forecast_county
  )


# CTU -----

## ----population-----------------------------------------------------------------------------
p_population_forecast <-
  t_ctu_forecast %>%
  filter(year == 2040)


## ----industrial workers---------------------------------------------------------------------
p_emp_forecast_ctu <-
  t_emp_forecast_ctu %>%
  filter(year == 2040) %>%
  dplyr::group_by(ctu_name, year, indlabel) %>%
  dplyr::mutate(metric =
                  dplyr::case_when(
                    (indlabel %in% naics_codes$industrial ~ "industrial_emp_forecast"),
                    (indlabel %in% naics_codes$commerical ~ "commercial_emp_forecast")
                  )) %>%
  group_by(ctu_name, year, metric) %>%
  summarise(value = sum(emp, na.rm = T))


## ----housing stock--------------------------------------------------------------------------

# TODO housing stock forecasts are based on the growth rate in each housing stock type
# this is fairly unreliable, because some years have negative or no growth
p_growth_rate_single_family_stock_ctu <-
  t_housing_stock_ctu %>%
  filter(year >=2010) %>%
  group_by(ctu_name) %>%
  mutate(
    single_family_units = single_family_detached + townhouse + manufactured_homes,
    multifamily_units = multifamily_in_5_or_more_units_bldng + duplex_triplex_or_quadplex
  ) %>%
  mutate(
    diff_year = year - lag(year),
    # Difference in time (just in case there are gaps)
    diff_growth = single_family_units - lag(single_family_units),
    # Difference in route between years
    rate_percent = (diff_growth / diff_year) / single_family_units * 100
  ) %>% # growth rate in percent
  summarise(average_annual_growth_rate_single_family = mean(rate_percent, na.rm = TRUE),
            average_annual_growth_rate_single_family = ifelse(average_annual_growth_rate_single_family <= 0, 1, average_annual_growth_rate_single_family))

p_growth_rate_multifamily_stock_ctu <-
  t_housing_stock_ctu %>%  filter(year >=2010) %>%
  rowwise() %>%
  mutate(
    single_family_units = single_family_detached + townhouse + manufactured_homes,
    multifamily_units = multifamily_in_5_or_more_units_bldng + duplex_triplex_or_quadplex
  ) %>%
  group_by(ctu_name) %>%
  summarize(
    diff_year = year - lag(year),
    # Difference in time (just in case there are gaps)
    diff_growth = multifamily_units - lag(multifamily_units),
    # Difference in route between years
    rate_percent = (diff_growth / abs(diff_year)) / multifamily_units * 100
  ) %>% # growth rate in percent
  summarise(average_annual_growth_rate_multifamily = mean(rate_percent, na.rm = TRUE),
            average_annual_growth_rate_multifamily = ifelse(average_annual_growth_rate_multifamily <= 0, 1, average_annual_growth_rate_multifamily))

p_housing_stock_ctu_ratios <-
  p_ctu_housing_stock %>%
  group_by(ctu_name) %>%
  left_join(p_growth_rate_single_family_stock_ctu) %>%
  left_join(p_growth_rate_multifamily_stock_ctu) %>%
  mutate(
    single_family_growth_rate = average_annual_growth_rate_single_family,
    multifamily_growth_rate = average_annual_growth_rate_multifamily
  ) %>%
  mutate(value =
           case_when(
             ((metric == "single_family_units") ~ value * single_family_growth_rate
             ),
             ((metric == "multifamily_units") ~ value * multifamily_growth_rate
             )
           )) %>%
  mutate(value = value / sum(value)) %>%
  ungroup()

p_housing_stock_ctu_forecast <-
  p_housing_stock_ctu_ratios %>%
  select(ctu_name, metric, value) %>%
  right_join(
    p_population_forecast %>%
      filter(metric == "households") %>%
      select(ctu_name, year, value),
    by = "ctu_name"
  ) %>%
  mutate(value = value.x * value.y) %>%
  select(ctu_name, year, metric, value)


p_housing_stock_ctu_forecast %>%
  filter(value < 0)

## ----average building area single family----------------------------------------------------
p_average_floor_area_single_family_ctu_forecast <-
  p_average_floor_area_single_family_ctu %>%
  left_join(t_ctu_county) %>%
  left_join(p_average_annual_growth_single_family_sqft) %>%
  select(-co_name) %>%
  group_by(ctu_name) %>%
  mutate(mean_growth_rate = mean(mean_growth_rate)) %>%
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

count(p_average_floor_area_single_family_ctu_forecast, ctu_name) %>% filter(n > 1)


## -------------------------------------------------------------------------------------------
p_average_floor_area_multifamily_ctu_forecast <-
  p_average_floor_area_multifamily_ctu %>%
  left_join(t_ctu_county) %>%
  left_join(p_average_annual_growth_multifamily_sqft) %>%
  select(-co_name) %>%
  group_by(ctu_name) %>%
  mutate(mean_growth_rate = mean(mean_growth_rate)) %>%
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
    p_population_forecast,
    p_housing_stock_ctu_forecast,
    p_average_floor_area_single_family_ctu_forecast,
    p_average_floor_area_multifamily_ctu_forecast,
    # p_county_characteristics_forecast %>%
      # rename("ctu_name" = "co_name") %>%
      # mutate(ctu_name = params$ctu_name),
    p_emp_forecast_ctu,
    p_ctu_county_forecast
  )


