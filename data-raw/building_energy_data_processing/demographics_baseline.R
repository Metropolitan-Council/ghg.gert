# baseline demographics
## -------------------------------------------------------------------------------------------
p_average_floor_area_single_family_county <-
  t_ztrax_sqft_summary_county %>%
  select(co_name, property_land_use, mean_sqft) %>%
  filter(property_land_use == "Single family residential") %>%
  # filter(co_name == p_county)  %>%
  group_by(co_name) %>%
  mutate(metric = "single_family_average_floor_area_sqft_county",
         year = 2018) %>%
  rename(value = mean_sqft) %>%
  select(co_name, year, metric, value)


## -------------------------------------------------------------------------------------------
p_average_floor_area_multifamily_county <-
  t_ztrax_sqft_summary_county %>%
  select(co_name, property_land_use, mean_sqft) %>%
  filter(property_land_use == "Condominium") %>%
  # filter(co_name == p_county)  %>%
  # group_by(co_name) %>%
  mutate(metric = "multifamily_average_floor_area_sqft_county",
         year = 2018) %>%
  rename(value = mean_sqft) %>%
  select(co_name, year, metric, value)


## -------------------------------------------------------------------------------------------
p_county_workers <-
  t_led_industry_county %>%
  # filter(co_name == p_county) %>%
  filter(year == 2018) %>%
  filter(jw_indicator == "W") %>%
  mutate(sector =
           case_when(
             (industry %in% naics_codes$led_commercial) ~ "commercial_workers_county",
             (industry %in% naics_codes$led_industrial) ~ "industrial_workers_county"
           )) %>%
  select(co_name, year, sector, count) %>%
  group_by(co_name, year, sector) %>%
  summarise(value = sum(count)) %>%
  rename(metric = sector)


## -------------------------------------------------------------------------------------------
p_county_characteristics <-
  bind_rows(
    p_average_floor_area_single_family_county,
    p_average_floor_area_multifamily_county,
    p_county_workers)


# CTU ----

## -------------------------------------------------------------------------------------------
p_ctu_population <-
  t_ctu_population %>%
  select(ctu_name, year, population, households) %>%
  filter(year == 2018) %>%
  group_by(ctu_name, year) %>%
  pivot_longer(
    cols = c("population", "households"),
    names_to = "metric",
    values_to = "value"
  )


## -------------------------------------------------------------------------------------------
p_jobs <-
  t_ctu_qcew_ctu %>%
  filter(naicstitle == "Total, All Industries") %>%
  filter(year==2018) %>%
  select(ctu_name, year, emp) %>%
  rename(value = emp) %>%
  mutate(metric = "total_jobs") %>%
  select(ctu_name, year, metric, value)


## -------------------------------------------------------------------------------------------
p_industrial_jobs <-
  t_ctu_qcew_ctu %>%
  filter(
    naicstitle %in% c(
      "Natural Resources and Mining",
      "Construction",
      "Trade, Transportation and Utilities"
    )
  ) %>%
  filter(year == 2018) %>%
  select(ctu_name, year, naicstitle, emp) %>%
  group_by(ctu_name, year) %>%
  summarise(value = sum(emp, na.rm = TRUE)) %>%
  mutate(metric = "industrial_jobs") %>%
  select(ctu_name, year, metric, value)


## -------------------------------------------------------------------------------------------
p_commercial_jobs <-
  t_ctu_qcew_ctu %>%
  filter(
    naicstitle %in% c(
      "Financial Activities",
      "Professional and Business Services",
      "Education and Health Services",
      "Leisure and Hospitality",
      "Public Administration"
    )
  ) %>%
  filter(year == 2018) %>%
  select(ctu_name, year, naicstitle, emp) %>%
  group_by(ctu_name, year) %>%
  summarise(value = sum(emp, na.rm = TRUE)) %>%
  mutate(metric = "commercial_jobs") %>%
  select(ctu_name, year, metric, value)


## -------------------------------------------------------------------------------------------
p_ctu_housing_stock <-
  t_housing_stock_ctu %>%
  mutate(
    single_family_units = single_family_detached + townhouse + manufactured_homes,
    multifamily_units = multifamily_in_5_or_more_units_bldng + duplex_triplex_or_quadplex
  ) %>%
  filter(year == 2018) %>%
  select(ctu_name, year, multifamily_units, single_family_units) %>%
  group_by(ctu_name, year) %>%
  pivot_longer(
    cols = c("single_family_units", "multifamily_units"),
    names_to = "metric",
    values_to = "value"
  )


## -------------------------------------------------------------------------------------------
p_average_floor_area_single_family_ctu <-
  t_ztrax_sqft_summary_ctu %>%
  select(ctu_name, property_land_use, mean_sqft) %>%
  filter(property_land_use == "Single family residential") %>%
  mutate(metric = "single_family_average_floor_area_sqft_ctu",
         year = 2018) %>%
  rename(value = mean_sqft) %>%
  select(ctu_name, year, metric, value)


## -------------------------------------------------------------------------------------------
p_average_floor_area_multifamily_ctu <-
  t_ztrax_sqft_summary_ctu %>%
  select(ctu_name, property_land_use, mean_sqft) %>%
  filter(property_land_use == "	Condominium") %>%
  mutate(metric = "multifamily_average_floor_area_sqft_ctu",
         year = 2018) %>%
  rename(value = mean_sqft) %>%
  select(ctu_name, year, metric, value)


## -------------------------------------------------------------------------------------------
p_ctu_characteristics <-
  bind_rows(
    p_ctu_population,
    p_jobs,
    p_commercial_jobs,
    p_industrial_jobs,
    p_ctu_housing_stock,
    p_average_floor_area_single_family_ctu,
    p_average_floor_area_multifamily_ctu
    # p_county_characteristics %>%
    #   rename("ctu_name" = "co_name") %>%
    #   mutate(ctu_name = params$ctu_name)
  )

