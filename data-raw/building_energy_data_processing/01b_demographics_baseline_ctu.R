# baseline demographics
# CTU DEMOGRAPHICS ----

## ---- get population & households from 'Emissions'----
p_ctu_population <-
  t_ctu_population %>%
  select(ctu_name, year, population, households) %>%
  filter(year == 2018) %>%
  group_by(ctu_name, year) %>%
  pivot_longer(
    cols = c("population", "households"),
    names_to = "var",
    values_to = "value"
  ) %>%
  group_by(ctu_name, year, var) %>%
  summarise(value = sum(value, na.rm = T), .groups = "keep")
# portions of a city that fall in more than one county
# are aggregated.

## ----- get jobs by industry from 'Emissions' -----
p_ctu_jobs <-
  t_ctu_qcew_ctu %>%
  filter(naicstitle == "Total, All Industries") %>%
  filter(year == 2018) %>%
  select(ctu_name, year, emp) %>%
  rename(value = emp) %>%
  mutate(var = "total_jobs") %>%
  select(ctu_name, year, var, value)


## ----- aggregate industrial jobs by NAICS -----
p_ctu_industrial_jobs <-
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
  summarise(value = sum(emp, na.rm = TRUE), .groups = "keep") %>%
  mutate(var = "industrial_jobs") %>%
  select(ctu_name, year, var, value)


## ----- aggregate commercial jobs by NAICS ----
p_ctu_commercial_jobs <-
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
  summarise(value = sum(emp, na.rm = TRUE), .groups = "keep") %>%
  mutate(var = "commercial_jobs") %>%
  select(ctu_name, year, var, value)


## ----- get forecast of single and multifamily units from 'Emissions' ----
p_ctu_housing_stock <-
  t_forecast_lu_ctu %>%
  filter(var %in% c("SFD_Units", "MF_Units")) %>%
  filter(year == 2018) %>%
  group_by(ctu_name, year)


## ----- estimate single family average floor area from ZTRAX ----
p_ctu_average_floor_area_single_family <-
  t_ztrax_sqft_summary_ctu %>%
  select(ctu_name, property_land_use, mean_sqft) %>%
  unique() %>%
  filter(property_land_use == "Single family residential") %>%
  group_by(ctu_name) %>%
  mutate(
    var = "single_family_average_floor_area_sqft_ctu",
    year = 2018,
    value = mean(mean_sqft, na.rm = T)
  ) %>%
  select(ctu_name, year, var, value)


## ---- get average multifamily floor area from ZTRAX ----
p_ctu_average_floor_area_multifamily <-
  t_ztrax_sqft_summary_ctu %>%
  select(ctu_name, property_land_use, mean_sqft) %>%
  filter(stringr::str_detect(property_land_use, "Condominium")) %>%
  mutate(
    var = "multifamily_average_floor_area_sqft_ctu",
    year = 2018
  ) %>%
  rename(value = mean_sqft) %>%
  select(ctu_name, year, var, value)


## ---- get county multifamily floor area for when ctu equivalent is missing ----
p_ctu_county <- p_county_characteristics %>%
  left_join(t_ctu_county, by = "co_name") %>%
  filter(var == "multifamily_average_floor_area_sqft_county") %>%
  group_by(ctu_name, year, var) %>%
  mutate(value = value * pct_population) %>%
  select(ctu_name, year, var, value) %>%
  group_by(ctu_name, year, var) %>%
  summarise(value = sum(value), .groups = "keep")
# uses weighted average based on population

#---- compile ctu demographic baseline characterics -----
p_ctu_characteristics <-
  bind_rows(
    p_ctu_population,
    p_ctu_jobs,
    p_ctu_commercial_jobs,
    p_ctu_industrial_jobs,
    p_ctu_housing_stock,
    p_ctu_average_floor_area_single_family,
    p_ctu_average_floor_area_multifamily,
    p_ctu_county
  ) %>%
  unique()
