# residential baseline
## -------------------------------------------------------------------------------------------
p_electricity_residential_ctu <-
  t_electricity_residential_ctu %>%
  mutate(
    residential_mwh = case_when(
      (actual_residential_mwh > 1) ~ actual_residential_mwh,
      (is.na(actual_residential_mwh)) ~ est_residential_mwh
    ),
    residential_elec_emis_t_co2e = case_when(
      (actual_residential_mwh > 1) ~ actual_residential_electricity_emis_t_co2e,
      (is.na(actual_residential_mwh)) ~ est_residential_electricity_emis_t_co2e
    )
  ) %>%
  select(ctu_name, year, residential_mwh, residential_elec_emis_t_co2e) %>%
  group_by(ctu_name, year) %>%
  pivot_longer(
    cols = c("residential_mwh", "residential_elec_emis_t_co2e"),
    names_to = "metric"
  )


## -------------------------------------------------------------------------------------------
p_natural_gas_residential_ctu <-
  t_natural_gas_residential_ctu %>%
  mutate(
    residential_ng_therms = case_when(
      (actual_residential_ng_therms > 1) ~ actual_residential_ng_therms,
      (is.na(actual_residential_ng_therms)) ~ est_residential_ng_therms
    ),
    residential_ng_emis_t_co2e = case_when(
      (actual_residential_ng_therms > 1) ~ actual_residential_ng_emis_t_co2e,
      (is.na(actual_residential_ng_therms)) ~ est_residential_ng_emis_t_co2e
    )
  ) %>%
  select(ctu_name,
         year,
         residential_ng_therms,
         residential_ng_emis_t_co2e) %>%
  group_by(ctu_name, year) %>%
  pivot_longer(
    cols = c("residential_ng_therms", "residential_ng_emis_t_co2e"),
    names_to = "metric"
  )


## -------------------------------------------------------------------------------------------
p_residential_kwh_per_sqft <-
  bind_rows(p_electricity_residential_ctu,
            p_ctu_characteristics) %>%
  pivot_wider(names_from = "metric", values_from = "value") %>%
  mutate(kwh_per_floor_area = (residential_mwh / ((
    single_family_units * single_family_average_floor_area_sqft_ctu
  ) +
    (
      multifamily_units * multifamily_average_floor_area_sqft_county
    )
  )) * 1000) %>%
  select(ctu_name, year, kwh_per_floor_area) %>%
  group_by(ctu_name, year) %>%
  pivot_longer(
    cols = c("kwh_per_floor_area"),
    names_to = "metric"
  )


## -------------------------------------------------------------------------------------------
p_residential_therms_per_sqft <-
  bind_rows(p_natural_gas_residential_ctu,
            p_ctu_characteristics) %>%
  pivot_wider(names_from = "metric", values_from = "value") %>%
  mutate(therms_ng_per_floor_area = (residential_ng_therms / ((
    single_family_units * single_family_average_floor_area_sqft_ctu
  ) +
    (
      multifamily_units * multifamily_average_floor_area_sqft_county
    )
  ))) %>%
  select(ctu_name, year, therms_ng_per_floor_area) %>%
  group_by(ctu_name, year) %>%
  pivot_longer(
    cols = c("therms_ng_per_floor_area"),
    names_to = "metric"
  )


## -------------------------------------------------------------------------------------------
p_residential_kwh_per_household <-
  bind_rows(p_electricity_residential_ctu,
            p_ctu_characteristics) %>%
  pivot_wider(names_from = "metric", values_from = "value") %>%
  mutate(residential_mwh_per_households = residential_mwh / households) %>%
  select(ctu_name, year, residential_mwh_per_households) %>%
  group_by(ctu_name, year) %>%
  pivot_longer(
    cols = c("residential_mwh_per_households"),
    names_to = "metric"
  )


## -------------------------------------------------------------------------------------------
p_residential_therms_per_household <-
  bind_rows(p_natural_gas_residential_ctu,
            p_ctu_characteristics) %>%
  pivot_wider(names_from = "metric", values_from = "value") %>%
  mutate(residential_therms_per_households = residential_ng_therms / households) %>%
  group_by(ctu_name, year) %>%
  select(ctu_name, year, residential_therms_per_households) %>%
  pivot_longer(
    cols = c("residential_therms_per_households"),
    names_to = "metric"
  )


## -------------------------------------------------------------------------------------------
p_ctu_residential_energy_baseline <-
  bind_rows(
    p_electricity_residential_ctu,
    p_natural_gas_residential_ctu,
    p_residential_kwh_per_sqft,
    p_residential_therms_per_sqft,
    p_residential_kwh_per_household,
    p_residential_therms_per_household
  )

