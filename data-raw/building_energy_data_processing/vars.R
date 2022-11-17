# not every strategy needs every variable.
# I've indexed the variables needed for each group

vars_base <- c(
  "ctu_name",
  "year"
)

vars_residential <- c(
  "households",
  "population",
  "multifamily_units",
  "single_family_units",

  # totals
  "residential_mwh",
  "residential_kwh",
  "residential_therms",
  "residential_ng_therms",

  # multiplier
  "residential_elec_emis_t_co2e",
  "residential_ng_emis_t_co2e",
  "residential_kwh_per_floor_area",
  "residential_therms_per_floor_area",
  "residential_kwh_per_floor_area",
  "residential_therms_per_floor_area",

  # floor area
  "single_family_average_floor_area_sqft_ctu",
  "multifamily_average_floor_area_sqft_ctu",
  "multifamily_average_floor_area_sqft_county",


  # "residential_mwh_per_households",
  # "residential_therms_per_households",
)


vars_non_res <- c(
  "ctu_name",
  "year",
  # "commercial_ng_therm_per_worker",
  "commercial_mwh",
  "industrial_mwh",
  "industrial_therms",
  "commercial_therms",
  "commercial_therm_per_worker",
  "industrial_therm_per_worker",
  "commercial_mwh_per_worker",
  "industrial_mwh_per_worker"
)

vars_grid <- c(
  "ctu_name",
  "year",
  "residential_floor_area_per_capita",
  "residential_kwh_per_floor_area",
  "commercial_mwh_per_worker",
  "industrial_mwh_per_worker"
)


all_vars <- c(
  "ctu_name",
  "year",
  "households",
  "population",
  "total_jobs",
  "commercial_jobs",
  "industrial_jobs",
  "multifamily_units",
  "single_family_units",
  "single_family_average_floor_area_sqft_ctu",
  "multifamily_average_floor_area_sqft_ctu",
  "multifamily_average_floor_area_sqft_county",
  "residential_mwh",
  "residential_elec_emis_t_co2e",
  "residential_ng_therms",
  "residential_ng_emis_t_co2e",
  "residential_kwh_per_floor_area",
  "residential_therms_per_floor_area",
  "residential_mwh_per_households",
  "residential_therms_per_households",
  "commercial_mwh",
  "industrial_mwh",
  "industrial_therms",
  "commercial_therms",
  "commercial_ng_therms",
  "industrial_ng_therms",
  "commercial_ng_therm_per_worker",
  "industrial_ng_therm_per_worker",
  "commercial_mwh_per_worker",
  "industrial_mwh_per_worker",
  "residential_floor_area_per_capita",
  "electricity_emissions_kg_co2e",
  "natural_gas_emissions_kg_co2e",
  "jobs",
  "commercial_emp",
  "industrial_emp",
  "residential_kwh_per_floor_area",
  "residential_kwh",
  "residential_therms_per_floor_area",
  "residential_therms",
  "commercial_ng_therms"
)
