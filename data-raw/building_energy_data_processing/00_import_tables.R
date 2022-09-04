# import tables

# demographic baseline
## -------------------------------------------------------------------------------------------
t_ztrax_sqft_summary_county <-
  import_from_emissions("metro_energy.ztrax_sqft_summary_county")

t_led_industry_county <-
  import_from_emissions("metro_demographic.vw_led_industry_county")

t_ctu_population <-
  import_from_emissions("metro_demographic.vw_ctu_population")

t_ctu_qcew_ctu <-
  import_from_emissions("metro_demographic.vw_qcew_ctu")

t_forecast_lu_ctu <-
  import_from_emissions("metro_demographic.vw_forecast_lu_ctu")

t_ztrax_sqft_summary_ctu <-
  import_from_emissions("metro_energy.vw_ztrax_sqft_summary_ctu")

t_ctu_county <-
  import_from_emissions("metro_demographic.vw_ctu_county")

# demographic forecast
## -------------------------------------------------------------------------------------------
t_ztrax_building_sqft <-
  import_from_emissions("metro_energy.vw_ztrax_building_sqft")

t_emp_forecast_industry_county <-
  import_from_emissions("metro_demographic.vw_emp_forecast_industry_county")

t_ctu_forecast <-
  import_from_emissions("metro_demographic.vw_ctu_forecast")

t_emp_forecast_industry_ctu <-
  import_from_emissions("metro_demographic.vw_emp_forecast_industry_ctu")


# residential baseline
## -------------------------------------------------------------------------------------------
t_electricity_residential_ctu <-
  import_from_emissions("metro_energy.vw_electricity_residential_ctu")

t_natural_gas_residential_ctu <-
  import_from_emissions("metro_energy.vw_natural_gas_residential_ctu")

# non-residential baseline
## -------------------------------------------------------------------------------------------
t_eia_electricity_servicewide <-
  import_from_emissions("metro_energy.vw_eia_electricity_servicewide")

t_mndoc_electricity_county <-
  import_from_emissions("metro_energy.vw_mndoc_electricity_county")

t_intersect_landuse_utility_service_area_county <-
  import_from_emissions("metro_energy.vw_intersect_landuse_utility_service_area_county")

t_eia_energy_consumption_state <-
  import_from_emissions("state_energy.eia_energy_consumption_state")

t_intersect_landuse_utility_service_area_ctu <-
  import_from_emissions("metro_energy.vw_intersect_landuse_utility_service_area_ctu")

t_state_qcew <-
  import_from_emissions("state_demographic.vw_state_qcew")

t_county <- import_from_emissions("state_demographic.county")

t_utility_electricity_by_ctu <-
  import_from_emissions("metro_energy.vw_utility_electricity_by_ctu")

t_nrel_energy_consumption_ctu <-
  import_from_emissions("metro_energy.vw_nrel_energy_consumption_ctu")

t_utility_natural_gas_by_ctu <-
  import_from_emissions("metro_energy.vw_utility_natural_gas_by_ctu")
