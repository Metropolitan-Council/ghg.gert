# non-residential baseline
library(tidyr)
library(dplyr)
library(magrittr)
library(purrr)
library(ghg.sp)
library(councilR)

# import tables
building_energy_data <- c()

# demographic baseline
## -------------------------------------------------------------------------------------------
building_energy_data$ztrax_sqft_summary_county <-
  import_from_emissions("metro_energy.ztrax_sqft_summary_county")

building_energy_data$led_industry_county <-
  import_from_emissions("metro_demographic.vw_led_industry_county")

building_energy_data$ctu_population <-
  import_from_emissions("metro_demographic.vw_ctu_population")

building_energy_data$ctu_qcew_ctu <-
  import_from_emissions("metro_demographic.vw_qcew_ctu")

building_energy_data$forecast_lu_ctu <-
  import_from_emissions("metro_demographic.vw_forecast_lu_ctu")

building_energy_data$ztrax_sqft_summary_ctu <-
  import_from_emissions("metro_energy.vw_ztrax_sqft_summary_ctu")

building_energy_data$ctu_county <-
  import_from_emissions("metro_demographic.vw_ctu_county")

# demographic forecast
## -------------------------------------------------------------------------------------------
building_energy_data$ztrax_building_sqft <-
  import_from_emissions("metro_energy.vw_ztrax_building_sqft")

building_energy_data$emp_forecast_industry_county <-
  import_from_emissions("metro_demographic.vw_emp_forecast_industry_county")

building_energy_data$ctu_forecast <-
  import_from_emissions("metro_demographic.vw_ctu_forecast")

building_energy_data$emp_forecast_industry_ctu <-
  import_from_emissions("metro_demographic.vw_emp_forecast_industry_ctu")


# residential baseline
## -------------------------------------------------------------------------------------------
building_energy_data$electricity_residential_ctu <-
  import_from_emissions("metro_energy.vw_electricity_residential_ctu")

building_energy_data$natural_gas_residential_ctu <-
  import_from_emissions("metro_energy.vw_natural_gas_residential_ctu")

## -------------------------------------------------------------------------------------------
building_energy_data$eia_electricity_servicewide <-
  import_from_emissions("metro_energy.vw_eia_electricity_servicewide")

building_energy_data$mndoc_electricity_county <-
  import_from_emissions("metro_energy.vw_mndoc_electricity_county")

building_energy_data$intersect_landuse_utility_service_area_county <-
  import_from_emissions("metro_energy.vw_intersect_landuse_utility_service_area_county")

building_energy_data$eia_energy_consumption_state <-
  import_from_emissions("state_energy.eia_energy_consumption_state")

building_energy_data$intersect_landuse_utility_service_area_ctu <-
  import_from_emissions("metro_energy.vw_intersect_landuse_utility_service_area_ctu")

building_energy_data$state_qcew <-
  import_from_emissions("state_demographic.vw_state_qcew")

building_energy_data$county <- import_from_emissions("state_demographic.county")

building_energy_data$utility_electricity_by_ctu <-
  import_from_emissions("metro_energy.vw_utility_electricity_by_ctu")

building_energy_data$nrel_energy_consumption_ctu <-
  import_from_emissions("metro_energy.vw_nrel_energy_consumption_ctu")

building_energy_data$utility_natural_gas_by_ctu <-
  import_from_emissions("metro_energy.vw_utility_natural_gas_by_ctu")


# -------------------------------------------------------------------------


usethis::use_data(building_energy_data, overwrite = T)
