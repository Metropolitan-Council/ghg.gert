## code to prepare `db_tables_mod_2` dataset goes here
mod_2 <- c()

mod_2$t_ctu_forecast <-
  councilR::import_from_emissions(table_name = "metro_sp_mod_1.vw_ctu_forecast") %>%
  tibble::as_tibble()

mod_2$t_ctu_population <-
  councilR::import_from_emissions(table_name = "metro_demographic.vw_ctu_population") %>%
  tibble::as_tibble()

mod_2$t_ctu_qcew_ctu <-
  councilR::import_from_emissions(table_name = "metro_demographic.vw_qcew_ctu") %>%
  tibble::as_tibble()

mod_2$t_ctu_county <-
  councilR::import_from_emissions(table_name = "metro_demographic.vw_ctu_county") %>%
  tibble::as_tibble()

mod_2$t_housing_stock_ctu <-
  councilR::import_from_emissions(table_name = "metro_demographic.vw_housing_stock_ctu") %>%
  tibble::as_tibble()

mod_2$t_emp_forecast_county <-
  councilR::import_from_emissions(table_name = "metro_demographic.vw_emp_forecast_county")

mod_2$t_emp_forecast_ctu <-
  councilR::import_from_emissions(table_name = "metro_demographic.vw_emp_forecast_ctu") %>%
  tibble::as_tibble()

mod_2$t_county <-
  councilR::import_from_emissions(table_name = "state_demographic.county") %>%
  tibble::as_tibble()

mod_2$t_ztrax_building_sqft <-
  councilR::import_from_emissions(table_name = "metro_sp_mod_2.vw_ztrax_building_sqft") %>%
  tibble::as_tibble()

mod_2$t_ztrax_sqft_summary_ctu <-
  councilR::import_from_emissions(table_name = "metro_sp_mod_2.vw_ztrax_sqft_summary_ctu") %>%
  tibble::as_tibble()

mod_2$t_ztrax_sqft_summary_county <-
  councilR::import_from_emissions(table_name = "metro_sp_mod_2.ztrax_sqft_summary_county") %>%
  tibble::as_tibble()

mod_2$t_led_industry_county <-
  councilR::import_from_emissions(table_name = "metro_demographic.vw_led_industry_county") %>%
  tibble::as_tibble()

mod_2$t_state_qcew <-
  councilR::import_from_emissions(table_name = "state_demographic.vw_state_qcew") %>%
  tibble::as_tibble()

## Energy Related Tables

mod_2$t_electricity_residential_ctu <-
  councilR::import_from_emissions(table_name = "metro_energy.vw_electricity_residential_ctu") %>%
  tibble::as_tibble()

mod_2$t_natural_gas_residential_ctu <-
  councilR::import_from_emissions(table_name = "metro_energy.vw_natural_gas_residential_ctu") %>%
  tibble::as_tibble()

mod_2$t_intersect_landuse_utility_service_area_ctu <-
  councilR::import_from_emissions(table_name = "metro_energy.vw_intersect_landuse_utility_service_area_ctu") %>%
  tibble::as_tibble()

mod_2$t_utility_electricity_by_ctu <-
  councilR::import_from_emissions(table_name = "metro_energy.vw_utility_electricity_by_ctu") %>%
  tibble::as_tibble()

mod_2$t_eia_electricity_servicewide <-
  councilR::import_from_emissions(table_name = "metro_energy.vw_eia_electricity_servicewide") %>%
  tibble::as_tibble()

mod_2$t_mndoc_electricity_county <-
  councilR::import_from_emissions(table_name = "metro_energy.vw_mndoc_electricity_county") %>%
  tibble::as_tibble() %>%
  dplyr::filter(utility_name != "Great River Energy")

mod_2$t_intersect_landuse_utility_service_area_county <-
  councilR::import_from_emissions(table_name = "metro_energy.vw_intersect_landuse_utility_service_area_county") %>%
  tibble::as_tibble()

mod_2$t_nrel_energy_consumption_ctu <-
  councilR::import_from_emissions(table_name = "metro_energy.vw_nrel_energy_consumption_ctu") %>%
  tibble::as_tibble()

mod_2$t_eia_energy_consumption_state <-
  councilR::import_from_emissions(table_name = "state_energy.eia_energy_consumption_state") %>%
  tibble::as_tibble()

mod_2$t_utility_natural_gas_by_ctu <-
  councilR::import_from_emissions(table_name = "metro_energy.vw_utility_natural_gas_by_ctu") %>%
  tibble::as_tibble()

usethis::use_data(db_tables_mod_2, overwrite = TRUE)
