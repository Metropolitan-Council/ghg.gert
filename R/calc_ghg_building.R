#' Calculate emissions by residential floor area
#'
#' @param res_tb table, table with residential building data.
#' @inheritParams run_scenario
#'
#' @return a table with columns `year`, `ctu_name`, `residential_mwh`,
#'    `residential_electricity_emissions_kg_co`,
#'    `residential_therms`, and `residential_natural_gas_emissions_kg_co`
#'
#'
#' @family buildings
#' @export
#'
calc_ghg_floor_area <- function(res_tb = building_data$residential,
                                .enviro_factors = enviro_factors){
  emis <- res_tb %>%
    dplyr::filter(var %in% c("single_family_units",
                             "multifamily_units",
                             "population",
                             "kwh_per_floor_area",
                             "therms_per_floor_area",
                             "single_family_average_floor_area_sqft_ctu",
                             "multifamily_average_floor_area_sqft_county")) %>%
    dplyr::group_by(ctu_name, year, var) %>%
    tidyr::pivot_wider(names_from = "var", values_from = value) %>%
    dplyr::mutate(kg_per_mwh = dplyr::case_when(year < 2040 ~.enviro_factors$KG_CO2E_PER_MHW_BASELINE,
                                                TRUE ~ .enviro_factors$KG_CO2E_PER_MHW_FORECAST),
                  kg_per_therm = dplyr::case_when(year < 2040 ~.enviro_factors$KG_CO2E_PER_THERM_BASELINE,
                                                  TRUE ~ .enviro_factors$KG_CO2E_PER_THERM_FORECAST)) %>%
    dplyr::mutate(
      residential_floor_area_per_capita = (
        (single_family_average_floor_area_sqft_ctu * single_family_units) +
          (multifamily_average_floor_area_sqft_county * multifamily_units))
      / population) %>%
    dplyr::mutate(
      residential_mwh = population * residential_floor_area_per_capita * (kwh_per_floor_area / 1000),
      residential_electricity_emissions_kg_co = residential_mwh * kg_per_mwh
    ) %>%
    dplyr::mutate(
      residential_therms = population * residential_floor_area_per_capita * therms_per_floor_area,
      residential_natural_gas_emissions_kg_co = residential_therms * kg_per_therm ) %>%
    dplyr::mutate(total_residential_emissions = sum(c(residential_natural_gas_emissions_kg_co,
                    residential_electricity_emissions_kg_co), na.rm = T)) %>%
    unique() %>%
    dplyr::select(ctu_name, year, residential_mwh, residential_electricity_emissions_kg_co,
                  residential_therms, residential_natural_gas_emissions_kg_co, total_residential_emissions)


  return(emis)
}



#' Calculate emissions by worker for industrial and commercial sectors
#'
#' @param non_res_tb table with non-residential data.
#'      Default is `building_data$non_residential`
#' @inheritParams run_scenario
#' @return
#' @export
#'
#' @family buildings
#'
calc_ghg_worker <- function(non_res_tb = building_data$non_residential,
                            .enviro_factors = enviro_factors){

  emis <- non_res_tb %>%
    dplyr::filter(var %in% c("commercial_jobs",
                             "industrial_jobs",
                             "commercial_therm_per_worker",
                             "industrial_therm_per_worker",
                             "commercial_mwh_per_worker",
                             "industrial_mwh_per_worker")) %>%
    dplyr::group_by(ctu_name, year, var) %>%
    tidyr::pivot_wider(names_from = "var", values_from = value) %>%
    dplyr::mutate(kg_per_mwh = dplyr::case_when(year < 2040 ~.enviro_factors$KG_CO2E_PER_MHW_BASELINE,
                                                TRUE ~ .enviro_factors$KG_CO2E_PER_MHW_FORECAST),
                  kg_per_therm = dplyr::case_when(year < 2040 ~.enviro_factors$KG_CO2E_PER_THERM_BASELINE,
                                                  TRUE ~ .enviro_factors$KG_CO2E_PER_THERM_FORECAST)
    ) %>%
    dplyr::mutate(
      # mw hours
      commercial_mwh = (commercial_jobs * commercial_mwh_per_worker),
      industrial_mwh = (industrial_jobs * industrial_mwh_per_worker),

      # therms
      commercial_therms = (commercial_jobs * commercial_therm_per_worker),
      industrial_therms = (industrial_jobs * industrial_therm_per_worker),

      # electric emissions
      commercial_electricity_emissions_kg_co = commercial_mwh * kg_per_mwh,
      industrial_electricity_emissions_kg_co = industrial_mwh * kg_per_mwh,

      # therm emissions
      commercial_natural_gas_emissions_kg_co = commercial_therms * kg_per_therm,
      industrial_natural_gas_emissions_kg_co = industrial_therms * kg_per_therm,

      total_industrial_commercial_emissions = sum(c(commercial_electricity_emissions_kg_co,
                                                    industrial_electricity_emissions_kg_co,
                                                    commercial_natural_gas_emissions_kg_co,
                                                    industrial_natural_gas_emissions_kg_co),
                                                  na.rm = T)
    ) %>%
    dplyr::select(ctu_name, year, commercial_mwh, industrial_mwh,
                  commercial_therms, industrial_therms,
                  commercial_electricity_emissions_kg_co, industrial_electricity_emissions_kg_co,
                  commercial_natural_gas_emissions_kg_co, industrial_natural_gas_emissions_kg_co,
                  total_industrial_commercial_emissions)


  return(emis)
}
