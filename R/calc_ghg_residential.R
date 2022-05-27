#' @title Calculate Residential Building Emissions
#' @family building_energy_module
#'
#' @description `calc_ghg_residential()` estimates total energy demand
#' and emissions from the residential sector by city/township.
#'
#' @param res_tb table, table with residential building data.
#' @inheritParams run_scenario
#'
#' @return a table with columns `year`, `ctu_name`, `residential_mwh`,
#'    `residential_electricity_emissions_kg_co`,
#'    `residential_therms`, and `residential_natural_gas_emissions_kg_co`
#'
#'

#' @export
#'
calc_ghg_residential <- function(res_tb,
                                 .grid_decarbonization_pct,
                                 .enviro_factors = enviro_factors) {
  emis <- res_tb %>%
    dplyr::filter(
      var %in% c(
        "population",
        "single_family_units",
        "multifamily_units",
        "population",
        "kwh_per_floor_area",
        "therms_per_floor_area",
        "single_family_average_floor_area_sqft_ctu",
        "multifamily_average_floor_area_sqft_county"
      )
    ) %>%
    dplyr::group_by(ctu_name, year, var) %>%
    tidyr::pivot_wider(names_from = "var", values_from = value) %>%
    dplyr::mutate(
      kg_per_mwh = dplyr::case_when(
        year < 2040 ~ .enviro_factors$KG_CO2E_PER_MHW_BASELINE,
        TRUE ~ .enviro_factors$KG_CO2E_PER_MHW_FORECAST
      ),
      kg_per_therm = dplyr::case_when(
        year < 2040 ~ .enviro_factors$KG_CO2E_PER_THERM_BASELINE,
        TRUE ~ .enviro_factors$KG_CO2E_PER_THERM_FORECAST
      )
    ) %>%
    dplyr::mutate(residential_floor_area_per_capita = (
      (
        single_family_average_floor_area_sqft_ctu * single_family_units
      ) +
        (
          multifamily_average_floor_area_sqft_county * multifamily_units
        )
    )
    / population) %>%
    dplyr::mutate(
      residential_mwh = population * residential_floor_area_per_capita * (kwh_per_floor_area / 1000),
      residential_electricity_emissions_kg_co = residential_mwh * (kg_per_mwh * .grid_decarbonization_pct)
    ) %>%
    dplyr::mutate(
      residential_therms = population * residential_floor_area_per_capita * therms_per_floor_area,
      residential_natural_gas_emissions_kg_co = if_else(
        .renewable_natural_gas == TRUE,
        (residential_therms - (population * 78)) * kg_per_therm,
        residential_therms * kg_per_therm
      )
    ) %>%
    dplyr::mutate(total_residential_emissions = sum(
      c(
        residential_natural_gas_emissions_kg_co,
        residential_electricity_emissions_kg_co
      ),
      na.rm = T
    )) %>%
    unique() %>%
    dplyr::select(
      ctu_name,
      year,
      residential_mwh,
      residential_electricity_emissions_kg_co,
      residential_therms,
      residential_natural_gas_emissions_kg_co,
      total_residential_emissions
    )


  return(emis)
}
