#' @title Calculate non-residential building emissions
#' @family buildings
#' @family non-residential
#' @family emissions
#'
#' @description Estimates total energy demand
#'      and emissions from the residential building sector by city/township
#'      for the user-specified scenario, and the business-as-usual scenario.
#'
#' @note `calc_ghg_non_residential()` estimates the building energy demand and emissions
#'      based on job forecasts and selected strategies. For a function that compiles all
#'      non-residential strategies refer to [`scen_building_non_residential()`].
#'
#' @param res_energy [tibble::tibble()].
#'      Table, table with residential energy,
#'      including columns `non_residential_mwh`, `non_residential_mcf`.
#' @inheritParams run_module_transportation
#' @inheritParams scen_building_residential
#' @inheritParams calc_energy_residential
#'
#'
#' @return [tibble::tibble()].
#'    A table with columns
#'    `geog_name`,
#'    `year`,
#'    `population`,
#'    `residential_mwh`,
#'    `residential_electricity_emissions_kg_co`,
#'    `residential_therms`, and
#'    `residential_natural_gas_emissions_kg_co`
#'
#' @export
#'
calc_ghg_non_residential <- function(non_res_energy,
                                     .selected_ctu,
                                     .grid_emissions = ghg.ccap::grid_emissions,
                                     .combustion_ef = ghg.ccap::combustion_ef) {

  ef_natgas  <- .combustion_ef$mt_co2e_per_unit[.combustion_ef$fuel_type == "Natural Gas"]

  non_res_energy <- filter_ctu(non_res_energy, .selected_ctu = .selected_ctu)

  non_res_emissions <- non_res_energy %>%
    dplyr::left_join(.grid_emissions, by = c("emissions_year")) %>%
    dplyr::mutate(
      electricity_emissions = non_residential_mwh * mt_co2e_per_mwh,
      natural_gas_emissions = non_residential_mcf * ef_natgas
    ) %>%
    dplyr::select(-c(factor_source, mt_co2e_per_mwh))

  return(non_res_emissions)
}
