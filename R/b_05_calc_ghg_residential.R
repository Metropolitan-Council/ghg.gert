#' @title Calculate residential building emissions
#' @family buildings
#' @family residential
#' @family emissions
#'
#' @description Estimates total energy demand
#'      and emissions from the residential building sector by city/township
#'      for the user-specified scenario, and the business-as-usual scenario.
#'
#' @note `calc_ghg_residential()` estimates the building energy demand and emissions
#'      based on the floor area assumptions. For a function that compiles all
#'      residential strategies refer to [`scen_building_residential()`].
#'
#' @param res_energy [tibble::tibble()].
#'      Table with residential energy, including columns
#'      `residential_mwh`, `residential_mcf`,
#'      `residential_propane_mmbtu`, and `residential_kerosene_mmbtu`.
#' @inheritParams run_module_transportation
#' @inheritParams scen_building_residential
#' @inheritParams calc_energy_residential
#'
#' @return [tibble::tibble()].
#'    A table with the input energy columns plus
#'    `electricity_emissions`, `natural_gas_emissions`,
#'    and `liquid_fuel_emissions` (propane + kerosene combined, mt CO2e).
#'
#' @export
#'
calc_ghg_residential <- function(res_energy,
                                 .selected_ctu,
                                 .grid_emissions = ghg.gert::grid_emissions,
                                 .combustion_ef = ghg.gert::combustion_ef) {
  res_energy <- filter_ctu(res_energy, .selected_ctu = .selected_ctu)

  # pull scalar EFs from combustion_ef lookup
  ef_natgas  <- .combustion_ef$mt_co2e_per_unit[.combustion_ef$fuel_type == "Natural Gas"]
  ef_propane <- .combustion_ef$mt_co2e_per_unit[.combustion_ef$fuel_type == "Propane"]
  ef_kerosene <- .combustion_ef$mt_co2e_per_unit[.combustion_ef$fuel_type == "Kerosene"]

  res_emissions <- res_energy %>%
    dplyr::left_join(.grid_emissions, by = "emissions_year") %>%
    dplyr::mutate(
      electricity_emissions  = residential_mwh * mt_co2e_per_mwh,
      natural_gas_emissions  = residential_mcf * ef_natgas,
      liquid_fuel_emissions  = residential_propane_mmbtu * ef_propane +
        residential_kerosene_mmbtu * ef_kerosene
    ) %>%
    dplyr::select(-c(factor_source, mt_co2e_per_mwh))

  return(res_emissions)
}
