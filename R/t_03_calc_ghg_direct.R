#' @title Calculate transportation direct emissions in metric tons.
#' @family emissions
#' @family transportation
#'
#' @description
#' Calculate the direct greenhouse gas emissions for a given mode and fuel
#'   type. If the fuel type is non-electric, the returned value represents
#'   tail-pipe emissions. If fuel type is electric, the returned value
#'   represents the *equivalent* emissions per kilowatt hour.
#'
#' @param tb_vmt [tibble::tibble()], VMT table
#' @param .mode character, given transportation mode.
#' @param .fuel_type character, fuel type for given mode.
#' @param .miles_per_gallon numeric, miles per gallon for mode.
#' @param .fuel_economy table, table with GHG factor values. Default is `ghg.ccap::fuel_economy`
#'
#' @inheritParams run_module_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams filter_ctu
#' @inheritParams run_scenario_building
#'
#' @return [tibble::tibble()] with column names
#'     - `type`
#'     - `scenario`
#'     - `mode`
#'     - `class`
#'     - `dir_ghg` numeric, direct greenhouse gas emissions in metric tons
#'     - ...
#' @export
#' @importFrom dplyr filter select case_when rowwise mutate_all left_join
#' @importFrom tidyr pivot_wider
#' @importFrom rlang sym
calc_ghg_direct <- function(tb_vmt,
                            tb,
                            .mode,
                            .fuel_type,
                            .miles_per_gallon,
                            .aeo_scenario = "REF",
                            .fuel_economy = ghg.ccap::fuel_economy,
                            .enviro_factors = ghg.ccap::enviro_factors,
                            .factor_values = ghg.ccap::factor_values) {
  check_inputs(name = "fuel_type", value = .fuel_type)
  # for given fuel type,
  # find the number of metric tons CO2 per gallon of fuel/kilowatt hour
  ghg_factors_current <- .factor_values$ghg %>%
    dplyr::filter(source == .fuel_type) %>%
    dplyr::select(source, year,
      ghg_factor = value
    )


  fuel_gallons <- calc_fuel_use(
    tb_vmt,
    tb = tb,
    .mode = .mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = .miles_per_gallon,
    .fuel_economy = .fuel_economy,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )


  ghg <-
    fuel_gallons %>%
    dplyr::left_join(ghg_factors_current,
      by = c("year")
    ) %>%
    dplyr::mutate(
      # emissions  = gallons * ghg_factor
      dir_ghg = (fuel_use_gallons_kwh * ghg_factor)) %>%
    dplyr::select(
      type,
      # source,
      scenario,
      mode,
      class,
      geog_name, geog_id,
      year,
      # aeo_scen,
      aeo_mode,
      # vmt,
      dir_ghg
    ) %>%
    unique()


  return(ghg)
}
