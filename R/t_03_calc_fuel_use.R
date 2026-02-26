#' @title Calculate use of fuel in gallons or  kWh (thousands)
#'
#' @inheritParams calc_ghg_direct
#' @inheritParams run_module_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams filter_ctu
#'
#'
#' @family transportation
#' @export
#' @importFrom dplyr filter select case_when rowwise mutate_all distinct
#' @importFrom tidyselect all_of
calc_fuel_use <- function(tb_vmt,
                          tb,
                          .mode,
                          .aeo_scenario = "REF",
                          .miles_per_gallon,
                          .fuel_economy = fuel_economy,
                          .enviro_factors = enviro_factors,
                          .factor_values = factor_values) {

  tb_l <- .fuel_economy %>%
    dplyr::filter(mode == .mode, var == .miles_per_gallon) %>%
    dplyr::select(mode, year, aeo_mode, per_gallon_val = value) %>%
    dplyr::distinct()

  aeo_f_l <- .factor_values$aeo %>%
    dplyr::filter(
      metric == "MPG",
      aeo_scen == .aeo_scenario,
      mode == tb_l$aeo_mode[[1]]
    ) %>%
    dplyr::select(year, aeo_factor = value)

  if (nrow(aeo_f_l) == 0) {
    aeo_f_l <- tibble::tibble(year = tb_l$year, aeo_factor = 1)
  }

  tb_l %>%
    dplyr::left_join(aeo_f_l, by = "year") %>%
    dplyr::mutate(fuel_factor = per_gallon_val * aeo_factor) %>%
    dplyr::select(year, fuel_factor, aeo_mode) %>%
    dplyr::left_join(tb_vmt, ., by = c("year", "aeo_mode")) %>%
    dplyr::mutate(fuel_use_gallons_kwh = vmt / fuel_factor) %>%
    dplyr::select(type, scenario, mode, geog_name, geog_id, class,
                  year, aeo_mode, fuel_use_gallons_kwh) %>%
    return()
}
