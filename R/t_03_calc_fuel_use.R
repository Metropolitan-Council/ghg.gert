#' @title Calculate use of fuel in thousands of gallons or thousands of kWh
#'
#' @inheritParams calc_ghg_direct
#' @inheritParams run_scenario_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams filter_ctu
#'
#'
#' @family transportation
#' @export
#' @importFrom dplyr filter select case_when rowwise mutate_all
#' @importFrom tidyselect all_of
#' @importFrom tidyr pivot_wider
calc_fuel_use <- function(tb_vmt,
                          tb,
                          .mode,
                          .aeo_scenario = "REF",
                          .miles_per_gallon,
                          .fuel_economy = fuel_economy,
                          .enviro_factors = enviro_factors,
                          .factor_values = factor_values) {

  # browser()
  tb_l <- .fuel_economy %>%
    dplyr::filter(mode == .mode, var == .miles_per_gallon) %>%
    unique() %>%
    tidyr::pivot_wider(
      names_from = var,
      values_from = value
    ) %>%
    dplyr::select(mode, year, aeo_mode,
      per_gallon_val = {{ .miles_per_gallon }}
    ) %>%
    unique()


  aeo_f_l <- .factor_values$aeo %>%
    dplyr::filter(
      metric == "MPG",
      aeo_scen == .aeo_scenario,
      mode == tb_l$aeo_mode
    ) %>%
    dplyr::select(year,
      aeo_factor = value
    )

  if (nrow(aeo_f_l) == 0) {
    aeo_f_l <- tibble::tibble(
      year = tb_l$year,
      aeo_factor = 1
    )
  }


  tb_aeo <- dplyr::left_join(tb_l,
    aeo_f_l,
    by = c("year")
  ) %>%
    dplyr::mutate(fuel_factor = per_gallon_val * aeo_factor) %>%
    dplyr::select(
      # mode,
      year,
      fuel_factor,
      aeo_mode
    )
  # browser()

  fuel_use <- dplyr::left_join(tb_vmt,
    tb_aeo,
    by = c("year", "aeo_mode")
  ) %>%
    dplyr::rowwise() %>%
    # VMT is given in thousands
    # so output is in thousands
    dplyr::mutate(fuel_use = vmt * fuel_factor) %>%
    dplyr::select(
      type,
      scenario,
      mode,
      ctu,
      class,
      year,
      aeo_mode,
      fuel_use
    )


  return(fuel_use)
}
