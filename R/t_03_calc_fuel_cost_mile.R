#' @title Calculate fuel cost per mile
#' @family transportation
#'
#' @param .miles_per_gallon numeric,
#'      Miles per gallon for current mode
#' @param .fuel_cost_gallon numeric,
#'      Fuel cost per gallon for current mode
#' @inheritParams calc_cost
#' @inheritParams calc_ghg_direct
#'
#' @return a tibble with columns for `mode`, `var`, `year`, and `fuel_cost_mile`.
#' @export
#' @importFrom dplyr filter select case_when left_join

calc_fuel_cost_mile <- function(tb,
                                .mode,
                                .aeo_scenario = "REF",
                                .miles_per_gallon,
                                .fuel_cost_gallon,
                                .fuel_economy = ghg.ccap::fuel_economy,
                                .enviro_factors = ghg.ccap::enviro_factors,
                                .factor_values = ghg.ccap::factor_values) {
  # cli::cli_progress_message("*** calculating fuel cost per mile \n")

  check_inputs(name = "aeo_scenario", .aeo_scenario)
  check_inputs(name = "miles_per_gallon", .miles_per_gallon)

  tb_l <- .fuel_economy %>%
    dplyr::filter(
      mode == .mode,
      var == .miles_per_gallon
    ) %>%
    dplyr::select(mode,
      year,
      fuel_mpg = var,
      aeo_mode,
      val_mpg = value
    ) %>%
    unique()


  aeo_f_l <- .factor_values$aeo %>%
    dplyr::filter(
      metric == "MPG",
      aeo_scen == .aeo_scenario,
      mode == unique(tb_l$aeo_mode)
    ) %>%
    dplyr::select(everything(),
      aeo_val = value
    )

  if (nrow(aeo_f_l) == 0) {
    cli::cli_warn(
      paste0("No AEO MPG available for ", .mode, " ", .miles_per_gallon),
      "Using reference value  = 1 instead"
    )

    aeo_f_l <- tibble::tribble(
      ~aeo_scen, ~mode, ~metric, ~year, ~aeo_val,
      "REF", unique(tb_l$aeo_mode), "MPG", "2015", 1,
      "REF", unique(tb_l$aeo_mode), "MPG", "2018", 1,
      "REF", unique(tb_l$aeo_mode), "MPG", "2020", 1,
      "REF", unique(tb_l$aeo_mode), "MPG", "2025", 1,
      "REF", unique(tb_l$aeo_mode), "MPG", "2030", 1,
      "REF", unique(tb_l$aeo_mode), "MPG", "2035", 1,
      "REF", unique(tb_l$aeo_mode), "MPG", "2040", 1,
      "REF", unique(tb_l$aeo_mode), "MPG", "2045", 1,
      "REF", unique(tb_l$aeo_mode), "MPG", "2050", 1
    )
  }

  re <- dplyr::left_join(tb_l, aeo_f_l,
    by = "year",
    suffix = c(".tb", ".aeo")
  ) %>%
    dplyr::mutate(fuel_cost_mile = .fuel_cost_gallon /
      (val_mpg * aeo_val)) %>%
    dplyr::select(year,
      mode = mode.tb,
      fuel_mpg,
      fuel_cost_mile
    )

  return(re)
}
