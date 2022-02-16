#' @title Calculate fuel cost per mile
#'
#' @param .miles_per_gallon miles per gallon for current mode
#' @param .fuel_cost_gallon  fuel cost per gallon for current mode
#' @inheritParams calc_cost
#' @inheritParams calc_ghg_direct
#'
#' @return a tibble with columns for `mode`, `var`, `year`, and `fuel_cost_mile`.
#' @export
#' @importFrom dplyr filter select case_when left_join
#' @family transportation
calc_fuel_cost_mile <- function(tb,
                                .mode,
                                .aeo_scenario = "REF",
                                .miles_per_gallon,
                                .fuel_cost_gallon,
                                .av_pct = 0,
                                .enviro_factors = enviro_factors) {
  # browser()
  tb_l <- tb %>%
    dplyr::filter(
      mode == .mode,
      var == .miles_per_gallon
    ) %>%
    mutate(av_multiplier = dplyr::case_when(
      .av_pct == 1 ~ .enviro_factors$MPG_AV,
      TRUE ~ 1
    )) %>%
    select(mode,
      year,
      fuel_mpg = var,
      aeo_mode,
      av_multiplier,
      val_mpg = value
    ) %>%
    unique()


  aeo_f_l <- factor_values$aeo %>%
    dplyr::filter(
      metric == "MPG",
      aeo_scen == .aeo_scenario,
      mode == unique(tb_l$aeo_mode)
    ) %>%
    select(everything(),
      aeo_val = value
    )

  re <- left_join(tb_l, aeo_f_l,
    by = "year",
    suffix = c(".tb", ".aeo")
  ) %>%
    mutate(fuel_cost_mile = .fuel_cost_gallon /
      (val_mpg * aeo_val * av_multiplier)) %>%
    select(year,
      mode = mode.tb,
      fuel_mpg,
      fuel_cost_mile
    )

  return(re)
}
