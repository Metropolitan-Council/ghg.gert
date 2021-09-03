#' @title Calculate fuel cost per mile
#'
#' @param .miles_per_gallon miles per gallon for current mode
#' @param .fuel_cost_gallon  fuel cost per gallon for current mode
#' @inheritParams calc_cost
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
                                .av_pct = 0) {
  # browser()

  tb_l <- tb %>%
    dplyr::filter(mode == .mode, var == .miles_per_gallon) %>%
    mutate(av_multiplier = dplyr::case_when(
      .av_pct == 1 ~ MPG_AV,
      TRUE ~ 1
    ))


  aeo_f_l <- factor_values$aeo %>%
    dplyr::filter(
      Metric == "MPG",
      AEOScen == .aeo_scenario,
      Mode == tb_l$aeo_mode
    )


  re <- left_join(tb_l, aeo_f_l,
    by = "year",
    suffix = c(".tb", ".aeo")
  ) %>%
    mutate(fuel_cost_mile = .fuel_cost_gallon /
      (value.tb * value.aeo * av_multiplier)) %>%
    select(year, mode, var, ctu, fuel_cost_mile)

  return(re)
}
