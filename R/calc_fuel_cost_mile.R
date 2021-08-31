#' @title Calculate fuel cost per mile
#'
#' @param .miles_per_gallon miles per gallon for current mode
#' @param .fuel_cost_gallon  fuel cost per gallon for current mode
#' @inheritParams calc_cost
#'
#' @return
#' @export
#' @importFrom dplyr filter select case_when
#' @family transportation
calc_fuel_cost_mile <- function(tb,
                                .mode,
                                .aeo_scenario = "REF",
                                .miles_per_gallon,
                                .fuel_cost_gallon,
                                .av_pct = 0) {
  adj_specific <- tb %>%
    dplyr::filter(mode == .mode, var == .miles_per_gallon) %>%
    dplyr::select(all_of(YRS)) *
    # fetch specific annual energy outlook (AEO) for the given metric, mode
    dplyr::case_when(
      .mode == "PLDV" ~ aeo_factors %>%
        dplyr::filter(AEOScen == .aeo_scenario, Metric == "MPG", Mode == "LDV") %>%
        dplyr::select(all_of(YRS)) %>%
        as.numeric(),
      .mode == "SUT" ~ aeo_factors %>%
        dplyr::filter(AEOScen == .aeo_scenario, Metric == "MPG", Mode == "MDT") %>%
        dplyr::select(all_of(YRS)) %>%
        as.numeric(),
      .mode == "CUT" ~ aeo_factors %>%
        dplyr::filter(AEOScen == .aeo_scenario, Metric == "MPG", Mode == "HDT") %>%
        dplyr::select(all_of(YRS)) %>%
        as.numeric(),
      .mode == "FR" ~ aeo_factors %>%
        dplyr::filter(AEOScen == .aeo_scenario, Metric == "MPG", Mode == "FRAIL") %>%
        dplyr::select(all_of(YRS)) %>%
        as.numeric(),
      .mode == "MM" | .mode == "AIR" | .mode == "WAT" ~ aeo_factors %>%
        dplyr::filter(AEOScen == .aeo_scenario, Metric == "MPG", Mode == "FSHIP") %>%
        dplyr::select(all_of(YRS)) %>%
        as.numeric(),
      TRUE ~ 1
    ) *
    # AV adjustment
    # if vehicle is AV, then multiply by 1
    # otherwise, multiply by the estimated reduction in fuel use for AVs
    dplyr::case_when(
      .av_pct == 1 ~ MPG_AV,
      TRUE ~ 1
    )

  fuel_cost_mile <- .fuel_cost_gallon / adj_specific

  return(fuel_cost_mile)
}
