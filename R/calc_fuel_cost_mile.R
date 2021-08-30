#' @title Calculate fuel cost per mile
#'
#' @param mpg miles per gallon for current mode
#' @param fuel_cost_gallon  fuel cost per gallon for current mode
#' @inheritParams calc_vmt
#' @inheritParams calc_cost
#'
#' @return
#' @export
#' @importFrom dplyr filter select case_when

calc_fuel_cost_mile <- function(tb,
                                .mode,
                                aeo = "REF",
                                mpg,
                                fuel_cost_gallon,
                                av = 0) {
  adj_specific <- tb %>%
    dplyr::filter(mode == .mode, var == mpg) %>%
    dplyr::select(all_of(YRS)) *
    # fetch specific annual energy outlook (AEO) for the given metric, mode
    dplyr::case_when(
      .mode == "PLDV" ~ aeo_factors %>%
        dplyr::filter(AEOScen == aeo, Metric == "MPG", Mode == "LDV") %>%
        dplyr::select(all_of(YRS)) %>%
        as.numeric(),
      .mode == "SUT" ~ aeo_factors %>%
        dplyr::filter(AEOScen == aeo, Metric == "MPG", Mode == "MDT") %>%
        dplyr::select(all_of(YRS)) %>%
        as.numeric(),
      .mode == "CUT" ~ aeo_factors %>%
        dplyr::filter(AEOScen == aeo, Metric == "MPG", Mode == "HDT") %>%
        dplyr::select(all_of(YRS)) %>%
        as.numeric(),
      .mode == "FR" ~ aeo_factors %>%
        dplyr::filter(AEOScen == aeo, Metric == "MPG", Mode == "FRAIL") %>%
        dplyr::select(all_of(YRS)) %>%
        as.numeric(),
      .mode == "MM" | .mode == "AIR" | .mode == "WAT" ~ aeo_factors %>%
        dplyr::filter(AEOScen == aeo, Metric == "MPG", Mode == "FSHIP") %>%
        dplyr::select(all_of(YRS)) %>%
        as.numeric(),
      TRUE ~ 1
    ) *
    # AV adjustment
    # if vehicle is AV, then multiply by 1
    # otherwise, multiply by the estimated reduction in fuel use for AVs
    dplyr::case_when(
      av == 1 ~ MPG_AV,
      TRUE ~ 1
    )

  fuel_cost_mile <- fuel_cost_gallon / adj_specific

  return(fuel_cost_mile)
}
