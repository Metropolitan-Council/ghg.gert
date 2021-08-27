#' @title Calculate fuel cost per mile
#'
#' @param mpg miles per gallon for current mode
#' @param fcg  fuel cost per gallon for current mode
#' @inheritParams calc_vmt
#' @inheritParams calc_cost
#'
#' @return
#' @export
#' @importFrom dplyr filter select case_when

calc_fuel_cost_mile <- function(tb,
                                m,
                                aeo = "REF",
                                mpg,
                                fcg,
                                av = 0) {
  fcm <- fcg /
    (tb %>% dplyr::filter(mode == m, var == mpg) %>%
      dplyr::select(all_of(YRS)) *
      dplyr::case_when(
        m == "PLDV" ~ aeo_factors %>%
          dplyr::filter(AEOScen == aeo, Metric == "MPG", Mode == "LDV") %>%
          dplyr::select(all_of(YRS)) %>%
          as.numeric(),
        m == "SUT" ~ aeo_factors %>%
          dplyr::filter(AEOScen == aeo, Metric == "MPG", Mode == "MDT") %>%
          dplyr::select(all_of(YRS)) %>%
          as.numeric(),
        m == "CUT" ~ aeo_factors %>%
          dplyr::filter(AEOScen == aeo, Metric == "MPG", Mode == "HDT") %>%
          dplyr::select(all_of(YRS)) %>%
          as.numeric(),
        m == "FR" ~ aeo_factors %>%
          dplyr::filter(AEOScen == aeo, Metric == "MPG", Mode == "FRAIL") %>%
          dplyr::select(all_of(YRS)) %>%
          as.numeric(),
        m == "MM" | m == "AIR" | m == "WAT" ~ aeo_factors %>%
          dplyr::filter(AEOScen == aeo, Metric == "MPG", Mode == "FSHIP") %>%
          dplyr::select(all_of(YRS)) %>%
          as.numeric(),
        TRUE ~ 1
      ) *
      # AV adjustment
      dplyr::case_when(
        av > 0 ~ MPG_AV,
        TRUE ~ 1
      ))
  return(fcm)
}
