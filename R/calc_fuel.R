#' Calculate use of fuel in thousands of gallons or thousands of kWh
#'
#' @inheritParams calc_ghg_direct
#'
#' @return
#' @family transportation
#' @export
#' @importFrom dplyr filter select case_when

calc_fuel <- function(tb_vmt,
                      tb,
                      m,
                      f,
                      aeo = "REF",
                      mpg,
                      av = 0
) {
  fuel <- tb_vmt %>%
    dplyr::select(all_of(YRS)) /
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

  return(fuel)
}
