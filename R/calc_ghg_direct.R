#' Calculate direct emissions
#'
#' @param tb_vmt output VMT table
#' @param tb input GHB table
#' @param m current mode
#' @param f current fuel type
#' @param mpg miles per gallon for mode
#' @param is_av whether the mode is AV. AV has a different
#' @inheritParams calc_vmt
#'
#' @return
#' @export
#'
#' @importFrom dplyr filter select case_when
calc_ghg_direct <- function(tb_vmt,
                            tb,
                            m,
                            f,
                            aeo = "REF",
                            mpg,
                            is_av = 0) {
) {
  ghg <- tb_vmt %>% dplyr::select(all_of(YRS)) /
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
      is_av > 0 ~ MPG_AV,
         TRUE ~ 1
       )) *
    ghg_factors %>%
    dplyr::filter(source == f) %>%
    dplyr::select(all_of(YRS))

  return(ghg)
}
