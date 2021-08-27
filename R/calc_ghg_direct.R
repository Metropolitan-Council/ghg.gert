#' Calculate direct emissions
#'
#' @param tb_vmt output VMT table
#' @param tb input GHB table
#' @param .mode current mode
#' @param .fuel_type current fuel type
#' @param mpg miles per gallon for mode
#' @param is_av whether the mode is AV. AV has a different
#'     MPG due to efficiency gains from automation of drive cycle.
#' @inheritParams calc_vmt
#'
#' @return
#' @export
#'
#' @importFrom dplyr filter select case_when rowwise mutate_all
calc_ghg_direct <- function(tb_vmt,
                            tb,
                            .mode,
                            .fuel_type,
                            aeo = "REF",
                            mpg,
                            is_av = 0) {
  # browser()


  ghg_factors_current <- ghg_factors %>%
    dplyr::filter(source == .fuel_type) %>%
    dplyr::select(all_of(YRS))

  ghg_by_mode <- tb %>%
    dplyr::filter(mode == .mode, var == mpg) %>%
    dplyr::select(all_of(YRS)) *
    dplyr::case_when(
      .mode == "PLDV" ~ aeo_factors %>%
        dplyr::filter(
          AEOScen == aeo,
          Metric == "MPG",
          Mode == "LDV"
        ) %>%
        dplyr::select(all_of(YRS)) %>%
        as.numeric(),
      .mode == "SUT" ~ aeo_factors %>%
        dplyr::filter(
          AEOScen == aeo,
          Metric == "MPG",
          Mode == "MDT"
        ) %>%
        dplyr::select(all_of(YRS)) %>%
        as.numeric(),
      .mode == "CUT" ~ aeo_factors %>%
        dplyr::filter(
          AEOScen == aeo,
          Metric == "MPG",
          Mode == "HDT"
        ) %>%
        dplyr::select(all_of(YRS)) %>%
        as.numeric(),
      .mode == "FR" ~ aeo_factors %>%
        dplyr::filter(
          AEOScen == aeo,
          Metric == "MPG",
          Mode == "FRAIL"
        ) %>%
        dplyr::select(all_of(YRS)) %>%
        as.numeric(),
      .mode == "MM" | .mode == "AIR" | .mode == "WAT" ~ aeo_factors %>%
        dplyr::filter(
          AEOScen == aeo,
          Metric == "MPG",
          Mode == "FSHIP"
        ) %>%
        dplyr::select(all_of(YRS)) %>%
        as.numeric(),
      TRUE ~ 1
    ) *
    # AV adjustment
    dplyr::case_when(
      is_av > 0 ~ MPG_AV,
      TRUE ~ 1
    )

  ghg <- tb_vmt %>%
    dplyr::select(all_of(YRS)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate_all(., function(col) {
      col / (ghg_by_mode * ghg_factors_current)
    })

  return(ghg)
}
