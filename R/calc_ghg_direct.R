#' Calculate direct emissions
#'
#' @param tb_vmt output VMT table
#' @param tb input GHB table
#' @param .mode current mode
#' @param .fuel_type current fuel type for mode
#' @param .miles_per_gallon miles per gallon for mode
#' @param .is_av whether the mode is AV. AV has a different
#'     MPG due to efficiency gains from automation of drive cycle.
#' @inheritParams calc_vmt
#'
#' @return
#' @export
#' @family transportation
#' @importFrom dplyr filter select case_when rowwise mutate_all
calc_ghg_direct <- function(tb_vmt,
                            tb,
                            .mode,
                            .fuel_type,
                            .aeo_scenario = "REF",
                            .miles_per_gallon,
                            .is_av = 0) {
  # browser()


  ghg_factors_current <- ghg_factors %>%
    dplyr::filter(source == .fuel_type) %>%
    dplyr::select(all_of(YRS))

  ghg_by_mode <- tb %>%
    dplyr::filter(mode == .mode, var == .miles_per_gallon) %>%
    dplyr::select(all_of(YRS)) *
    dplyr::case_when(
      .mode == "PLDV" ~ aeo_factors %>%
        dplyr::filter(
          AEOScen == .aeo_scenario,
          Metric == "MPG",
          Mode == "LDV"
        ) %>%
        dplyr::select(all_of(YRS)) %>%
        as.numeric(),
      .mode == "SUT" ~ aeo_factors %>%
        dplyr::filter(
          AEOScen == .aeo_scenario,
          Metric == "MPG",
          Mode == "MDT"
        ) %>%
        dplyr::select(all_of(YRS)) %>%
        as.numeric(),
      .mode == "CUT" ~ aeo_factors %>%
        dplyr::filter(
          AEOScen == .aeo_scenario,
          Metric == "MPG",
          Mode == "HDT"
        ) %>%
        dplyr::select(all_of(YRS)) %>%
        as.numeric(),
      .mode == "FR" ~ aeo_factors %>%
        dplyr::filter(
          AEOScen == .aeo_scenario,
          Metric == "MPG",
          Mode == "FRAIL"
        ) %>%
        dplyr::select(all_of(YRS)) %>%
        as.numeric(),
      .mode == "MM" | .mode == "AIR" | .mode == "WAT" ~ aeo_factors %>%
        dplyr::filter(
          AEOScen == .aeo_scenario,
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
    mutate(
      `2015` = `2015` / (ghg_by_mode * ghg_factors_current)$`2015`,
      `2018` = `2018` / (ghg_by_mode * ghg_factors_current)$`2018`,
      `2020` = `2020` / (ghg_by_mode * ghg_factors_current)$`2020`,
      `2025` = `2025` / (ghg_by_mode * ghg_factors_current)$`2025`,
      `2030` = `2030` / (ghg_by_mode * ghg_factors_current)$`2030`,
      `2035` = `2035` / (ghg_by_mode * ghg_factors_current)$`2035`,
      `2040` = `2040` / (ghg_by_mode * ghg_factors_current)$`2040`,
    )

  return(ghg)
}
