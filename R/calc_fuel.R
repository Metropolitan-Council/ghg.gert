#' Calculate use of fuel in thousands of gallons or thousands of kWh
#'
#' @inheritParams calc_ghg_direct
#' @inheritParams calc_vmt
#'
#' @return
#' @family transportation
#' @export
#' @importFrom dplyr filter select case_when rowwise mutate_all
#' @importFrom tidyselect all_of
calc_fuel <- function(tb_vmt,
                      tb,
                      .mode,
                      .fuel_type,
                      aeo = "REF",
                      mpg,
                      av = 0) {
  # browser()

  ghg_by_mode <- tb %>%
    dplyr::filter(mode == .mode, var == mpg) %>%
    dplyr::select(tidyselect::all_of(YRS)) *
    dplyr::case_when(
      .mode == "PLDV" ~ aeo_factors %>%
        dplyr::filter(AEOScen == aeo, Metric == "MPG", Mode == "LDV") %>%
        dplyr::select(tidyselect::all_of(YRS)) %>%
        as.numeric(),
      .mode == "SUT" ~ aeo_factors %>%
        dplyr::filter(AEOScen == aeo, Metric == "MPG", Mode == "MDT") %>%
        dplyr::select(tidyselect::all_of(YRS)) %>%
        as.numeric(),
      .mode == "CUT" ~ aeo_factors %>%
        dplyr::filter(AEOScen == aeo, Metric == "MPG", Mode == "HDT") %>%
        dplyr::select(tidyselect::all_of(YRS)) %>%
        as.numeric(),
      .mode == "FR" ~ aeo_factors %>%
        dplyr::filter(AEOScen == aeo, Metric == "MPG", Mode == "FRAIL") %>%
        dplyr::select(tidyselect::all_of(YRS)) %>%
        as.numeric(),
      .mode == "MM" | .mode == "AIR" | .mode == "WAT" ~ aeo_factors %>%
        dplyr::filter(AEOScen == aeo, Metric == "MPG", Mode == "FSHIP") %>%
        dplyr::select(tidyselect::all_of(YRS)) %>%
        as.numeric(),
      TRUE ~ 1
    ) *
    # AV adjustment
    dplyr::case_when(
      av > 0 ~ MPG_AV,
      TRUE ~ 1
    )


  fuel <- tb_vmt %>%
    dplyr::select(tidyselect::all_of(YRS)) %>%
    mutate(
      `2015` = `2015` / (ghg_by_mode)$`2015`,
      `2018` = `2018` / (ghg_by_mode)$`2018`,
      `2020` = `2020` / (ghg_by_mode)$`2020`,
      `2025` = `2025` / (ghg_by_mode)$`2025`,
      `2030` = `2030` / (ghg_by_mode)$`2030`,
      `2035` = `2035` / (ghg_by_mode)$`2035`,
      `2040` = `2040` / (ghg_by_mode)$`2040`,
    )
  return(fuel)
}
