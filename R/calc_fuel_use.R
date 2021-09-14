#' Calculate use of fuel in thousands of gallons or thousands of kWh
#'
#' @inheritParams calc_ghg_direct
#' @inheritParams calc_vmt_forecast
#'
#' @return
#' @family transportation
#' @export
#' @importFrom dplyr filter select case_when rowwise mutate_all
#' @importFrom tidyselect all_of
#' @importFrom tidyr pivot_wider
calc_fuel_use <- function(tb_vmt,
                          tb,
                          .mode,
                          .fuel_type,
                          .aeo_scenario = "REF",
                          .miles_per_gallon,
                          .is_av = 0) {
  # browser()

  tb_l <- tb %>%
    dplyr::filter(mode == .mode, var == .miles_per_gallon) %>%
    mutate(av_multiplier = dplyr::case_when(
      .is_av == 1 ~ MPG_AV,
      TRUE ~ 1
    )) %>%
    tidyr::pivot_wider(
      names_from = var,
      values_from = value
    )


  aeo_f_l <- factor_values$aeo %>%
    dplyr::filter(
      Metric == "MPG",
      AEOScen == .aeo_scenario,
      Mode == tb_l$aeo_mode
    ) %>%
    select(year,
      aeo_factor = value
    )

  if (nrow(aeo_f_l) == 0) {
    aeo_f_l <- tibble(
      year = tb_l$year,
      aeo_factor = 1
    )
  }


  tb_aeo <- left_join(tb_l,
    aeo_f_l,
    by = c("year")
  ) %>%
    mutate(fuel_factor = !!
    rlang::sym(.miles_per_gallon) * aeo_factor * av_multiplier) %>%
    select(
      mode,
      year,
      fuel_factor,
      aeo_mode
    )
  # browser()

  fuel_use <- left_join(tb_vmt,
    tb_aeo,
    by = c("mode", "year", "aeo_mode")
  ) %>%
    mutate(fuel_use = vmt * fuel_factor) %>%
    select(
      # type,
      scenario,
      mode,
      ctu,
      year,
      aeo_mode,
      class,
      fuel_use
    )


  return(fuel_use)
}
