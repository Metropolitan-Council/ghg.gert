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
    mutate(aeo_factor = value)


  tb_aeo <- left_join(tb_l,
    aeo_f_l,
    by = c("year")
  ) %>%
    mutate(fuel_factor = !!
      rlang::sym(.miles_per_gallon) * aeo_factor * av_multiplier) %>%
    select(-value)


  fuel_use <- left_join(tb_vmt,
    tb_aeo,
    by = c("mode", "year", "aeo_mode", "type")
  ) %>%
    mutate(fuel_use = vmt * fuel_factor) %>%
    select(type,
      scenario,
      mode,
      ctu = ctu.x,
      year,
      type,
      AEOScen,
      aeo_mode,
      class,
      fuel_use
    )


  return(fuel_use)
}
