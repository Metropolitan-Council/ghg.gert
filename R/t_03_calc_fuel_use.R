#' @title Calculate use of fuel in thousands of gallons or thousands of kWh
#'
#' @inheritParams calc_ghg_direct
#' @inheritParams calc_vmt_forecast
#'
#' @family transportation
#' @export
#' @importFrom dplyr filter select case_when rowwise mutate_all
#' @importFrom tidyselect all_of
#' @importFrom tidyr pivot_wider
calc_fuel_use <- function(tb_vmt,
                          tb,
                          .mode,
                          .aeo_scenario = "REF",
                          .miles_per_gallon,
                          .is_av = FALSE,
                          .enviro_factors = enviro_factors) {
  cat("*** calculating fuel use \n")
  # browser()

  tb_l <- tb %>%
    dplyr::filter(mode == .mode, var == .miles_per_gallon) %>%
    unique() %>%
    mutate(av_multiplier = dplyr::case_when(
      .is_av == 1 ~ .enviro_factors$MPG_AV,
      TRUE ~ 1
    )) %>%
    tidyr::pivot_wider(
      names_from = var,
      values_from = value
    ) %>%
    select(mode, year, aeo_mode, av_multiplier,
      per_gallon_val = {{ .miles_per_gallon }}
    ) %>%
    unique()


  aeo_f_l <- factor_values$aeo %>%
    dplyr::filter(
      metric == "MPG",
      aeo_scen == .aeo_scenario,
      mode == tb_l$aeo_mode
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
    mutate(fuel_factor = per_gallon_val * aeo_factor * av_multiplier) %>%
    select(
      # mode,
      year,
      fuel_factor,
      aeo_mode
    )
  # browser()

  fuel_use <- left_join(tb_vmt,
    tb_aeo,
    by = c("year", "aeo_mode")
  ) %>%
    rowwise() %>%
    mutate(fuel_use = vmt * fuel_factor) %>%
    select(
      type,
      scenario,
      mode,
      ctu,
      class,
      year,
      aeo_mode,
      fuel_use
    )


  return(fuel_use)
}
