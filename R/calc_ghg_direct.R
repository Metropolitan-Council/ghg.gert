#' Calculate direct emissions
#'
#' @param tb_vmt output VMT table
#' @param tb input GHB table
#' @param .mode current mode
#' @param .fuel_type current fuel type for mode
#' @param .miles_per_gallon miles per gallon for mode
#' @param .is_av whether the mode is AV. AV has a different
#'     MPG due to efficiency gains from automation of drive cycle.
#' @inheritParams calc_vmt_forecast
#'
#' @return
#' @export
#' @family transportation
#' @importFrom dplyr filter select case_when rowwise mutate_all left_join
#' @importFrom tidyr pivot_wider
#' @importFrom rlang sym
calc_ghg_direct <- function(tb_vmt,
                            tb,
                            .mode,
                            .fuel_type,
                            .aeo_scenario = "REF",
                            .miles_per_gallon,
                            .is_av = 0,
                            .enviro_factors = enviro_factors) {
  # browser()


  ghg_factors_current <- factor_values$ghg %>%
    dplyr::filter(source == .fuel_type) %>%
    dplyr::select(source, year,
      ghg_factor = value
    )


  aeo_factors_current <- factor_values$aeo %>%
    dplyr::filter(
      metric == "MPG",
      aeo_scen == .aeo_scenario,
      mode == unique(tb_vmt$aeo_mode)
    ) %>%
    select(aeo_scen, metric, year, aeo_factor = value)


  # if there isn't an AEO miles per gallon value for the given mode,
  # use a value of 1
  if (nrow(aeo_factors_current) == 0) {
    aeo_factors_current <- tibble(
      year = ghg_factors_current$year,
      aeo_factor = 1
    )
  }

  aeo_ghg <- dplyr::left_join(ghg_factors_current,
    aeo_factors_current,
    by = c("year")
  )

  tb_current <- tb %>%
    dplyr::filter(
      mode == .mode,
      var == .miles_per_gallon
    ) %>%
    dplyr::mutate(
      av_multiplier = dplyr::case_when(
        .is_av == 1 ~ .enviro_factors$MPG_AV,
        TRUE ~ 1
      ),
      val_mpg = value
    ) %>%
    select(
      year,
      ctu,
      val_mpg,
      av_multiplier,
      mode,
      aeo_mode
    )


  tb_aeo_ghg <- dplyr::left_join(tb_current,
    aeo_ghg,
    by = c("year")
  ) %>%
    # calculate miles per gallon, multiplied by annual energy outlook factor and AV multiplier
    dplyr::mutate(val_mpg_aeo = val_mpg * aeo_factor * av_multiplier) %>%
    select(year, mode, aeo_mode, val_mpg_aeo, ghg_factor)


  ghg <- dplyr::left_join(tb_vmt,
    tb_aeo_ghg,
    by = c("mode", "year", "aeo_mode"),
    suffix = c(".vmt", ".aeo_ghg")
  ) %>%
    dplyr::mutate(dir_ghg = (vmt / val_mpg_aeo) * ghg_factor) %>%
    dplyr::select(
      # type,
      # source,
      scenario,
      mode,
      class,
      ctu,
      year,
      # aeo_scen,
      aeo_mode,
      # vmt,
      dir_ghg
    )


  return(ghg)
}
