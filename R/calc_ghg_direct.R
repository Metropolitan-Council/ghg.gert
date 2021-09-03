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
#' @importFrom dplyr filter select case_when rowwise mutate_all left_join
#' @importFrom tidyr pivot_wider
#' @importFrom rlang sym
calc_ghg_direct <- function(tb_vmt,
                            tb,
                            .mode,
                            .fuel_type,
                            .aeo_scenario = "REF",
                            .miles_per_gallon,
                            .is_av = 0) {
  # browser()


  ghg_factors_current <- factor_values$ghg %>%
    dplyr::filter(source == .fuel_type)


  aeo_factors_current <- factor_values$aeo %>%
    dplyr::filter(
      Metric == "MPG",
      AEOScen == .aeo_scenario,
      Mode == unique(tb_vmt$aeo_mode)
    )


  aeo_ghg <- dplyr::left_join(ghg_factors_current,
    aeo_factors_current,
    by = c("year"),
    suffix = c(".ghg_factor", ".aeo_factor")
  )

  tb_current <- tb %>%
    dplyr::filter(
      mode == .mode,
      var == .miles_per_gallon
    ) %>%
    dplyr::mutate(
      av_multiplier = dplyr::case_when(
        .is_av == 1 ~ MPG_AV,
        TRUE ~ 1
      ),
      value.mpg = value
    ) %>%
    select(-value)


  tb_aeo_ghg <- dplyr::left_join(tb_current,
    aeo_ghg,
    by = c("year")
  ) %>%
    # calculate miles per gallon, multiplied by annual energy outlook factor and AV multiplier
    dplyr::mutate(value.mpg_aeo = value.mpg * value.aeo_factor * av_multiplier)


  ghg <- dplyr::left_join(tb_vmt,
    tb_aeo_ghg,
    by = c("mode", "year", "aeo_mode", "type")
  ) %>%
    dplyr::mutate(dir_ghg = (vmt / value.mpg_aeo) * value.ghg_factor) %>%
    dplyr::select(type,
      scenario,
      mode,
      class,
      ctu = ctu,
      year,
      # AEOScen,
      aeo_mode,
      # vmt,
      dir_ghg
    )


  return(ghg)
}
