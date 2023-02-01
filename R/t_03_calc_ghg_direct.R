#' @title Calculate transportation direct emissions
#' @family emissions
#' @family transportation
#'
#' @param tb_vmt [tibble::tibble()], output VMT table
#' @param .mode character, given transportation mode.
#' @param .fuel_type character, fuel type for given mode.
#' @param .miles_per_gallon numeric, miles per gallon for mode.
#' @param .is_av logical, whether the mode is AV.
#'     AV has a different MPG due to efficiency gains from automation of drive cycle.
#'     Default is `FALSE`.
#' @inheritParams calc_vmt_forecast
#'
#' @return [tibble::tibble()] with column names
#'     - `type`
#'     - `scenario`
#'     - `mode`
#'     - `class`
#'     - ...
#' @export
#' @importFrom dplyr filter select case_when rowwise mutate_all left_join
#' @importFrom tidyr pivot_wider
#' @importFrom rlang sym
calc_ghg_direct <- function(tb_vmt,
                            tb,
                            .mode,
                            .fuel_type,
                            .aeo_scenario = "REF",
                            .miles_per_gallon,
                            .is_av = FALSE,
                            .enviro_factors = enviro_factors) {
  cat("*** calculating direct GHG emissions \n")
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
    mutate(val_mpg = value) %>%
    select(
      year,
      ctu,
      val_mpg,
      mode,
      aeo_mode
    )


  tb_aeo_ghg <- dplyr::left_join(tb_current,
    aeo_ghg,
    by = c("year")
  ) %>%
    # calculate miles per gallon, multiplied by annual energy outlook factor and AV multiplier
    dplyr::mutate(val_mpg_aeo = val_mpg * aeo_factor) %>%
    select(year, mode, aeo_mode, val_mpg_aeo, ghg_factor)


  ghg <- dplyr::left_join(
    tb_vmt,
    tb_aeo_ghg,
    by = c("year", "aeo_mode"),
    suffix = c(".vmt", ".aeo_ghg")
  ) %>%
    dplyr::mutate(dir_ghg = (vmt / val_mpg_aeo) * ghg_factor) %>%
    dplyr::select(type,
      # source,
      scenario,
      mode = mode.vmt,
      class,
      ctu,
      year,
      # aeo_scen,
      aeo_mode,
      # vmt,
      dir_ghg
    ) %>%
    unique()


  return(ghg)
}
