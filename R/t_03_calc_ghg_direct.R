#' @title Calculate transportation direct emissions
#' @family emissions
#' @family transportation
#'
#' @description
#' Calculate the direct greenhouse gas emissions for a given mode and fuel
#'   type. If the fuel type is non-electric, the returned value represents
#'   tail-pipe emissions. If fuel type is electric, the returned value
#'   represents the equivalent emissions per kilowatt hour, modulated
#'   by the percentage of the grid that is de-carbonized
#'   (`.grid_decarbonization_pct`). If the entire grid is de-carbonized
#'   (`.grid_decarbonization_pct = 1`), then there are no emissions for
#'   electric vehicles.
#'
#' @param tb_vmt [tibble::tibble()], output VMT table
#' @param .mode character, given transportation mode.
#' @param .fuel_type character, fuel type for given mode.
#' @param .miles_per_gallon numeric, miles per gallon for mode.
#'
#' @inheritParams calc_vmt_forecast
#' @inheritParams run_scenario_building
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
                            .grid_decarbonization_pct = 1,
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


  if(.fuel_type == "BEV" & .grid_decarbonization_pct == 0){
    cli::cli_warn(
      "No grid de-carbonization present - all BEV fuel evaluated on a 100% carbonized electrial grid ")
  }

  grid_elast <-
    tibble(
      year = unique(tb_vmt$year),
      grid_decarb = calc_elasticity(
        elas_list = c(rep(0, length(unique(tb_vmt$year)))),
        elas = .grid_decarbonization_pct,
        num_inits = 3,
        num_yrs = length(unique(tb_vmt$year)) - 3
      ))



  ghg <- dplyr::left_join(
    tb_vmt,
    tb_aeo_ghg,
    by = c("year", "aeo_mode"),
    suffix = c(".vmt", ".aeo_ghg")
  ) %>%
    dplyr::left_join(grid_elast,
              by = c("year")) %>%
  dplyr::mutate(
    dir_ghg =
      dplyr::if_else((mode.vmt == "PLDV" & class == "BEV"),
                     ((vmt / val_mpg_aeo) * ghg_factor * (1 - grid_decarb)),
                     ((vmt / val_mpg_aeo) * ghg_factor))) %>%
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
