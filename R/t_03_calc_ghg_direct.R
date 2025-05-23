#' @title Calculate transportation direct emissions in metric tons.
#' @family emissions
#' @family transportation
#'
#' @description
#' Calculate the direct greenhouse gas emissions for a given mode and fuel
#'   type. If the fuel type is non-electric, the returned value represents
#'   tail-pipe emissions. If fuel type is electric, the returned value
#'   represents the *equivalent* emissions per kilowatt hour, modulated
#'   by the percentage of the grid that is de-carbonized
#'   (`.grid_decarbonization_pct`). If the entire grid is de-carbonized
#'   (`.grid_decarbonization_pct = 1`), then there are no emissions for
#'   electric vehicles.
#'
#' @param tb_vmt [tibble::tibble()], output VMT table
#' @param .mode character, given transportation mode.
#' @param .fuel_type character, fuel type for given mode.
#' @param .miles_per_gallon numeric, miles per gallon for mode.
#' @param .fuel_economy table, table with GHG factor values. Default is `ghg.ccap::fuel_economy`
#'
#' @inheritParams run_module_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams filter_ctu
#' @inheritParams run_scenario_building
#'
#' @return [tibble::tibble()] with column names
#'     - `type`
#'     - `scenario`
#'     - `mode`
#'     - `class`
#'     - `dir_ghg` numeric, direct greenhouse gas emissions in metric tons
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
                            .grid_decarbonization_pct = 0.6,
                            .fuel_economy = ghg.ccap::fuel_economy,
                            .enviro_factors = ghg.ccap::enviro_factors,
                            .factor_values = ghg.ccap::factor_values) {
  check_inputs(name = "fuel_type", value = .fuel_type)
  # for given fuel type,
  # find the number of metric tons (tonnes) CO2 per gallon of fuel
  ghg_factors_current <- .factor_values$ghg %>%
    dplyr::filter(source == .fuel_type) %>%
    dplyr::select(source, year,
      ghg_factor = value
    )


  fuel_gallons <- calc_fuel_use(
    tb_vmt,
    tb = tb,
    .mode = .mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = .miles_per_gallon,
    .fuel_economy = .fuel_economy,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )

  if (.fuel_type == "ER" & .grid_decarbonization_pct == 0) {
    cli::cli_warn(
      "No grid de-carbonization present - all BEV fuel evaluated on a 100% carbonized electrial grid "
    )
  }

  # spread grid decarbonization across intermediate years
  grid_elast <-
    tibble(
      year = unique(tb_vmt$year),
      grid_decarb =
        c(calc_elasticity(
          elas_list = c(rep(0, length(unique(tb_vmt$year)))),
          elas = .grid_decarbonization_pct,
          num_inits = 3,
          num_yrs = length(unique(tb_vmt$year)) - 5
        )[1:7], .grid_decarbonization_pct, .grid_decarbonization_pct)
    )


  ghg <-
    dplyr::left_join(
      fuel_gallons, grid_elast,
      by = c("year")
    ) %>%
    dplyr::left_join(ghg_factors_current,
      by = c("year")
    ) %>%
    dplyr::mutate(
      # emissions  = gallons * ghg_factor
      dir_ghg =
        dplyr::if_else(
          (mode == "PLDV" & class == "BEV"),
          (fuel_use_gallons_kwh * ghg_factor * (1 - grid_decarb)),
          (fuel_use_gallons_kwh * ghg_factor)
        )
    ) %>%
    dplyr::select(
      type,
      # source,
      scenario,
      mode,
      class,
      geog_name, geog_id,
      year,
      # aeo_scen,
      aeo_mode,
      # vmt,
      dir_ghg
    ) %>%
    unique()


  return(ghg)
}
