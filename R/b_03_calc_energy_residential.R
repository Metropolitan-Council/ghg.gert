#' @title Calculate residential building mwh
#' @family buildings
#' @family residential
#' @family emissions
#'
#' @description Estimates total energy demand
#'      from the residential building sector by city/township
#'      for the user-specified scenario, and the business-as-usual scenario.
#'
#' @note `calc_energy_residential()` estimates the building energy demand
#'      based on the housing efficiency assumptions. For a function that compiles all
#'      residential strategies refer to [`scen_residential_building()`].
#'
#' @param res_tb [tibble::tibble()].
#'      Table, table with residential building data.
#' @inheritParams run_module_transportation
#' @inheritParams scen_building_residential
#'
#' @return [tibble::tibble()].
#'    A table with columns
#'    `geog_name`,
#'    `inventory_year`,
#'    `geog_id`,
#'    `residential_mwh`,
#'    `residential_mcf`,
#'    `scenario`
#'
#' @examples
#' \dontrun{
#' library(ghg.ccap)
#'
#' calc_ghg_residential(
#'   res_tb = building_data$residential,
#'   res_tb_bau = building_data$residential,
#'   .selected_ctu = "all",
#'   .grid_decarbonization_pct = 1,
#'   .enviro_factors = enviro_factors
#' )
#' }
#' @export
calc_energy_residential <- function(res_tb,
                                    res_tb_bau,
                                    .scenario = "",
                                    .sf_heat_pump_pct,
                                    .mf_heat_pump_pct,
                                    .selected_ctu = .selected_ctu,
                                    .mwh_coefficients = ghg.ccap::mwh_coefficients,
                                    .mcf_coefficients = ghg.ccap::mcf_coefficients,
                                    .enviro_factors = ghg.ccap::enviro_factors) {
  # cli::cli_progress_message("*** calculating residential ghg emissions \n")

  # browser()

  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)
  res_tb_bau <- filter_ctu(res_tb_bau, .selected_ctu = .selected_ctu) %>%
    mutate(effective_unit_change = 0)

  ### calculate heat pump effects here
  energy_calc <- function(tb,
                          .mwh_coefficients = .mwh_coefficients,
                          .mcf_coefficients = .mcf_coefficients,
                          .sf_heat_pump_pct = .sf_heat_pump_pct,
                          .mf_heat_pump_pct = .mf_heat_pump_pct) {
    # browser()
    energy_tb <- tb %>%
      left_join(.mwh_coefficients,
        by = c("sp_categories" = "var")
      ) %>%
      left_join(.mcf_coefficients,
        by = c("sp_categories" = "var")
      ) %>%
      mutate(
        effective_units = value + effective_unit_change,
        residential_mwh = case_when(
          grepl("single", sp_categories) ~
            # homes with natural gas
            mwh_per_unit_eia * (effective_units * (1 - .sf_heat_pump_pct)) +
            # homes with heat pumps
            mwh_per_unit_heat_pump * (effective_units * .sf_heat_pump_pct),
          grepl("multi", sp_categories) ~
            # homes with natural gas
            mwh_per_unit_eia * (effective_units * (1 - .mf_heat_pump_pct)) +
            # homes with heat pumps
            mwh_per_unit_heat_pump * (effective_units * .mf_heat_pump_pct)
        ),
        residential_mcf = case_when(
          grepl("single", sp_categories) ~
            # homes with natural gas
            mcf_per_unit_eia * (effective_units * (1 - .sf_heat_pump_pct)) +
            # homes with heat pumps
            mcf_per_unit_heat_pump * (effective_units * .sf_heat_pump_pct),
          grepl("multi", sp_categories) ~
            # homes with natural gas
            mcf_per_unit_eia * (effective_units * (1 - .mf_heat_pump_pct)) +
            # homes with heat pumps
            mcf_per_unit_heat_pump * (effective_units * .mf_heat_pump_pct)
        )
      ) %>%
      dplyr::group_by(geog_name, geog_id, inventory_year) %>%
      dplyr::summarize(
        residential_mwh = sum(residential_mwh),
        residential_mcf = sum(residential_mcf)
      ) %>%
      dplyr::select(
        geog_name,
        inventory_year,
        geog_id,
        residential_mwh,
        residential_mcf
      )

    return(energy_tb)
  }


  energy_bau <-
    energy_calc(
      tb = res_tb_bau,
      .mwh_coefficients = .mwh_coefficients,
      .mcf_coefficients = .mcf_coefficients,
      .sf_heat_pump_pct = 0,
      .mf_heat_pump_pct = 0
    )

  energy_strategy <-
    energy_calc(
      tb = res_tb,
      .mwh_coefficients = .mwh_coefficients,
      .mcf_coefficients = .mcf_coefficients,
      .sf_heat_pump_pct = .sf_heat_pump_pct,
      .mf_heat_pump_pct = .mf_heat_pump_pct
    )

  energy_final <- bind_rows(
    energy_bau %>%
      mutate(scenario = "bau"),
    energy_strategy %>%
      mutate(scenario = .scenario)
  )

  return(energy_final)
}
