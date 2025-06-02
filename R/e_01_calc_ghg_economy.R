#' @title Calculate electricity emissions
#' @family buildings
#' @family transportation
#' @family residential
#' @family business
#' @family emissions
#'
#' @description Collect electricity demand from all sectors and calculate emissions
#'      based on electrical grid emission factors.
#'
#' @note `calc_ghg_residential()` estimates the building energy demand and emissions
#'      based on the floor area assumptions. For a function that compiles all
#'      residential strategies refer to [`scen_residential_building()`].
#'
#' @param res_tb [tibble::tibble()].
#'      Table, table with residential building data.
#' @inheritParams run_module_transportation
#' @inheritParams scen_building_residential
#'
#' @return [tibble::tibble()].
#'    A table with columns
#'    `ctu_name`,
#'    `year`,
#'    `population`,
#'    `residential_mwh`,
#'    `residential_electricity_emissions_mt_co2e`,
#'    `business_mwh`, and
#'    `business_electricity_emissions_mt_co`
#'    `transportation_mwh`, and
#'    `transportation_electricity_emissions_mt_co2e`
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
#'
calc_ghg_mwh <- function(res_mwh,
                         res_mwh_bau = building_energy_data$electricity_residential_ctu,
                         comm_mwh,
                         comm_mwh_bau = building_energy_data$electricity_business_ctu,
                         vmt_mwh,
                         vmt_mwh_bau,
                         grid_emissions = grid_emissions,
                         grid_scenario = "MISO",
                         .selected_ctu,
                         .grid_decarbonization_estimator,
                         .enviro_factors = ghg.ccap::enviro_factors) {
  # cli::cli_progress_message("*** calculating grid ghg emissions \n")

  res_mwh <- filter_ctu(res_mwh, .selected_ctu = .selected_ctu)
  res_mwh_bau <- filter_ctu(res_mwh_bau, .selected_ctu = .selected_ctu)

  comm_mwh <- filter_ctu(comm_mwh, .selected_ctu = .selected_ctu)
  comm_mwh_bau <- filter_ctu(comm_mwh_bau, .selected_ctu = .selected_ctu)

  vmt_mwh <- filter_ctu(vmt_mwh, .selected_ctu = .selected_ctu)
  vmt_mwh_bau <- filter_ctu(vmt_mwh_bau, .selected_ctu = .selected_ctu)

  grid_emissions <- grid_emissions %>%
    filter(inventory_year <= 2024 | grepl(grid_scenario, factor_source))

  emis_bau <- bind_rows(
    res_mwh_bau,
    # vmt_mwh_bau,
    comm_mwh_bau
  ) %>%
    left_join(grid_emissions,
      by = "inventory_year"
    ) %>%
    mutate(mt_co2e = mwh * mt_co2e_per_mwh) %>%
    select(ctu_name, ctu_class, sector, inventory_year, mwh, mt_co2e)

  emis_strategy <- bind_rows(
    res_mwh_bau,
    # vmt_mwh_bau,
    comm_mwh_bau
  ) %>%
    left_join(
      grid_emissions,
      bau_tb
    ) %>%
    mutate(mt_co2e = mwh * mt_co2e_per_mwh) %>%
    select(ctu_name, ctu_class, sector, inventory_year, mwh, mt_co2e)

  emis_final <-
    dplyr::right_join(
      emis_bau,
      emis_strategy,
      by = c("ctu_name", "inventory_year"),
      suffix = c(".bau", ".scen")
    ) %>%
    tidyr::pivot_longer(
      names_to = "var",
      values_to = "value",
      cols = -c(ctu_name, year)
    ) %>%
    tidyr::separate(
      col = var,
      into = c("var", "scen"),
      sep = "\\."
    ) %>%
    dplyr::ungroup()

  return(emis_final)
}
