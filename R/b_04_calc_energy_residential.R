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
#'      residential strategies refer to [`scen_building_residential()`].
#'
#' @param res_tb [tibble::tibble()].
#'      Table, table with residential building data.
#' @param .sf_heatpump_pct numeric, percent of single family homes converting to heat pumps
#' @param .mf_heatpump_pct numeric, percent of multifamily homes converting to heat pumps
#'
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
                                    .baseline_year,
                                    .scenario = "alt",
                                    .selected_ctu) {
  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)
  res_tb_bau <- filter_ctu(res_tb_bau, .selected_ctu = .selected_ctu)

  baseline_energy <- left_join(
    filter_ctu(ghg.ccap::building_energy_data$electricity_inventory,
      .selected_ctu = .selected_ctu
    ) %>%
      dplyr::filter(
        inventory_year <= .baseline_year,
        sector == "Residential"
      ),
    filter_ctu(ghg.ccap::building_energy_data$natgas_inventory,
      .selected_ctu = .selected_ctu
    ) %>%
      dplyr::filter(
        inventory_year <= .baseline_year,
        sector == "Residential"
      ),
    by = join_by(geog_name, geog_id, geog_level, sector, inventory_year)
  )
  # browser()
  # ctu specific energy profiles
  ctu_energy_profile <- calc_building_energy(.selected_ctu = .selected_ctu)

  # --- calibration: align model to last 5 observed years ---------------
  bau_baseline_units <- res_tb_bau %>%
    dplyr::filter(
      inventory_year >= (.baseline_year - 4),
      inventory_year <= .baseline_year
    ) %>%
    dplyr::distinct(geog_name, sp_categories, inventory_year, allocated_units) %>%
    left_join(
      dplyr::filter(ctu_energy_profile, scenario == "baseline"),
      by = c("sp_categories" = "mc_classification")
    )

  observed_recent <- dplyr::filter(baseline_energy, inventory_year >= (.baseline_year - 4))

  mwh_adjustment <- sum(observed_recent$mwh) /
    sum(bau_baseline_units$allocated_units * bau_baseline_units$scenario_mwh)

  mcf_adjustment <- sum(observed_recent$mcf) /
    sum(bau_baseline_units$allocated_units * bau_baseline_units$scenario_mcf)

  # calculate expected energy load for cities here
  # heat pump expected energy will be lowered for retrofit homes
  # ctu average energy load will be split based on heat pump percentage

  compute_energy <- function(tb) {
    tb %>%
      dplyr::filter(inventory_year > .baseline_year) %>%
      left_join(
        ctu_energy_profile,
        by = c("sp_categories" = "mc_classification", "scenario")
      ) %>%
      dplyr::mutate(
        residential_mwh = allocated_units * scenario_mwh * mwh_adjustment,
        residential_mcf = allocated_units * scenario_mcf * mcf_adjustment
      ) %>%
      dplyr::group_by(geog_name, geog_id, inventory_year) %>%
      dplyr::summarize(
        residential_mwh = sum(residential_mwh, na.rm = TRUE),
        residential_mcf = sum(residential_mcf, na.rm = TRUE),
        .groups = "drop"
      )
  }

  baseline_rows <- baseline_energy %>%
    dplyr::select(geog_name, geog_id, inventory_year,
      residential_mwh = mwh, residential_mcf = mcf
    )

  energy_final <- dplyr::bind_rows(
    dplyr::bind_rows(baseline_rows, compute_energy(res_tb_bau)) %>%
      dplyr::mutate(scenario = "bau"),
    dplyr::bind_rows(baseline_rows, compute_energy(res_tb)) %>%
      dplyr::mutate(scenario = .scenario)
  )

  return(energy_final)
}
