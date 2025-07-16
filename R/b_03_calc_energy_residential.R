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
#' @param .sf_heat_pump_pct numeric, percent of single family homes converting to heat pumps
#' @param .mf_heat_pump_pct numeric, percent of multifamily homes converting to heat pumps
#' @param .mwh_coefficients table, table with megawatt hour coefficients. Default is
#'   `ghg.ccap::mwh_coefficients`
#' @param .mcf_coefficients table, table with natural gas cubic feet coefficients. Default is
#'   `ghg.ccap::mcf_coefficients`
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
                                    .sf_heat_pump_pct,
                                    .mf_heat_pump_pct,
                                    .baseline_year,
                                    .scenario = "alt",
                                    .selected_ctu,
                                    .heatpump_start_year,
                                    .mwh_coefficients = ghg.ccap::mwh_coefficients,
                                    .mcf_coefficients = ghg.ccap::mcf_coefficients,
                                    .enviro_factors = ghg.ccap::enviro_factors) {
  # cli::cli_progress_message("*** calculating residential ghg emissions \n")

  check_inputs(name = "single_family_heat_pump_pct", .sf_heat_pump_pct)
  check_inputs(name = "multifamily_heat_pump_pct", .mf_heat_pump_pct)
  check_inputs(name = "heatpump_start_year", .heatpump_start_year)

  # browser()

  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)
  res_tb_bau <- filter_ctu(res_tb_bau, .selected_ctu = .selected_ctu) %>%
    mutate(effective_unit_change = 0)

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

  ### adjsut the model prediction to the sum of the last 5 observed years
  mwh_adjustment <-
    (baseline_energy %>%
      filter(inventory_year >= (.baseline_year - 4)) %>%
      pull(mwh) %>%
      sum()) /
      (res_tb_bau %>%
        filter(inventory_year >= (.baseline_year - 4) & inventory_year <= .baseline_year) %>%
        left_join(.mwh_coefficients,
          by = c("sp_categories" = "var")
        ) %>%
        mutate(mwh_pred = value * mwh_per_unit_eia) %>%
        pull(mwh_pred) %>%
        sum())

  mcf_adjustment <-
    (baseline_energy %>%
      filter(inventory_year >= (.baseline_year - 4)) %>%
      pull(mcf) %>%
      sum()) /
      (res_tb_bau %>%
        filter(inventory_year >= (.baseline_year - 4) & inventory_year <= .baseline_year) %>%
        left_join(.mcf_coefficients,
          by = c("sp_categories" = "var")
        ) %>%
        mutate(mcf_pred = value * mcf_per_unit_eia) %>%
        pull(mcf_pred) %>%
        sum())

  ### calculate heat pump effects here
  energy_calc <- function(tb,
                          .mwh_coefficients = .mwh_coefficients,
                          .mcf_coefficients = .mcf_coefficients,
                          .heatpump_start_year = .heatpump_start_year,
                          .sf_heat_pump_pct = .sf_heat_pump_pct,
                          .mf_heat_pump_pct = .mf_heat_pump_pct) {
    # browser()
    energy_tb <- tb %>%
      filter(inventory_year > .baseline_year) %>%
      left_join(.mwh_coefficients,
        by = c("sp_categories" = "var")
      ) %>%
      left_join(.mcf_coefficients,
        by = c("sp_categories" = "var")
      ) %>%
      mutate(
        effective_units = value + effective_unit_change,
        residential_mwh = case_when(
          inventory_year < .heatpump_start_year ~
            mwh_adjustment * mwh_per_unit_eia * effective_units,
          grepl("single", sp_categories) &
            inventory_year >= .heatpump_start_year ~
            # homes with natural gas
            mwh_adjustment * mwh_per_unit_eia * (effective_units * (1 - .sf_heat_pump_pct)) +
            # homes with heat pumps
            # little wonky due to city-level adjustment, better would be to have a heat pump mwh number to add pre-loaded
            (mwh_adjustment * mwh_per_unit_eia + # municipality adjustment
              (mwh_per_unit_heat_pump - mwh_per_unit_eia)) * # heat pump add-on
              (effective_units * .sf_heat_pump_pct),
          grepl("multi", sp_categories) &
            inventory_year >= .heatpump_start_year ~
            # homes with natural gas
            mwh_adjustment * mwh_per_unit_eia * (effective_units * (1 - .mf_heat_pump_pct)) +
            # homes with heat pumps
            mwh_per_unit_heat_pump * (effective_units * .mf_heat_pump_pct)
        ),
        residential_mcf = case_when(
          inventory_year < .heatpump_start_year ~
            mcf_adjustment * mcf_per_unit_eia * effective_units,
          grepl("single", sp_categories) &
            inventory_year >= .heatpump_start_year ~
            # homes with natural gas
            mcf_adjustment * mcf_per_unit_eia * (effective_units * (1 - .sf_heat_pump_pct)) +
            # homes with heat pumps
            mcf_adjustment * mcf_per_unit_heat_pump * (effective_units * .sf_heat_pump_pct),
          grepl("multi", sp_categories) &
            inventory_year >= .heatpump_start_year ~
            # homes with natural gas
            mcf_adjustment * mcf_per_unit_eia * (effective_units * (1 - .mf_heat_pump_pct)) +
            # homes with heat pumps
            mcf_adjustment * mcf_per_unit_heat_pump * (effective_units * .mf_heat_pump_pct)
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


  energy_bau <- bind_rows(
    baseline_energy %>%
      select(geog_name,
        geog_id,
        inventory_year,
        residential_mwh = mwh,
        residential_mcf = mcf
      ),
    energy_calc(
      tb = res_tb_bau,
      .mwh_coefficients = .mwh_coefficients,
      .mcf_coefficients = .mcf_coefficients,
      .heatpump_start_year = .heatpump_start_year,
      .sf_heat_pump_pct = 0,
      .mf_heat_pump_pct = 0
    )
  )


  energy_strategy <- bind_rows(
    baseline_energy %>%
      select(geog_name,
        geog_id,
        inventory_year,
        residential_mwh = mwh,
        residential_mcf = mcf
      ),
    energy_calc(
      tb = res_tb,
      .mwh_coefficients = .mwh_coefficients,
      .mcf_coefficients = .mcf_coefficients,
      .heatpump_start_year = .heatpump_start_year,
      .sf_heat_pump_pct = .sf_heat_pump_pct,
      .mf_heat_pump_pct = .mf_heat_pump_pct
    )
  )

  energy_final <- bind_rows(
    energy_bau %>%
      mutate(scenario = "bau"),
    energy_strategy %>%
      mutate(scenario = .scenario)
  )

  return(energy_final)
}
