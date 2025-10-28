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
                                    .heatpump_end_year,
                                    .enviro_factors = ghg.ccap::enviro_factors) {
  # cli::cli_progress_message("*** calculating residential ghg emissions \n")

  check_inputs(name = "single_family_heat_pump_pct", .sf_heat_pump_pct)
  check_inputs(name = "multifamily_heat_pump_pct", .mf_heat_pump_pct)
  check_inputs(name = "heatpump_start_year", .heatpump_start_year)

  # browser()

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

  ctu_energy_profile <- calc_building_energy(.selected_ctu = .selected_ctu) %>%
    mutate(cat_match = case_when(
      scenario == "baseline" ~ "existing_nonretrofit",
      scenario == "retrofit" ~ "retrofit_units",
      scenario == "new_build" ~ "new_non_leed",
      TRUE ~ scenario
    ))

  ### adjust the model prediction to the sum of the last 5 observed years
  mwh_adjustment <-
    (baseline_energy %>%
      filter(inventory_year >= (.baseline_year - 4)) %>%
      pull(mwh) %>%
      sum()) /
      (res_tb_bau %>%
        filter(inventory_year >= (.baseline_year - 4) & inventory_year <= .baseline_year) %>%
        distinct(geog_name, sp_categories, inventory_year, value) %>%
        left_join(
          ctu_energy_profile %>%
            filter(scenario == "baseline"),
          by = c("sp_categories" = "mc_classification")
        ) %>%
        mutate(mwh_pred = value * scenario_mwh) %>%
        pull(mwh_pred) %>%
        sum())

  mcf_adjustment <-
    (baseline_energy %>%
      filter(inventory_year >= (.baseline_year - 4)) %>%
      pull(mcf) %>%
      sum()) /
      (res_tb_bau %>%
        filter(inventory_year >= (.baseline_year - 4) & inventory_year <= .baseline_year) %>%
        distinct(geog_name, sp_categories, inventory_year, value) %>%
        left_join(
          ctu_energy_profile %>%
            filter(scenario == "baseline"),
          by = c("sp_categories" = "mc_classification")
        ) %>%
        mutate(mcf_pred = value * scenario_mcf) %>%
        pull(mcf_pred) %>%
        sum())

  # calculate expected energy load for cities here
  # heat pump expected energy will be lowered for retrofit homes
  # ctu average energy load will be split based on heat pump percentage

  ctu_energy_profile_adjustments <- ctu_energy_profile %>%
    unique() %>%
    tidyr::pivot_wider(
      id_cols = mc_classification,
      names_from = scenario,
      values_from = c(scenario_mwh, scenario_mcf),
      names_glue = "{scenario}_{.value}",
      # if multiple values, use minimum
      values_fn = min
    ) %>%
    mutate(
      heatpump_mwh = heatpump_scenario_mwh - baseline_scenario_mwh, # heat pump scen mwh addition to baseline is assumed to be all heating gain
      retrofit_heating_pct = (retrofit_scenario_mcf - heatpump_scenario_mcf) / # calculate what amount of nat gas was for heating in retrofit
        (baseline_scenario_mcf - heatpump_scenario_mcf),
      appliance_mcf = heatpump_scenario_mcf # how much nat gas used when no heating required?
    ) %>%
    select(
      mc_classification,
      heatpump_mwh,
      retrofit_heating_pct,
      appliance_mcf
    )

  ### TEMPORARY LEED ADD-ON UNTIL BETTER DATA IS AVAILABILE
  ctu_energy_profile <- bind_rows(
    ctu_energy_profile,
    ctu_energy_profile %>%
      filter(scenario == "new_build") %>%
      mutate(
        scenario = "new_leed_build",
        cat_match = "new_leed"
      )
  )

  ### calculate heat pump effects here
  energy_calc <- function(tb,
                          .heatpump_start_year = .heatpump_start_year,
                          .heatpump_end_year = .heatpump_end_year,
                          .sf_heat_pump_pct = .sf_heat_pump_pct,
                          .mf_heat_pump_pct = .mf_heat_pump_pct) {
    ### ramp up heat pump installation evenly from start year to end year

    ramp_years <- .heatpump_start_year:.heatpump_end_year
    n_ramp <- length(ramp_years)

    pct_ramp <- tibble::tibble(
      inventory_year = ramp_years,
      hp_sf_pct = seq(
        from = .sf_heat_pump_pct / n_ramp,
        to = .sf_heat_pump_pct,
        length.out = n_ramp
      ),
      hp_mf_pct = seq(
        from = .mf_heat_pump_pct / n_ramp,
        to = .mf_heat_pump_pct,
        length.out = n_ramp
      )
    )

    # Join pct values by condition
    pct_by_year <- tibble::tibble(inventory_year = 2005:2050) %>%
      left_join(pct_ramp, by = "inventory_year") %>%
      dplyr::mutate(
        hp_sf_pct = dplyr::case_when(
          inventory_year < .heatpump_start_year ~ 0,
          inventory_year > .heatpump_end_year ~ .sf_heat_pump_pct,
          TRUE ~ hp_sf_pct
        ),
        hp_mf_pct = dplyr::case_when(
          inventory_year < .heatpump_start_year ~ 0,
          inventory_year > .heatpump_end_year ~ .mf_heat_pump_pct,
          TRUE ~ hp_mf_pct
        )
      )

    # browser()
    energy_tb <- tb %>%
      filter(inventory_year > .baseline_year) %>%
      left_join(pct_by_year, by = "inventory_year") %>%
      mutate(hp_pct = if_else(grepl("multi", sp_categories),
        hp_mf_pct,
        hp_sf_pct
      )) %>%
      left_join(ctu_energy_profile,
        by = c(
          "sp_categories" = "mc_classification",
          "efficiency_description" = "cat_match"
        )
      ) %>%
      left_join(ctu_energy_profile_adjustments,
        by = c("sp_categories" = "mc_classification")
      ) %>%
      mutate(
        residential_mwh = case_when( # will take the weighted average of heatpump/non-heatpump homes
          efficiency_description == "existing_nonretrofit" ~
            ((scenario_mwh * (1 - hp_pct)) + ((scenario_mwh + heatpump_mwh) * hp_pct)) * efficiency_unit_value * mwh_adjustment,
          efficiency_description == "retrofit_units" ~
            ((scenario_mwh * (1 - hp_pct)) + ((scenario_mwh + (heatpump_mwh * retrofit_heating_pct)) * hp_pct)) * efficiency_unit_value * mwh_adjustment,
          efficiency_description == "new_non_leed" ~
            ((scenario_mwh * (1 - hp_pct)) + ((scenario_mwh + heatpump_mwh) * hp_pct)) * efficiency_unit_value * mwh_adjustment,
          efficiency_description == "new_leed" ~
            (((scenario_mwh * (1 - hp_pct)) + ((scenario_mwh + (heatpump_mwh)) * hp_pct))) * .enviro_factors$LEED_GOLD_REDUCTION_PCT * efficiency_unit_value * mwh_adjustment
        ),
        residential_mcf = case_when(
          efficiency_description == "existing_nonretrofit" ~
            ((scenario_mcf * (1 - hp_pct)) + (appliance_mcf * hp_pct)) * efficiency_unit_value * mcf_adjustment,
          efficiency_description == "retrofit_units" ~
            ((scenario_mcf * (1 - hp_pct)) + (appliance_mcf * hp_pct)) * efficiency_unit_value * mcf_adjustment, # this might be overselling non_heating_perc doesn't take into account efficient
          efficiency_description == "new_non_leed" ~
            ((scenario_mcf * (1 - hp_pct)) + (appliance_mcf * hp_pct)) * efficiency_unit_value * mcf_adjustment,
          efficiency_description == "new_leed" ~
            (((scenario_mcf * (1 - hp_pct)) * .enviro_factors$LEED_GOLD_REDUCTION_PCT + (appliance_mcf * hp_pct))) * efficiency_unit_value * mcf_adjustment
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
      .heatpump_start_year = .heatpump_start_year,
      .heatpump_end_year = .heatpump_end_year,
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
      .heatpump_start_year = .heatpump_start_year,
      .heatpump_end_year = .heatpump_end_year,
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
