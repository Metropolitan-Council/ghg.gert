#' @title Calculate non-residential building energy
#' @family buildings
#' @family non-residential
#' @family emissions
#'
#' @description Estimates total energy demand
#'      from the non-residential building sector by city/township
#'      for the user-specified scenario, and the business-as-usual scenario.
#'
#' @note `calc_energy_non_residential()` estimates the building energy demand
#'      based on the housing efficiency assumptions. For a function that compiles all
#'      nonresidential strategies refer to [`scen_building_non_residential()`].
#'
#' @param non_res_tb [tibble::tibble()].
#'      Table, table with residential building data.
#'
#' @inheritParams run_module_transportation
#' @inheritParams scen_building_non_residential
#'
#' @return [tibble::tibble()].
#'    A table with columns
#'    `geog_name`,
#'    `inventory_year`,
#'    `geog_id`,
#'    `nonresidential_mwh`,
#'    `nonresidential_mcf`,
#'    `scenario`
#'
#' @examples
#' \dontrun{
#' library(ghg.ccap)
#'
#' calc_ghg_non_residential(
#'   res_tb = building_data$residential,
#'   res_tb_bau = building_data$residential,
#'   .selected_ctu = "all",
#'   .grid_decarbonization_pct = 1,
#'   .enviro_factors = enviro_factors
#' )
#' }
#' @export
calc_energy_non_residential <- function(non_res_tb,
                                        non_res_tb_bau,
                                        .heatpump_start_year,
                                        .heatpump_end_year,
                                        .baseline_year,
                                        .scenario = "alt",
                                        .selected_ctu,
                                        .jobs_heatpump_pct,
                                        .enviro_factors = ghg.ccap::enviro_factors) {
  # cli::cli_progress_message("*** calculating residential ghg emissions \n")

  check_inputs(name = "jobs_heatpump_pct", .jobs_heatpump_pct)
  check_inputs(name = "heatpump_start_year", .heatpump_start_year)


  non_res_tb <- filter_ctu(non_res_tb, .selected_ctu = .selected_ctu)
  non_res_tb_bau <- filter_ctu(non_res_tb_bau, .selected_ctu = .selected_ctu)

  # snag the .selected community designation -- breaks when .selected_city = "all" and just uses Afton/first city
  pluck_commDesgn <- non_res_tb_bau %>%
    summarise(val = first(imagine_designation)) %>%
    pull(val)

  baseline_energy <- left_join(
    filter_ctu(ghg.ccap::building_energy_data$electricity_inventory,
      .selected_ctu = .selected_ctu
    ) %>%
      dplyr::filter(
        inventory_year <= .baseline_year,
        sector == "Business"
      ),
    filter_ctu(ghg.ccap::building_energy_data$natgas_inventory,
      .selected_ctu = .selected_ctu
    ) %>%
      dplyr::filter(
        inventory_year <= .baseline_year,
        sector == "Business"
      ),
    by = join_by(geog_name, geog_id, geog_level, sector, inventory_year)
  )

  # make scenario_comm_des available for use in ghg.ccap.app
  utils::data(
    "scenario_comm_des",
    package = "ghg.ccap",
    envir   = environment()
  )

  # Pull relevant community designation's energy profile for .selected_ctu
  ctu_energy_profile <- scenario_comm_des %>%
    filter(imagine_designation == pluck_commDesgn) %>%
    mutate(cat_match = case_when(
      scenario == "baseline" ~ "existing_nonretrofit_jobs",
      scenario == "retrofit_efficiency" ~ "retrofit_jobs",
      scenario == "new_build" ~ "new_non_leed_jobs",
      scenario == "electrification" ~ "heatpump_jobs",
      scenario == "new_build_efficient" ~ "new_leed_jobs"
    ))

  ### adjust the model prediction to the sum of the last 5 observed years
  mwh_adjustment <-
    (baseline_energy %>%
      filter(inventory_year >= (.baseline_year - 4)) %>%
      pull(mwh) %>%
      sum()) /
      (non_res_tb_bau %>%
        filter(inventory_year >= (.baseline_year - 4) & inventory_year <= .baseline_year) %>%
        dplyr::distinct(geog_name, imagine_designation, inventory_year, value) %>%
        left_join(
          ctu_energy_profile %>%
            filter(scenario == "baseline"),
          by = "imagine_designation"
        ) %>%
        mutate(mwh_pred = value * mwh_per_job) %>%
        pull(mwh_pred) %>%
        sum())

  mcf_adjustment <-
    (baseline_energy %>%
      filter(inventory_year >= (.baseline_year - 4)) %>%
      pull(mcf) %>%
      sum()) /
      (non_res_tb_bau %>%
        filter(inventory_year >= (.baseline_year - 4) & inventory_year <= .baseline_year) %>%
        dplyr::distinct(geog_name, imagine_designation, inventory_year, value) %>%
        left_join(
          ctu_energy_profile %>%
            filter(scenario == "baseline"),
          by = "imagine_designation"
        ) %>%
        mutate(mcf_pred = value * mcf_per_job) %>%
        pull(mcf_pred) %>%
        sum())


  # heat pump expected energy will be lowered for retrofit buildings
  # ctu average energy load will be split based on heat pump percentage
  ctu_energy_profile_adjustments <- ctu_energy_profile %>%
    select(-cat_match) %>%
    tidyr::pivot_wider(
      names_from = scenario,
      values_from = c(mwh_per_job, mcf_per_job),
      names_glue = "{scenario}_{.value}"
    ) %>%
    mutate(
      heatpump_mwh = electrification_mwh_per_job - baseline_mwh_per_job, # heat pump scen mwh addition to baseline is assumed to be all heating gain
      retrofit_heating_pct = (retrofit_efficiency_mcf_per_job - electrification_mcf_per_job) / # calculate what amount of nat gas was for heating in retrofit
        (baseline_mcf_per_job - electrification_mcf_per_job),
      appliance_mcf = electrification_mcf_per_job # how much nat gas used when no heating required?
    ) %>%
    select(
      imagine_designation,
      heatpump_mwh,
      retrofit_heating_pct,
      appliance_mcf
    )


  ### calculate heat pump effects here
  energy_calc <- function(tb,
                          .heatpump_start_year = .heatpump_start_year,
                          .heatpump_end_year = .heatpump_end_year,
                          .jobs_heatpump_pct = .jobs_heatpump_pct) {
    ### ramp up heat pump installation evenly from start year to end year
    ramp_years <- .heatpump_start_year:.heatpump_end_year
    n_ramp <- length(ramp_years)

    pct_ramp <- tibble::tibble(
      inventory_year = ramp_years,
      hp_pct = seq(
        from = .jobs_heatpump_pct / n_ramp,
        to = .jobs_heatpump_pct,
        length.out = n_ramp
      )
    )

    # Join pct values by condition
    pct_by_year <- tibble::tibble(inventory_year = 2005:2050) %>%
      left_join(pct_ramp, by = "inventory_year") %>%
      dplyr::mutate(
        hp_pct = dplyr::case_when(
          inventory_year < .heatpump_start_year ~ 0,
          inventory_year > .heatpump_end_year ~ .jobs_heatpump_pct,
          TRUE ~ hp_pct
        )
      )

    energy_tb <- tb %>%
      filter(inventory_year > .baseline_year) %>%
      left_join(pct_by_year, by = "inventory_year") %>%
      left_join(ctu_energy_profile,
        by = join_by(
          efficiency_description == cat_match,
          imagine_designation == imagine_designation
        )
      ) %>%
      left_join(ctu_energy_profile_adjustments,
        by = join_by("imagine_designation")
      ) %>%
      mutate(
        non_residential_mwh = case_when( # electrification will be split amongst retrofit and non-retrofit units
          efficiency_description == "existing_nonretrofit_jobs" ~
            ((mwh_per_job * (1 - hp_pct)) + ((mwh_per_job + heatpump_mwh) * hp_pct)) * efficiency_unit_value * mwh_adjustment,
          efficiency_description == "retrofit_jobs" ~
            ((mwh_per_job * (1 - hp_pct)) + ((mwh_per_job + (heatpump_mwh * retrofit_heating_pct)) * hp_pct)) * efficiency_unit_value * mwh_adjustment,
        # new efficient builds are electrified AND retrofit so can be calculated directly with the adjustment
          efficiency_description == "new_non_leed_jobs" ~
            mwh_per_job * efficiency_unit_value * mwh_adjustment,
        efficiency_description == "new_leed_jobs" ~
          mwh_per_job * efficiency_unit_value * mwh_adjustment
        ),
        non_residential_mcf = case_when(
          efficiency_description  == "existing_nonretrofit_jobs" ~
            ((mcf_per_job * (1 - hp_pct)) + (appliance_mcf * hp_pct)) * efficiency_unit_value * mcf_adjustment,
          efficiency_description == "retrofit_jobs" ~
            ((mcf_per_job * (1 - hp_pct)) + (appliance_mcf * hp_pct)) * efficiency_unit_value * mcf_adjustment,
          efficiency_description == "new_non_leed_jobs" ~
            mcf_per_job * efficiency_unit_value * mcf_adjustment,
          efficiency_description == "new_leed_jobs" ~
            mcf_per_job * efficiency_unit_value * mcf_adjustment)
      ) %>%
      dplyr::group_by(geog_name, geog_id, inventory_year) %>%
      dplyr::summarize(
        non_residential_mwh = sum(non_residential_mwh),
        non_residential_mcf = sum(non_residential_mcf)
      ) %>%
      dplyr::select(
        geog_name,
        inventory_year,
        geog_id,
        non_residential_mwh,
        non_residential_mcf
      )

    return(energy_tb)
  }


  energy_bau <- bind_rows(
    baseline_energy %>%
      select(geog_name,
        geog_id,
        inventory_year,
        non_residential_mwh = mwh,
        non_residential_mcf = mcf
      ),
    energy_calc(
      tb = non_res_tb_bau,
      .heatpump_start_year = .heatpump_start_year,
      .heatpump_end_year = .heatpump_end_year,
      .jobs_heatpump_pct = 0
    )
  )


  energy_strategy <- bind_rows(
    baseline_energy %>%
      select(geog_name,
        geog_id,
        inventory_year,
        non_residential_mwh = mwh,
        non_residential_mcf = mcf
      ),
    energy_calc(
      tb = non_res_tb,
      .heatpump_start_year = .heatpump_start_year,
      .heatpump_end_year = .heatpump_end_year,
      .jobs_heatpump_pct = .jobs_heatpump_pct
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


