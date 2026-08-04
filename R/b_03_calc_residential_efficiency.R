#' @title Calculate new housing - LEED certified
#' @family buildings
#'
#' @description Calculates the efficiency of single-family homes
#'    built in accordance with LEED Gold standards, considering the proportion of
#'    new homes built to these standards, the difference in single-family
#'    housing units between 2021 and 2050, and the reduction in energy use
#'    intensity due to LEED Gold construction. This function is designed to
#'    estimate the impact of energy-efficient construction on residential
#'    greenhouse gas emissions.
#'
#' @param .new_sf_homes_leed_gold_pct numeric,  a value between `0` and `1`.
#'      The percentage of new single-family homes built according to *LEED Gold* standards.
#'      Default is `0.0`
#' @param .new_mf_homes_leed_gold_pct numeric,  a value between `0` and `1`.
#'      The percentage of new multi-family homes built according to *LEED Gold* standards.
#'      Default is `0.0`
#'
#' @inheritParams run_scenario_building
#' @inheritParams calc_parking_lot_land_cover
#' @inheritParams run_all_modules
#' @inheritParams run_scenario_land_use
#' @inheritParams filter_ctu
#'
#' @details
#'    Uses the expected growth of new housing according to UrbanSim estimates
#'
#' @return [tibble::tibble()].
#' @export
#'
#'
calc_housing_leed <- function(res_tb,
                              .selected_ctu,
                              .new_sf_homes_leed_gold_pct,
                              .new_mf_homes_leed_gold_pct,
                              .leed_start_year) {
  # cli::cli_progress_message("*** calculating floor area LEED Gold certification strategy \n")
  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)

  check_inputs(name = "new_sf_homes_leed_gold_pct", .new_sf_homes_leed_gold_pct)
  check_inputs(name = "new_mf_homes_leed_gold_pct", .new_mf_homes_leed_gold_pct)
  check_inputs(name = "leed_start_year", .leed_start_year)

  # if (.new_sf_homes_leed_gold_pct == 0) {
  #   #cli::cli_warn("No change in new single family home energy efficiency")
  #   new_sf <- res_tb %>%
  #     dplyr::filter(grepl("single", sp_categories)) %>%
  #     mutate(
  #       new_leed = 0,
  #       new_units = ifelse(value_change_from_base < 0, 0, value_change_from_base),
  #       effective_unit_change_leed = 0
  #     )
  # } else if (.new_sf_homes_leed_gold_pct != 0) {

  new_sf <- res_tb %>%
    dplyr::filter(grepl("single", sp_categories) | grepl("manufacture", sp_categories)) %>%
    dplyr::mutate(
      new_units = ifelse(value_change_from_base < 0, 0, value_change_from_base),
      new_leed = if_else(emissions_year < .leed_start_year,
        0,
        round(new_units * .new_sf_homes_leed_gold_pct)
      ),
      new_non_leed = new_units - new_leed
    ) %>%
    pivot_longer(
      cols = c(new_leed, new_non_leed),
      names_to = "efficiency_description",
      values_to = "efficiency_unit_value"
    )

  # }

  # if (.new_mf_homes_leed_gold_pct == 0) {
  #   cli::cli_warn("No change in new multifamily home energy efficiency")
  #   new_mf <- res_tb %>%
  #     dplyr::filter(grepl("multi", sp_categories)) %>%
  #     mutate(
  #       new_leed = 0,
  #       new_units = ifelse(value_change_from_base < 0, 0, value_change_from_base),
  #       effective_unit_change_leed = 0
  #     )
  # } else if (.new_mf_homes_leed_gold_pct != 0) {
  new_mf <- res_tb %>%
    dplyr::filter(grepl("multi", sp_categories)) %>%
    dplyr::mutate(
      new_units = ifelse(value_change_from_base < 0, 0, value_change_from_base),
      new_leed = if_else(emissions_year < .leed_start_year,
        0,
        round(new_units * .new_mf_homes_leed_gold_pct)
      ),
      new_non_leed = new_units - new_leed
    ) %>%
    pivot_longer(
      cols = c(new_leed, new_non_leed),
      names_to = "efficiency_description",
      values_to = "efficiency_unit_value"
    )
  # }

  leed_buildings <- bind_rows(
    new_sf %>%
      select(
        geog_name,
        geog_id,
        sp_categories,
        emissions_year,
        value,
        value_change_from_base,
        new_units,
        efficiency_description,
        efficiency_unit_value
      ),
    new_mf %>%
      select(
        geog_name,
        geog_id,
        sp_categories,
        emissions_year,
        value,
        value_change_from_base,
        new_units,
        efficiency_description,
        efficiency_unit_value
      )
  )

  return(leed_buildings)
}


#' @title Calculate housing retrofit
#' @family buildings
#'
#' @description Calculates number of homes targeted for retrofits based on user inputs
#' and CTU housing projections. This function must inherit an object from calc_housing_leed()
#'
#' @param .existing_home_retrofit_pct numeric,  a value between `0` and `1`.
#'      Percentage of existing homes retrofitted to reduce energy usage by *33%*.
#'      Default is `0.0`.
#' @param .existing_home_ultra_retrofit_pct numeric,  a value between `0` and `1`.
#'      Percentage of existing homes retrofitted to reduce energy usage by *66%*.
#'      Default is `0.00`.
#'
#' @inheritParams run_scenario_building
#' @inheritParams calc_parking_lot_land_cover
#' @inheritParams run_all_modules
#' @inheritParams run_scenario_land_use
#' @inheritParams filter_ctu
#'
#' @details Uses the average single family floor area in 2018
#'
#' @return [tibble::tibble()]
#'       A table with columns `ctu_name`, `year`, `var`, and `value`.
#'       Table contains adjusted `single_family_units`,
#'       `single_family_average_floor_area_sqft_ctu`, `multifamily_units`, and
#'       `multifamily_average_floor_area_sqft_county` records for column `var`
#'       when `year == 2040` relative to the residential inputs table.
#' @export
#'
#'
calc_residential_retrofit <- function(res_tb,
                                      .selected_ctu,
                                      .existing_sf_retrofit_pct,
                                      .existing_mf_retrofit_pct,
                                      .retrofit_start_year,
                                      .retrofit_end_year) {
  # cli::cli_progress_message("*** calculating floor area retrofit strategy \n")
  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)


  check_inputs(name = "existing_sf_retrofit_pct", .existing_sf_retrofit_pct)
  check_inputs(name = "existing_mf_retrofit_pct", .existing_mf_retrofit_pct)
  check_inputs(name = "retrofit_start_year", .retrofit_start_year)

  ### ramp up retrofits evenly from start year to end year

  ramp_years <- .retrofit_start_year:.retrofit_end_year
  n_ramp <- length(ramp_years)

  pct_ramp <- tibble::tibble(
    emissions_year = ramp_years,
    sf_pct = seq(
      from = .existing_sf_retrofit_pct / n_ramp,
      to = .existing_sf_retrofit_pct,
      length.out = n_ramp
    ),
    mf_pct = seq(
      from = .existing_mf_retrofit_pct / n_ramp,
      to = .existing_mf_retrofit_pct,
      length.out = n_ramp
    )
  )

  # Join pct values by condition
  pct_by_year <- tibble::tibble(emissions_year = 2005:2050) %>%
    left_join(pct_ramp, by = "emissions_year") %>%
    dplyr::mutate(
      sf_pct = dplyr::case_when(
        emissions_year < .retrofit_start_year ~ 0,
        emissions_year > .retrofit_end_year ~ .existing_sf_retrofit_pct,
        TRUE ~ sf_pct
      ),
      mf_pct = dplyr::case_when(
        emissions_year < .retrofit_start_year ~ 0,
        emissions_year > .retrofit_end_year ~ .existing_mf_retrofit_pct,
        TRUE ~ mf_pct
      )
    )


  # if (.existing_sf_retrofit_pct == 0) {
  #   cli::cli_warn("No change in existing single family home energy efficiency")
  #   existing_sf <- res_tb %>%
  #     dplyr::filter(grepl("single", sp_categories)) %>%
  #     mutate(
  #       retrofit_units = 0,
  #       effective_unit_change_retro = 0
  #     )
  # } else if (.existing_sf_retrofit_pct != 0) {
  existing_sf <- res_tb %>%
    dplyr::filter(grepl("single", sp_categories) | grepl("manufacture", sp_categories)) %>%
    left_join(pct_by_year %>% select(emissions_year, sf_pct),
      by = "emissions_year"
    ) %>%
    dplyr::mutate(
      new_units = if_else(value_change_from_base > 0, value_change_from_base, 0),
      existing_units = value - new_units,
      retrofit_units = if_else(emissions_year < .retrofit_start_year,
        0,
        round(existing_units * sf_pct)
      ),
      existing_nonretrofit = existing_units - retrofit_units
    ) %>%
    select(-sf_pct) %>%
    pivot_longer(
      cols = c(retrofit_units, existing_nonretrofit),
      names_to = "efficiency_description",
      values_to = "efficiency_unit_value"
    )


  # }


  # if (.existing_mf_retrofit_pct == 0) {
  #   cli::cli_warn("No change in existing multifamily home energy efficiency")
  #   existing_mf <- res_tb %>%
  #     dplyr::filter(grepl("multi", sp_categories)) %>%
  #     mutate(
  #       retrofit_units = 0,
  #       effective_unit_change_retro = 0
  #     )
  # } else if (.existing_mf_retrofit_pct != 0) {
  existing_mf <- res_tb %>%
    dplyr::filter(grepl("multi", sp_categories)) %>%
    left_join(pct_by_year %>% select(emissions_year, mf_pct),
      by = "emissions_year"
    ) %>%
    dplyr::mutate(
      new_units = if_else(value_change_from_base > 0, value_change_from_base, 0),
      existing_units = value - new_units,
      retrofit_units = if_else(emissions_year < .retrofit_start_year,
        0,
        round(existing_units * mf_pct)
      ),
      existing_nonretrofit = existing_units - retrofit_units
    ) %>%
    select(-mf_pct) %>%
    pivot_longer(
      cols = c(retrofit_units, existing_nonretrofit),
      names_to = "efficiency_description",
      values_to = "efficiency_unit_value"
    )
  # }

  retrofit_results <- bind_rows(
    existing_sf %>%
      dplyr::ungroup() %>%
      select(
        geog_name,
        geog_id,
        sp_categories,
        emissions_year,
        value,
        value_change_from_base,
        new_units,
        efficiency_description,
        efficiency_unit_value
      ),
    existing_mf %>%
      dplyr::ungroup() %>%
      select(
        geog_name,
        geog_id,
        sp_categories,
        emissions_year,
        value,
        value_change_from_base,
        new_units,
        efficiency_description,
        efficiency_unit_value
      )
  )

  return(retrofit_results)
}

#' @title Split residential units into electrification scenarios
#' @family buildings, residential
#'
#' @description Takes the combined output of [calc_housing_leed()] and
#'   [calc_residential_retrofit()] and splits each efficiency category into
#'   heat-pump and non-heat-pump portions, producing a long table of
#'   allocated units keyed to the eight scenarios in [calc_building_energy()].
#'
#' @param res_tb [tibble::tibble()]. Bound output of `calc_housing_leed()` and
#'   `calc_residential_retrofit()`, containing columns `efficiency_description`
#'   and `efficiency_unit_value`.
#' @inheritParams run_scenario_building
#' @inheritParams filter_ctu
#'
#' @return [tibble::tibble()] with columns `geog_name`, `geog_id`,
#'   `sp_categories`, `emissions_year`, `scenario`, `allocated_units`.
#' @export
calc_residential_electrification <- function(
  res_tb,
  .selected_ctu,
  .heatpump_start_year,
  .heatpump_end_year,
  .sf_heatpump_pct,
  .mf_heatpump_pct
) {
  check_inputs("single_family_heatpump_pct", .sf_heatpump_pct)
  check_inputs("multifamily_heatpump_pct", .mf_heatpump_pct)
  check_inputs("heatpump_start_year", .heatpump_start_year)

  # --- year-by-year ramp -----------------------------------------------
  ramp_years <- .heatpump_start_year:.heatpump_end_year
  n_ramp <- length(ramp_years)

  # browser()
  pct_by_year <- tibble::tibble(emissions_year = 2005:2050) %>%
    left_join(
      tibble::tibble(
        emissions_year = ramp_years,
        hp_sf_pct = seq(.sf_heatpump_pct / n_ramp, .sf_heatpump_pct, length.out = n_ramp),
        hp_mf_pct = seq(.mf_heatpump_pct / n_ramp, .mf_heatpump_pct, length.out = n_ramp)
      ),
      by = "emissions_year"
    ) %>%
    dplyr::mutate(
      hp_sf_pct = dplyr::case_when(
        emissions_year < .heatpump_start_year ~ 0,
        emissions_year > .heatpump_end_year ~ .sf_heatpump_pct,
        TRUE ~ hp_sf_pct
      ),
      hp_mf_pct = dplyr::case_when(
        emissions_year < .heatpump_start_year ~ 0,
        emissions_year > .heatpump_end_year ~ .mf_heatpump_pct,
        TRUE ~ hp_mf_pct
      )
    )

  # --- efficiency_description → scenario pair lookup -------------------
  hp_split_map <- tibble::tribble(
    ~efficiency_description, ~no_hp_scenario, ~hp_scenario,
    "existing_nonretrofit", "baseline", "heatpump",
    "retrofit_units", "retrofit", "combination",
    "new_non_leed", "new_build", "new_build_heatpump",
    "new_leed", "new_build_leed", "new_build_leed" # new build_leed (sustainable new build) already has heatpumps inherent in build
  )

  # --- split units into hp / no-hp rows --------------------------------
  hp_split_out <- res_tb %>%
    left_join(pct_by_year, by = "emissions_year") %>%
    dplyr::mutate(
      hp_pct = dplyr::if_else(grepl("multi", sp_categories), hp_mf_pct, hp_sf_pct)
    ) %>%
    left_join(hp_split_map, by = "efficiency_description") %>%
    tidyr::pivot_longer(
      cols      = c(no_hp_scenario, hp_scenario),
      names_to  = "hp_type",
      values_to = "scenario"
    ) %>%
    dplyr::mutate(
      allocated_units = dplyr::if_else(
        hp_type == "hp_scenario",
        round(efficiency_unit_value * hp_pct),
        round(efficiency_unit_value * (1 - hp_pct))
      )
    ) %>%
    dplyr::select(
      geog_name, geog_id, sp_categories,
      emissions_year, scenario, allocated_units
    )

  return(hp_split_out)
}
