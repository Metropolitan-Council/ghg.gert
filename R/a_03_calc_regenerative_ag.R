#' @title Calculate regenerative ag emission reductions
#' @family cropland
#'
#' @description Calculates the emission reductions associated with switching to
#' no till agriculture and cover cropping.
#'
#' @param .cover_crops_current numeric,  a value between `0` and `1`.
#'      The current percentage of acreage cover cropping.
#'      Default is `0.0`
#' @param .cover_crops_goal numeric,  a value between `0` and `1`.
#'      The percentage of acreage to have cover cropping in 2050
#' @param .cover_crops_start_year numeric,  a value between `2028` and `2045`.
#'      The year new cover crops program targets
#' @param .no_till_current numeric,  a value between `0` and `1`.
#'      The current percentage of acreage with no till.
#'      Default is `0.0`
#' @param .no_till_goal numeric,  a value between `0` and `1`.
#'      The percentage of acreage to have no till in 2050
#' @param .no_till_start_year numeric,  a value between `2028` and `2045`.
#'      The year new no till program targets
#' @param .regen_ag_start_year numeric,  a value between `2028` and `2045`.
#'      Joint start year for no_till and regen_ag
#'
#' @inheritParams run_module_agriculture
#' @inheritParams calculate_cropland_emissions
#' @inheritParams filter_ctu
#'
#' @details
#'    Uses the coefficients found in MN CAF technical documenation
#'
#' @return [tibble::tibble()].
#' @export
calc_regen_ag <- function(emissions,
                       .selected_ctu,
                       caf_strategies = ghg.ccap::agriculture_regen_ag_caf,
                                  .scenario,
                                  .baseline_year,
                                  ag_area_adj,
                       .cover_crops_start_year,
                       .cover_crops_current,
                       .cover_crops_goal,
                       .no_till_start_year,
                       .no_till_current,
                       .no_till_goal,
                       .regen_ag_start_year) {

  #browser()

  county_use <- county_assign(.selected_ctu = .selected_ctu)

  crop_emissions <- emissions %>%
    filter(source == "Soil residue emissions")

  strategy_use <- caf_strategies %>%
    filter(geog_name == county_use)

  # CAF strategies are anchored to n2o emissions as proportions
  # Strategies are non-additive. Assumption is that acres will share strategies as much as possible

  combo_strat <- min(.cover_crops_goal, .no_till_goal)
  cc_only <- if_else(.cover_crops_goal > .no_till_goal, .cover_crops_goal - .no_till_goal, 0)
  no_till_only <- if_else(.no_till_goal > .cover_crops_goal, .no_till_goal - .cover_crops_goal, 0)

  #calculate end amount of c seq and n2o reduction in 2050
  no_till_red <- strategy_use %>% filter(strategy == "No till") %>% pull(n2o_emis_ratio) * no_till_only
  no_till_seq <- strategy_use %>% filter(strategy == "No till") %>% pull(c_seq_ratio ) * no_till_only

  # cover crop n2o reduction being set to zero, consistent with
  ## article suggests cover crops increase C sequestration - reduction of N2O less certain
  # https://conservancy.umn.edu/server/api/core/bitstreams/e3fc83a3-ae4c-4c6a-b31d-4714b24082e0/content

  cc_red <- 0
  cc_seq <- strategy_use %>% filter(strategy == "Non-legume cover crop") %>% pull(c_seq_ratio ) * cc_only # covert MT C acre-1 yr-1 to MT CO2 acre-1 yr-1

  combo_red <- strategy_use %>% filter(strategy == "Cover crop and no till") %>% pull(n2o_emis_ratio) * combo_strat
  combo_seq <- strategy_use %>% filter(strategy == "Cover crop and no till") %>% pull(c_seq_ratio ) * combo_strat

  #browser()

  regen_ag_emissions_alt <- crop_emissions %>%
    mutate(no_till_reduction = case_when(
      # NO TILL REDUCTIONS
      inventory_year < .regen_ag_start_year ~ 0,
      inventory_year >= .regen_ag_start_year ~
        value_emissions * (-1 * (no_till_red) * (inventory_year - .regen_ag_start_year) /
                             (2050 - .regen_ag_start_year))
    ),
    no_till_sequestration = case_when(
      inventory_year < .regen_ag_start_year ~ 0,
      inventory_year >= .regen_ag_start_year ~
        value_emissions * ((no_till_seq) * (inventory_year - .regen_ag_start_year) /
                             (2050 - .regen_ag_start_year))
    ),
    # COVER CROP REDUCTIONS
    cc_reduction = case_when(
      inventory_year < .regen_ag_start_year ~ 0,
      inventory_year >= .regen_ag_start_year ~
        value_emissions * (-1 * (cc_red) * (inventory_year - .regen_ag_start_year) /
                             (2050 - .regen_ag_start_year))
    ),
    cc_sequestration = case_when(
      inventory_year < .regen_ag_start_year ~ 0,
      inventory_year >= .regen_ag_start_year ~
        value_emissions * ((cc_seq) * (inventory_year - .regen_ag_start_year) /
                             (2050 - .regen_ag_start_year))
    ),
    # COMBINATION STRATEGIES
    combo_reduction = case_when(
      inventory_year < .regen_ag_start_year ~ 0,
      inventory_year >= .regen_ag_start_year ~
        value_emissions * (-1 * (combo_red) * (inventory_year - .regen_ag_start_year) /
                             (2050 - .regen_ag_start_year))
    ),
    combo_sequestration = case_when(
      inventory_year < .regen_ag_start_year ~ 0,
      inventory_year >= .regen_ag_start_year ~
        value_emissions * ((combo_seq) * (inventory_year - .regen_ag_start_year) /
                             (2050 - .regen_ag_start_year))
    ),
    sequestration = combo_sequestration + cc_sequestration + no_till_sequestration,
    n2o_reduction = combo_reduction + cc_reduction + no_till_reduction,
    value_emissions_net = (value_emissions - sequestration) - n2o_reduction,
    scenario = .scenario
    ) %>%
    select(geog_name, geog_id, geog_level, sector, category, source, inventory_year,
           value_emissions, scenario, sequestration, n2o_reduction, value_emissions_net)

  # # Line below looks like an orphan, commenting out for now but may be useful for debugging
  # regen_ag_emissions_alt %>% filter(inventory_year %in% c(2022, 2030, 2050))


  # Check if any year has negative net emissions and adjust
  # if(any(regen_ag_emissions_alt$value_emissions_net < 0, na.rm = TRUE)) {
  #   cli_alert("Cropland emissions estimated to be net sink, adjusting to 0")
  #   regen_ag_emissions_alt <- regen_ag_emissions_alt %>%
  #     mutate(value_emissions_net = pmax(value_emissions_net, 0))
  # }

  #browser()

  cropland_emissions <- bind_rows(crop_emissions %>%
                                    mutate(sequestration = 0,
                                           n2o_reduction = 0),
                                  regen_ag_emissions_alt %>%
                                    select(-value_emissions) %>%
                                    rename(value_emissions = value_emissions_net)
  )

  return(cropland_emissions)
}
