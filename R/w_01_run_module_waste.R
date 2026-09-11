#' @title Execute waste scenarios
#' @family waste
#'
#' @description This function generates the outputs of the waste module
#'    for the given scenario at the city/township level. It incorporates various
#'    parameters to evaluate and analyze different waste scenarios for landfill,
#'    organics, recycling, and waste to energy. Outputs are
#'    provided as a list of tibbles with columns `inventory_year`, `geog_id`, `geog_name`,
#'       `geog_level`, `source`, `value_activity`, `units_activity`, `data_type`,
#'       `sector`, `category`, `data_source`, `factor_source`, `value_emissions`,
#'       `units_emissions`, `bau_activity` and `bau_emissions`.
#'
#' @inheritParams filter_ctu
#' @inheritParams calculate_waste_reduction
#' @inheritParams calculate_landfill_emissions
#' @inheritParams calculate_incin_emissions
#' @inheritParams calculate_organic_emissions
#'
#'
#' @return [list()].
#'       Returns a list of tables with columns `inventory_year`, `geog_id`, `geog_name`,
#'       `geog_level`, `source`, `value_activity`, `units_activity`, `data_type`,
#'       `sector`, `category`, `data_source`, `factor_source`, `value_emissions`,
#'       `units_emissions`, `bau_activity` and `bau_emissions`.
#'       The table is the output of the waste module, any modification to
#'       the inputs of the waste module must be specified as an argument
#'       to the function `run_module_waste()`
#'
#' @export
#' @importFrom cli cli_progress_message
#' @importFrom tibble deframe
#'
run_module_waste <- function(tb_inv = waste_data$inventory,
                             tb_future = waste_data$projections,
                             tb_base = waste_data$solid_waste_baseline,
                             tb_char = waste_data$characterization,
                             tb_target = waste_data$mpca,
                             .selected_ctu = "all",
                             # user inputs below
                             .waste_reduction_pct = 0,
                             .waste_reduction_start = 2025,
                             .waste_reduction_end = 2050,
                             .source_diversion_start = 2025,
                             .source_diversion_end = 2050,
                             .diverted_to_landfill_pct = NULL,
                             .diverted_to_recycle_pct = NULL,
                             .diverted_to_organics_pct = NULL,
                             .diverted_to_wte_pct = NULL,
                             .diverted_to_onsite_pct = NULL,
                             .diverted_to_compost_pct = NULL,
                             .methane_recovery_pct = 0,
                             .methane_recovery_start = 2025,
                             .methane_recovery_end = 2050,
                             .anaerobic_digestion_pct = 0,
                             .anaerobic_digestion_start = 2025,
                             .anaerobic_digestion_end = 2050) {
  if (.selected_ctu == "all") {
    tb_inv <- filter(tb_inv, geog_level == "CITY")
    tb_future <- filter(tb_future, geog_level == "CITY")
    tb_base <- filter(tb_base, geog_level == "CITY")
  }

  tb_inv <- filter_ctu(tb_inv, .selected_ctu = .selected_ctu)
  tb_future <- filter_ctu(tb_future, .selected_ctu = .selected_ctu)
  tb_base <- filter_ctu(tb_base, .selected_ctu = .selected_ctu)


  l_names <- c(
    "waste_reduction_pct",
    "waste_reduction_start",
    "waste_reduction_end",
    "source_diversion_start",
    "source_diversion_end",
    "diverted_to_landfill_pct",
    "diverted_to_recycle_pct",
    "diverted_to_organics_pct",
    "diverted_to_wte_pct",
    "diverted_to_onsite_pct",
    "diverted_to_compost_pct",
    "methane_recovery_pct",
    "methane_recovery_start",
    "methane_recovery_end",
    "anaerobic_digestion_pct",
    "anaerobic_digestion_start",
    "anaerobic_digestion_end"
  )

  l_vals <- list(
    .waste_reduction_pct,
    .waste_reduction_start,
    .waste_reduction_end,
    .source_diversion_start,
    .source_diversion_end,
    .diverted_to_landfill_pct,
    .diverted_to_recycle_pct,
    .diverted_to_organics_pct,
    .diverted_to_wte_pct,
    .diverted_to_onsite_pct,
    .diverted_to_compost_pct,
    .methane_recovery_pct,
    .methane_recovery_start,
    .methane_recovery_end,
    .anaerobic_digestion_pct,
    .anaerobic_digestion_start,
    .anaerobic_digestion_end
  )


  purrr::map2(l_names, l_vals, check_inputs)


  # 1. Calculate BAU scenario -----------------------------------------------
  # bind your activity inventory and projections together to create a business as usual scenario
  bau_activity <- rbind(
    tb_inv, tb_future
  ) %>%
    group_by(inventory_year) %>%
    summarize(value_activity = sum(value_activity, na.rm = T)) %>%
    dplyr::ungroup()


  ## Do the same for our business as usual scenario
  landfill_emis_bau <- ghg.gert::calculate_landfill_emissions(
    waste_inv = tb_inv,
    waste_future = tb_future,
    waste_char = tb_char
  )
  incin_emis_bau <- ghg.gert::calculate_incin_emissions(
    waste_inv = tb_inv,
    waste_future = tb_future
  )
  organic_emis_bau <- ghg.gert::calculate_organic_emissions(
    waste_inv = tb_inv,
    waste_future = tb_future
  )

  ## NEW! wastewater emissions
  wastewater_emis_bau <- ghg.gert::calculate_wastewater_emissions(
    waste_inv = tb_inv,
    waste_future = tb_future
  )

  # define global warming potential
  gwp <-
    list(
      "co2" = 1,
      "ch4" = 27.9,
      "n2o" = 273,
      "cf4" = 7380,
      "HFC-152a" = 164
    )

  # Function to compile all emissions sources into metric tons CO2e
  compile_waste_emis <- function(df) {
    # browser()
    df %>%
      arrange(inventory_year, source, units_emissions) %>%
      # give each row a unique id to avoid pivoting error
      dplyr::mutate(id = dplyr::row_number()) %>%
      dplyr::group_by(id) %>%
      tidyr::pivot_wider(
        names_from = units_emissions,
        values_from = value_emissions
      ) %>%
      replace(is.na(.), 0) %>%
      dplyr::mutate(
        ch4_co2e = `Metric tons CH4` * gwp$ch4,
        n2o_co2e = `Metric tons N2O` * gwp$n2o,
        co2_co2e = `Metric tons CO2`,
        value_emissions = ch4_co2e + n2o_co2e + co2_co2e,
        units_emissions = "Metric tons CO2e"
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        -c(
          `Metric tons CH4`,
          `Metric tons CO2`,
          `Metric tons N2O`,
          ch4_co2e,
          n2o_co2e,
          co2_co2e,
          id
        )
      ) %>%
      group_by(inventory_year, source) %>%
      summarize(
        value_emissions = sum(value_emissions)
      ) %>%
      dplyr::ungroup() %>%
      mutate(
        units_emissions = "Metric tons CO2e",
        sector = "Waste",
        # category = "Solid waste and wastewater",
        # data_source = "MPCA SCORE Report",
        # factor_source = "IPCC solid waste methodology"
      ) %>%
      cross_join(df %>%
        dplyr::select(geog_id, geog_name, geog_level) %>%
        head(1)) %>%
      relocate(c(geog_id, geog_name, geog_level), .after = inventory_year)
  }


  # Compute the total emissions for BAU scenario
  bau_emissions <- rbind(
    rbind(landfill_emis_bau$inv, landfill_emis_bau$future),
    rbind(incin_emis_bau$inv, incin_emis_bau$future),
    rbind(organic_emis_bau$inv, organic_emis_bau$future),
    rbind(wastewater_emis_bau$inv, wastewater_emis_bau$future)
  ) %>%
    compile_waste_emis() %>%
    group_by(inventory_year) %>%
    summarize(value_emissions = sum(value_emissions)) %>%
    dplyr::ungroup()


  tb_bau <- bau_activity %>%
    left_join(bau_emissions, by = "inventory_year") %>%
    rename(
      bau_activity = value_activity,
      bau_emissions = value_emissions
    )


  rm(
    bau_activity, bau_emissions,
    landfill_emis_bau, incin_emis_bau, organic_emis_bau
  )


  # 2. Run waste reduction module -------------------------------------------
  if (.waste_reduction_pct == 0) {
    tb_proj_01 <- tb_future
  } else {
    tb_proj_01 <- ghg.gert::calculate_waste_reduction(
      waste_tb = tb_future,
      .waste_reduction_pct = .waste_reduction_pct,
      .waste_reduction_start = .waste_reduction_start,
      .waste_reduction_end = .waste_reduction_end
    )
  }


  # 3. Run source diversion module ------------------------------------------
  # Here we need to inherit the results from the waste reduction module
  # Next the user gets to select how much (%) waste is diverted from landfills to
  # recycling, organics, and WTE. As an MPCA rule, there must always be 5%
  # background activity for landfills (no less than).

  # First determine the current shares of each source in the inventory year
  baseline_shares <- tb_proj_01 %>%
    filter(source != "Wastewater") %>%
    filter(inventory_year == .source_diversion_start) %>%
    mutate(
      total_activity = sum(value_activity),
      share_of_total = value_activity / total_activity,
    ) %>%
    dplyr::select(source, share_of_total) %>%
    mutate(source = case_when(
      source == "Landfill" ~ "landfill",
      source == "Recycling" ~ "recycle",
      source == "Organics" ~ "organics",
      source == "Waste to energy" ~ "wte",
      source == "Onsite" ~ "onsite",
      source == "MSW_Compost" ~ "compost",
      TRUE ~ source
    )) %>%
    # convert to named vector
    tibble::deframe()

  # Next let's compile the user inputs for the diversion targets (where applicable)
  user_targets <- c(
    landfill = if (is.null(.diverted_to_landfill_pct)) NA_real_ else .diverted_to_landfill_pct,
    compost  = if (is.null(.diverted_to_compost_pct)) NA_real_ else .diverted_to_compost_pct,
    onsite   = if (is.null(.diverted_to_onsite_pct)) NA_real_ else .diverted_to_onsite_pct,
    organics = if (is.null(.diverted_to_organics_pct)) NA_real_ else .diverted_to_organics_pct,
    recycle  = if (is.null(.diverted_to_recycle_pct)) NA_real_ else .diverted_to_recycle_pct,
    wte      = if (is.null(.diverted_to_wte_pct)) NA_real_ else .diverted_to_wte_pct
  )


  # If the user sets only one (or a few) variable, we should absolutely retain that value,
  # and adjust the other values proportionally.
  adjust_targets <- function(user_targets, baseline_shares, tol = 1e-9) {
    fixed_mask <- !is.na(user_targets)
    flexible_mask <- is.na(user_targets)

    sum_fixed <- sum(user_targets[fixed_mask])

    final_targets <- user_targets

    if (sum_fixed > 1 + tol) {
      # Case 2: over 100%, scale down proportionally
      warning("User-specified percentages exceed 100%. Scaling all values proportionally.")
      final_targets[fixed_mask] <- user_targets[fixed_mask] / sum_fixed
      final_targets[flexible_mask] <- 0
    } else {
      # Case 1: under or equal to 100%, redistribute remainder
      remaining <- 1 - sum_fixed
      if (remaining > tol && any(flexible_mask)) {
        baseline_flexible <- baseline_shares[flexible_mask]
        final_targets[flexible_mask] <- baseline_flexible / sum(baseline_flexible) * remaining
      } else if (remaining <= tol) {
        final_targets[flexible_mask] <- 0
      }
    }

    return(final_targets)
  }


  # Adjust the user targets based on the baseline shares
  final_targets <- adjust_targets(user_targets, baseline_shares)


  # If the user leaves all values at the default NULL, we should not change the
  # activity shares at all, and just return the original projections.
  if (is.null(.diverted_to_landfill_pct) &
    is.null(.diverted_to_recycle_pct) &
    is.null(.diverted_to_organics_pct) &
    is.null(.diverted_to_wte_pct) &
    is.null(.diverted_to_onsite_pct) &
    is.null(.diverted_to_compost_pct)) {
    tb_proj_02 <- tb_proj_01
  } else {
    # If ANY of the .diverted_to_*_pct are not NULL, we need to adjust the share of that
    # source's activity.
    tb_proj_02 <- tb_proj_01 %>%
      filter(source != "Wastewater") %>%
      group_by(inventory_year) %>%
      mutate(
        total_activity = sum(value_activity)
      ) %>%
      dplyr::ungroup() %>%
      mutate(
        share_of_total = value_activity / total_activity,
        target_share = case_when(
          source == "Landfill" ~ final_targets["landfill"],
          source == "Recycling" ~ final_targets["recycle"],
          source == "Organics" ~ final_targets["organics"],
          source == "Waste to energy" ~ final_targets["wte"],
          source == "Onsite" ~ final_targets["onsite"],
          source == "MSW_Compost" ~ final_targets["compost"],
          TRUE ~ NA_real_
        ),
        # calculate the difference between the target share and the current share
        share_diff = target_share - share_of_total,
        # using the start and end date,
        # we want the final date to have percentages set by the user
        # those final percentages should persist until the end of the dataset
        # changes should only be made to the underlying categories starting on the start date
        # and ending on the end date
        new_share = case_when(
          inventory_year < .source_diversion_start ~ share_of_total,
          inventory_year >= .source_diversion_start & inventory_year <= .source_diversion_end ~
            share_of_total + (share_diff * (inventory_year - .source_diversion_start) / (.source_diversion_end - .source_diversion_start)),
          inventory_year > .source_diversion_end ~ target_share
        ),
        adjusted_activity = total_activity * new_share
      )


    # reselect and rename columns to match original tb_inv structure
    tb_proj_02 <- tb_proj_02 %>%
      dplyr::select(-value_activity) %>%
      rename(value_activity = adjusted_activity) %>%
      dplyr::select(colnames(tb_inv)) %>%
      # add wastewater back in
      bind_rows(
        tb_proj_01 %>%
          filter(source == "Wastewater")
      ) %>%
      arrange(inventory_year, source)
  }


  # 4. Calculate emissions --------------------------------------------------
  # calculate landfill emissions
  landfill_emis <- ghg.gert::calculate_landfill_emissions(
    waste_inv = tb_inv,
    waste_future = tb_proj_02,
    waste_char = tb_char,
    .methane_recovery_pct = .methane_recovery_pct,
    .methane_recovery_start = .methane_recovery_start,
    .methane_recovery_end = .methane_recovery_end
  )

  # calculate incineration emissions
  incin_emis <- ghg.gert::calculate_incin_emissions(
    waste_inv = tb_inv,
    waste_future = tb_proj_02
  )

  # calculate organic emissions
  organic_emis <- ghg.gert::calculate_organic_emissions(
    waste_inv = tb_inv,
    waste_future = tb_proj_02,
    .anaerobic_digestion_pct = .anaerobic_digestion_pct,
    .anaerobic_digestion_start = .anaerobic_digestion_start,
    .anaerobic_digestion_end = .anaerobic_digestion_end,
    .methane_recovery_pct = .methane_recovery_pct,
    .methane_recovery_start = .methane_recovery_start,
    .methane_recovery_end = .methane_recovery_end
  )


  ## NEW! wastewater emissions
  wastewater_emis <- ghg.gert::calculate_wastewater_emissions(
    waste_inv = tb_inv,
    waste_future = tb_proj_02
  )

  # 5. Compile the results --------------------------------------------------
  waste_emissions <- list()


  waste_emissions$activity$inv <- tb_inv %>% left_join(tb_bau, by = join_by(inventory_year))
  waste_emissions$activity$future <- tb_proj_02 %>% left_join(tb_bau, by = join_by(inventory_year))

  waste_emissions$emissions$inv <-
    rbind(landfill_emis$inv, incin_emis$inv, organic_emis$inv, wastewater_emis$inv) %>%
    compile_waste_emis() %>%
    left_join(tb_bau, by = join_by(inventory_year))


  waste_emissions$emissions$future <-
    rbind(landfill_emis$future, incin_emis$future, organic_emis$future, wastewater_emis$future) %>%
    compile_waste_emis() %>%
    left_join(tb_bau, by = join_by(inventory_year))


  return(waste_emissions)
}
