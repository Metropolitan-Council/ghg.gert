#' Calculate Manure Management Emissions
#'
#' @param livestock_df Data frame with columns: county_name, year, livestock_type, head_count
#' @param agriculture_variables List containing: mcf, vs, manure_state, nex, Bo
#' @param ag_constants_vec Named vector of constants
#' @param gwp_list List with ch4 and n2o global warming potentials
#'
#' @return Data frame with emissions by county, year, storage_state, and gas type
#'
#'
calculate_manure_emissions <- function(livestock = agriculture_activity_data$livestock,
                                       agriculture_variables,
                                       ag_constants_vec,
                                       gwp_list,
                                       ag_manure_mgmt_complete) {

  livestock_df <- filter_ctu(livestock, .selected_ctu = .selected_ctu)

  # Validate inputs
  required_livestock_cols <- c("county_name", "year", "livestock_type", "head_count")
  if (!all(required_livestock_cols %in% names(livestock_df))) {
    stop("livestock_df must contain: ", paste(required_livestock_cols, collapse = ", "))
  }

  # Extract variables from list
  vs_data <- agriculture_variables$vs
  nex_data <- agriculture_variables$nex
  mcf_data <- agriculture_variables$mcf
  Bo_data <- agriculture_variables$Bo
  manure_split <- agriculture_variables$manure_state

  # ===== CH4 EMISSIONS =====
  ch4_emissions <- livestock_df %>%
    left_join(vs_data,
              by = c("year", "livestock_type")) %>%
    left_join(Bo_data, by = "livestock_type") %>%
    left_join(mcf_data,
              by = c("year", "livestock_type")) %>%
    mutate(
      mt_ch4 = head_count * mt_vs_head_yr * Bo * mcf_percent * ag_constants_vec["kg_m3"],
      mt_co2e = mt_ch4 * gwp_list$ch4
    ) %>%
    group_by(year, county_name, livestock_type) %>%
    summarize(
      mt_ch4 = sum(mt_ch4, na.rm = TRUE),
      mt_co2e = sum(mt_co2e, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(manure_split,
              by = c("year", "livestock_type")) %>%
    mutate(
      mt_ch4_by_storage = mt_ch4 * percentage,
      mt_co2e_by_storage = mt_co2e * percentage
    ) %>%
    select(year, county_name, livestock_type, storage_state,
           mt_ch4_by_storage, mt_co2e_by_storage)

  # ===== N2O EMISSIONS (LIQUIDS AND SOLIDS) =====

  # Calculate liquid and solid percentages from manure_split
  liquids_perc <- manure_split %>%
    filter(storage_state == "Liquid") %>%
    select(year, livestock_type, liquids_perc = percentage)

  solids_perc <- manure_split %>%
    filter(storage_state == "Solid") %>%
    select(year, livestock_type, solids_perc = percentage)

  n2o_emissions <- livestock_df %>%
    left_join(nex_data,
              by = c("year", "livestock_type")) %>%
    left_join(liquids_perc, by = c("year", "livestock_type")) %>%
    left_join(solids_perc, by = c("year", "livestock_type")) %>%
    mutate(
      liquids_perc = replace_na(liquids_perc, 0),
      solids_perc = replace_na(solids_perc, 0)
    ) %>%
    mutate(
      mt_n2o_liquids = head_count *
        kg_nex_head_yr *
        (1 - ag_constants_vec["VolPercent"]) *
        liquids_perc *
        ag_constants_vec["LiquidEF"] *
        ag_constants_vec["N2O_N2"] / 1000,
      mt_n2o_solids = head_count *
        kg_nex_head_yr *
        (1 - ag_constants_vec["VolPercent"]) *
        solids_perc *
        ag_constants_vec["SolidEF"] *
        ag_constants_vec["N2O_N2"] / 1000,
      mt_co2e_liquids = mt_n2o_liquids * gwp_list$n2o,
      mt_co2e_solids = mt_n2o_solids * gwp_list$n2o
    ) %>%
    pivot_longer(
      cols = c(mt_co2e_liquids, mt_co2e_solids, mt_n2o_liquids, mt_n2o_solids),
      names_to = c(".value", "storage_state"),
      names_pattern = "(mt_[a-z0-9]+)_(liquids|solids)"
    ) %>%
    mutate(storage_state = str_to_title(storage_state)) %>%
    group_by(year, county_name, livestock_type, storage_state) %>%
    summarize(
      mt_n2o = sum(mt_n2o, na.rm = TRUE),
      mt_co2e = sum(mt_co2e, na.rm = TRUE),
      .groups = "drop"
    )

  # ===== N2O EMISSIONS FROM INDIRECT RUNOFF =====

  KN_excretion <- livestock_df %>%
    left_join(nex_data, by = c("year", "livestock_type")) %>%
    mutate(total_kn_excretion_kg = head_count * kg_nex_head_yr)

  nex_runoff_emissions <- KN_excretion %>%
    group_by(year, county_name, livestock_type) %>%
    summarize(mt_total_kn_excretion = sum(total_kn_excretion_kg / 1000),
              .groups = "drop") %>%
    mutate(
      mt_n = mt_total_kn_excretion * (1 - ag_constants_vec["VolPercent"]) *
        ag_constants_vec["LeachEF"],
      mt_n2o = mt_n * ag_constants_vec["LeachEF2"] * ag_constants_vec["N2O_N2"],
      mt_co2e = mt_n2o * gwp_list$n2o
    ) %>%
    left_join(manure_split, by = c("year", "livestock_type")) %>%
    mutate(
      mt_n2o_by_storage = mt_n2o * percentage,
      mt_co2e_by_storage = mt_co2e * percentage
    ) %>%
    group_by(year, county_name, livestock_type, storage_state) %>%
    summarize(
      mt_n2o = sum(mt_n2o_by_storage, na.rm = TRUE),
      mt_co2e = sum(mt_co2e_by_storage, na.rm = TRUE),
      .groups = "drop"
    )

  # ===== N2O EMISSIONS FROM DIRECT SOIL APPLICATION =====

  # Calculate management type percentages
  manure_mgmt_perc <- ag_manure_mgmt_complete %>%
    mutate(management_type = case_when(
      managed == "Yes" ~ "Managed",
      mgmt_system %in% c("Pasture", "PRP", "Dry Lot", "Range",
                         "Pasture, Range & Paddock") ~ "Pasture_range",
      mgmt_system == "Daily Spread" ~ "Daily_spread"
    )) %>%
    group_by(year, livestock_type, management_type) %>%
    summarize(percentage = sum(percentage), .groups = "drop")

  # Get percentages for each management type
  managed_perc <- manure_mgmt_perc %>%
    filter(management_type == "Managed") %>%
    select(year, livestock_type, percent_managed = percentage)

  daily_spread_perc <- manure_mgmt_perc %>%
    filter(management_type == "Daily_spread") %>%
    select(year, livestock_type, percent_daily_spread = percentage)

  pasture_perc <- manure_mgmt_perc %>%
    filter(management_type == "Pasture_range") %>%
    select(year, livestock_type, percent_pasture = percentage)

  manure_soils <- KN_excretion %>%
    left_join(managed_perc, by = c("year", "livestock_type")) %>%
    left_join(daily_spread_perc, by = c("year", "livestock_type")) %>%
    left_join(pasture_perc, by = c("year", "livestock_type")) %>%
    mutate(
      percent_managed = case_when(
        livestock_type %in% c("Broilers", "Pullets") ~ 1,
        livestock_type %in% c("Sheep") ~ 0.5,
        TRUE ~ percent_managed
      ),
      percent_pasture = case_when(
        livestock_type %in% c("Calves") ~ 1,
        livestock_type %in% c("Sheep") ~ 0.5,
        TRUE ~ percent_pasture
      ),
      managed_nex = total_kn_excretion_kg * percent_managed,
      pasture_nex = total_kn_excretion_kg * percent_pasture,
      daily_spread_nex = total_kn_excretion_kg * percent_daily_spread
    ) %>%
    replace_na(list(
      percent_managed = 0, percent_pasture = 0, percent_daily_spread = 0,
      managed_nex = 0, pasture_nex = 0, daily_spread_nex = 0
    ))

  manure_soils_emissions <- manure_soils %>%
    mutate(
      MT_n2o_manure_application = (managed_nex + daily_spread_nex) *
        (1 - ag_constants_vec["VolPercent_Indirect"]) *
        ag_constants_vec["NonVolEF"] /
        1000 *
        ag_constants_vec["N2O_N2"],
      MT_n2o_pasture = pasture_nex * ag_constants_vec["prpEF"] / 1000 *
        ag_constants_vec["N2O_N2"],
      MT_co2e_manure_application = MT_n2o_manure_application * gwp_list$n2o,
      MT_co2e_pasture = MT_n2o_pasture * gwp_list$n2o
    ) %>%
    group_by(year, county_name, livestock_type) %>%
    summarize(
      mt_n2o = sum(MT_n2o_manure_application + MT_n2o_pasture, na.rm = TRUE),
      mt_co2e = sum(MT_co2e_manure_application + MT_co2e_pasture, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(storage_state = "Applied")

  # ===== COMBINE ALL RESULTS =====
  emissions_output <- bind_rows(
    ch4_emissions %>%
      rename(mt_gas = mt_ch4_by_storage, mt_co2e = mt_co2e_by_storage) %>%
      mutate(gas_type = "ch4", source = "manure_management"),
    n2o_storage_emissions %>%
      rename(mt_gas = mt_n2o) %>%
      mutate(gas_type = "n2o", source = "manure_management"),
    nex_runoff_emissions %>%
      rename(mt_gas = mt_n2o) %>%
      mutate(gas_type = "n2o", source = "indirect_manure_runoff"),
    manure_soils_emissions %>%
      rename(mt_gas = mt_n2o) %>%
      mutate(gas_type = "n2o", source = "direct_manure_soil")
  ) %>%
    select(year, county_name, livestock_type, storage_state,
           gas_type, source, mt_gas, mt_co2e)

  return(emissions_output)
}


