# Calculate Manure Management Emissions - baseline and CAF

agriculture_variables <- ghg.ccap::agriculture_variables
gwp_list <- ghg.ccap::gwp_list


### calculate BAU for all CTUs


livestock_df <- ghg.ccap::agriculture_activity_data$livestock

# extract required variables from list
ag_constants_vec <- agriculture_variables$ag_constants
vs_data <- agriculture_variables$vs
nex_data <- agriculture_variables$nex
mcf_data <- agriculture_variables$mcf
Bo_data <- agriculture_variables$Bo
manure_split <- agriculture_variables$manure_state


### MPCA CAF pathway

## from Table 14 of CAF Techinical support doc (12/5/2025)
caf_manure_coefs <- list(
  dairy_ch4 = (0.8 * 0.2 * -0.9) + # cover and flare
    (0.8 * 0.1 * -.972) + # anaerobic digestion
    (0.8 * 0.3 * -0.48) + # solid liquid separation
    (0.8 * 0.3 * -0.64), # slurry acidification
  dairy_n2o =
    (0.8 * 0.3 * -0.05), # solid liquid separation
  beef_n2o = 0.5 * 0.75 * -0.24, # lower crude protein diet
  swine_ch4 = (1 * 0.1 * -.852) + # solid liquid separation
    (1 * 0.5 * -0.71) + # slurry acidification
    (0.5 * 0.5 * -0.32) + # empty deep pits 2x/year
    (0.1 * 0.5 * -0.9), # cover and flare
  swine_n2o = (1 * 0.1 * -.333) + # solid liquid separation
    (1 * 0.5 * -0.5) + # slurry acidification
    (0.5 * 0.75 * -0.211), # lower crude protein diet
  poultry_ch4 = 0.9 * 0.3 * -0.99, # thermochemical processing
  poultry_n2o = 0.9 * 0.3 * -0.99 # thermochemical processing
)

# ===== CH4 EMISSIONS =====
ch4_emissions <- livestock_df %>%
  left_join(vs_data,
    by = c("inventory_year", "livestock_type")
  ) %>%
  left_join(Bo_data, by = "livestock_type") %>%
  left_join(mcf_data,
    by = c("inventory_year", "livestock_type")
  ) %>%
  mutate(
    mt_ch4 = head_count * mt_vs_head_yr * Bo * mcf_percent * ag_constants_vec["kg_m3"],
    mt_co2e = mt_ch4 * gwp_list$ch4
  ) %>%
  group_by(inventory_year, geog_name, county_name, livestock_type) %>%
  summarize(
    mt_ch4 = sum(mt_ch4, na.rm = TRUE),
    mt_co2e = sum(mt_co2e, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(manure_split,
    by = c("inventory_year", "livestock_type")
  ) %>%
  mutate(
    mt_ch4_by_storage = mt_ch4 * percentage,
    mt_co2e_by_storage = mt_co2e * percentage
  ) %>%
  select(
    inventory_year, geog_name, county_name, livestock_type, storage_state,
    mt_ch4_by_storage, mt_co2e_by_storage
  ) %>%
  mutate(mt_co2e_caf = case_when(
    inventory_year <= 2027 ~ mt_co2e_by_storage,
    inventory_year > 2027 &
      livestock_type %in% c(
        "Calves",
        "Dairy Cows",
        "Feedlot Cattle"
      ) &
      storage_state == "Liquid" ~ mt_co2e_by_storage * (1 + caf_manure_coefs$dairy_ch4),
    inventory_year > 2027 &
      livestock_type %in% c("Swine") &
      storage_state == "Liquid" ~ mt_co2e_by_storage * (1 + caf_manure_coefs$swine_ch4),
    inventory_year > 2027 &
      livestock_type %in% c(
        "Broilers",
        "Layers",
        "Pullets",
        "Turkeys"
      ) &
      storage_state == "Solid" ~ mt_co2e_by_storage * (1 + caf_manure_coefs$poultry_ch4),
    TRUE ~ mt_co2e_by_storage
  )) %>%
  ungroup()

# ===== N2O EMISSIONS (LIQUIDS AND SOLIDS) =====

# Calculate liquid and solid percentages from manure_split
liquids_perc <- manure_split %>%
  filter(storage_state == "Liquid") %>%
  select(inventory_year, livestock_type, liquid_perc = percentage)

solids_perc <- manure_split %>%
  filter(storage_state == "Solid") %>%
  select(inventory_year, livestock_type, solid_perc = percentage)


n2o_emissions <- livestock_df %>%
  left_join(nex_data,
    by = c("inventory_year", "livestock_type")
  ) %>%
  left_join(liquids_perc, by = c("inventory_year", "livestock_type")) %>%
  left_join(solids_perc, by = c("inventory_year", "livestock_type")) %>%
  mutate(
    liquid_perc = replace_na(liquid_perc, 0),
    solid_perc = replace_na(solid_perc, 0)
  ) %>%
  mutate(
    mt_n2o_liquids = head_count *
      kg_nex_head_yr *
      (1 - ag_constants_vec["VolPercent"]) *
      liquid_perc *
      ag_constants_vec["LiquidEF"] *
      ag_constants_vec["N2O_N2"] / 1000,
    mt_n2o_solids = head_count *
      kg_nex_head_yr *
      (1 - ag_constants_vec["VolPercent"]) *
      solid_perc *
      ag_constants_vec["SolidEF"] *
      ag_constants_vec["N2O_N2"] / 1000,
    mt_co2e_liquids = mt_n2o_liquids * gwp_list$n2o,
    mt_co2e_solids = mt_n2o_solids * gwp_list$n2o
  ) %>%
  pivot_longer(
    cols = c(mt_co2e_liquids, mt_co2e_solids, mt_n2o_liquids, mt_n2o_solids),
    names_to = c(".value", "storage_state"),
    names_pattern = "(mt_[a-z0-9]+)_(liquid|solid)"
  ) %>%
  mutate(storage_state = stringr::str_to_title(storage_state)) %>%
  group_by(inventory_year, geog_name, county_name, livestock_type, storage_state) %>%
  summarize(
    mt_n2o = sum(mt_n2o, na.rm = TRUE),
    mt_co2e_by_storage = sum(mt_co2e, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  mutate(mt_co2e_caf = case_when(
    inventory_year <= 2027 ~ mt_co2e_by_storage,
    inventory_year > 2027 &
      livestock_type %in% c(
        "Calves",
        "Dairy Cows",
        "Feedlot Cattle"
      ) &
      storage_state == "Liquid" ~ mt_co2e_by_storage * (1 + caf_manure_coefs$dairy_n2o),
    inventory_year > 2027 &
      livestock_type %in% c("Swine") &
      storage_state == "Liquid" ~ mt_co2e_by_storage * (1 + caf_manure_coefs$swine_n2o),
    inventory_year > 2027 &
      livestock_type %in% c("Beef Cows") &
      storage_state == "Solid" ~ mt_co2e_by_storage * (1 + caf_manure_coefs$beef_n2o),
    inventory_year > 2027 &
      livestock_type %in% c(
        "Broilers",
        "Layers",
        "Pullets",
        "Turkeys"
      ) &
      storage_state == "Solid" ~ mt_co2e_by_storage * (1 + caf_manure_coefs$poultry_n2o),
    TRUE ~ mt_co2e_by_storage
  )) %>%
  ungroup()

# ===== N2O EMISSIONS FROM INDIRECT RUNOFF =====

KN_excretion <- livestock_df %>%
  left_join(nex_data, by = c("inventory_year", "livestock_type")) %>%
  mutate(total_kn_excretion_kg = head_count * kg_nex_head_yr)

nex_runoff_emissions <- KN_excretion %>%
  group_by(inventory_year, geog_name, county_name, livestock_type) %>%
  summarize(
    mt_total_kn_excretion = sum(total_kn_excretion_kg / 1000),
    .groups = "drop"
  ) %>%
  mutate(
    mt_n = mt_total_kn_excretion * (1 - ag_constants_vec["VolPercent"]) *
      ag_constants_vec["LeachEF"],
    mt_n2o = mt_n * ag_constants_vec["LeachEF2"] * ag_constants_vec["N2O_N2"],
    mt_co2e = mt_n2o * gwp_list$n2o
  ) %>%
  left_join(manure_split, by = c("inventory_year", "livestock_type")) %>%
  mutate(
    mt_n2o_by_storage = mt_n2o * percentage,
    mt_co2e_by_storage = mt_co2e * percentage
  ) %>%
  group_by(inventory_year, geog_name, county_name, livestock_type, storage_state) %>%
  summarize(
    mt_n2o = sum(mt_n2o_by_storage, na.rm = TRUE),
    mt_co2e_by_storage = sum(mt_co2e_by_storage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  mutate(mt_co2e_caf = case_when(
    inventory_year <= 2027 ~ mt_co2e_by_storage,
    inventory_year > 2027 &
      livestock_type %in% c(
        "Calves",
        "Dairy Cows",
        "Feedlot Cattle"
      ) &
      storage_state == "Liquid" ~ mt_co2e_by_storage * (1 + caf_manure_coefs$dairy_n2o),
    inventory_year > 2027 &
      livestock_type %in% c("Swine") &
      storage_state == "Liquid" ~ mt_co2e_by_storage * (1 + caf_manure_coefs$swine_n2o),
    inventory_year > 2027 &
      livestock_type %in% c("Beef Cows") &
      storage_state == "Solid" ~ mt_co2e_by_storage * (1 + caf_manure_coefs$beef_n2o),
    inventory_year > 2027 &
      livestock_type %in% c(
        "Broilers",
        "Layers",
        "Pullets",
        "Turkeys"
      ) &
      storage_state == "Solid" ~ mt_co2e_by_storage * (1 + caf_manure_coefs$poultry_n2o),
    TRUE ~ mt_co2e_by_storage
  )) %>%
  ungroup()

# ===== N2O EMISSIONS FROM DIRECT SOIL APPLICATION =====

# Calculate management type percentages
manure_mgmt_perc <- ghg.ccap::agriculture_variables$manure_mgmt %>%
  mutate(management_type = case_when(
    managed == "Yes" ~ "Managed",
    mgmt_system %in% c(
      "Pasture", "PRP", "Dry Lot", "Range",
      "Pasture, Range & Paddock"
    ) ~ "Pasture_range",
    mgmt_system == "Daily Spread" ~ "Daily_spread"
  )) %>%
  group_by(inventory_year, livestock_type, management_type) %>%
  summarize(percentage = sum(percentage), .groups = "drop") %>%
  ungroup()

# Get percentages for each management type
managed_perc <- manure_mgmt_perc %>%
  filter(management_type == "Managed") %>%
  select(inventory_year, livestock_type, percent_managed = percentage)

daily_spread_perc <- manure_mgmt_perc %>%
  filter(management_type == "Daily_spread") %>%
  select(inventory_year, livestock_type, percent_daily_spread = percentage)

pasture_perc <- manure_mgmt_perc %>%
  filter(management_type == "Pasture_range") %>%
  select(inventory_year, livestock_type, percent_pasture = percentage)

manure_soils <- KN_excretion %>%
  left_join(managed_perc, by = c("inventory_year", "livestock_type")) %>%
  left_join(daily_spread_perc, by = c("inventory_year", "livestock_type")) %>%
  left_join(pasture_perc, by = c("inventory_year", "livestock_type")) %>%
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
  group_by(inventory_year, geog_name, county_name, livestock_type) %>%
  summarize(
    mt_n2o = sum(MT_n2o_manure_application + MT_n2o_pasture, na.rm = TRUE),
    mt_co2e = sum(MT_co2e_manure_application + MT_co2e_pasture, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    storage_state = "Applied",
    mt_co2e_caf = mt_co2e
  ) %>%
  ungroup()

# ===== COMBINE ALL RESULTS =====
agriculture_manure_caf <- bind_rows(
  ch4_emissions %>%
    rename(
      mt_gas = mt_ch4_by_storage,
      mt_co2e_bau = mt_co2e_by_storage,
      mt_co2e_alt = mt_co2e_caf
    ) %>%
    mutate(gas_type = "ch4", source = "manure_management"),
  n2o_emissions %>%
    rename(
      mt_gas = mt_n2o,
      mt_co2e_bau = mt_co2e_by_storage,
      mt_co2e_alt = mt_co2e_caf
    ) %>%
    mutate(gas_type = "n2o", source = "manure_management"),
  nex_runoff_emissions %>%
    rename(
      mt_gas = mt_n2o,
      mt_co2e_bau = mt_co2e_by_storage,
      mt_co2e_alt = mt_co2e_caf
    ) %>%
    mutate(gas_type = "n2o", source = "indirect_manure_runoff"),
  manure_soils_emissions %>%
    rename(
      mt_gas = mt_n2o,
      mt_co2e_bau = mt_co2e,
      mt_co2e_alt = mt_co2e_caf
    ) %>%
    mutate(gas_type = "n2o", source = "direct_manure_soil")
) %>%
  select(
    inventory_year, geog_name, county_name, livestock_type, storage_state,
    gas_type, source, mt_gas, mt_co2e_bau, mt_co2e_alt
  ) %>%
  pivot_longer(
    cols = starts_with("mt_co2e_"),
    names_to = "scenario",
    names_prefix = "mt_co2e_",
    values_to = "value_emissions"
  )

usethis::use_data(agriculture_manure_caf, overwrite = T)
