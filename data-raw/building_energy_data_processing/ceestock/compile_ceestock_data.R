### import CEEStock data for electrification and retrofit elasticities

# library(dplyr, tidyr, readr)


ceestock <- readr::read_csv("./data-raw/building_energy_data_processing/ceestock/ceestcok_savings_v1.csv") %>%
  janitor::clean_names() %>%
  mutate(
    mc_classification = case_when(
      model_geometry_building_type_acs == "Single-Family Detached" ~ "single_family_detached",
      TRUE ~ "single_family_attached"
    ),         sqft_bin = stringr::str_replace_all(model_geometry_floor_area,
                                                   "-", " to "),
    # convert elec mmbtu to mwh
    across(
      .cols = matches("elec") & !matches("pct"),
      .fns  = ~ .x * 0.2933, ### electricity mmbtu to mwh
      .names = "{.col}"
    ),
    # convert nat gas mmbtu to mcf
    across(
      .cols = matches("gas") & !matches("pct"),
      .fns  = ~ .x * 0.963, # nat gas mmbtu to mcf
      .names = "{.col}"
    )
  ) %>% # and rename them properly
  rename_with(
    ~ stringr::str_replace(.x, "mm_btu", "mwh"),
    matches("elec") & !matches("pct")
  ) %>%
  rename_with(
    ~ stringr::str_replace(.x, "mm_btu", "mcf"),
    matches("gas") & !matches("pct")
  ) %>%
  rename_with(
    ~ stringr::str_replace(.x, "savings", "change")
  ) %>%
  mutate(gas_other_mcf = gas_mcf - gas_heat_mcf) # pull out gas appliance (other))

#### baseline read in ####

cee_baseline_sf <- ceestock %>%
  filter(model_heating_fuel == "Natural Gas", # discounting dwellings already using electricity for now
         scenario == "Baseline") %>%
  group_by(scenario, model_vintage_acs, sqft_bin, mc_classification) %>%
  summarize(elec_mwh = mean(elec_mwh),
            gas_heat_mcf= mean(gas_heat_mcf ),
            gas_mcf = mean(gas_mcf),
            gas_other_mcf = mean(gas_other_mcf)
  ) %>%
  ungroup() %>%
  rename(build_year = model_vintage_acs)

### retrofits

cee_retrofit_sf <- ceestock %>%
  filter(model_heating_fuel == "Natural Gas", # discounting dwellings already using electricity for now
         scenario == "Only Wx") %>% # building envelope improvement per CEE
  mutate(scenario = "Retrofit") %>%
  group_by(scenario, model_vintage_acs, sqft_bin, mc_classification) %>%
  summarize(elec_mwh = mean(elec_mwh),
            elec_change_mwh = mean(elec_change_mwh),
            gas_mcf = mean(gas_mcf),
            gas_change_mcf = mean(gas_change_mcf)
  ) %>%
  ungroup() %>%
  rename(build_year = model_vintage_acs)

#### electrification (heating) ####
### because cee packages heat pumps with appliance electrification, need to pull out the heat effects here

cee_heatpump_sf <- ceestock %>%
  filter(model_heating_fuel == "Natural Gas", # discounting dwellings already using electricity for now
         scenario == "Dual Fuel 80% No Wx") %>% # heat pump and ALL appliances to electricity per CEE
  mutate(
    scenario = "Heatpump",
    mc_classification = case_when(
    model_geometry_building_type_acs == "Single-Family Detached" ~ "single_family_detached",
    TRUE ~ "single_family_attached"
  ),         sqft_bin = stringr::str_replace_all(model_geometry_floor_area,
                                                 "-", " to "),
  gas_other_mcf = (gas_mcf - gas_heat_mcf),
  gas_other_change_mcf = gas_change_mcf - gas_heat_change_mcf,
  gas_mcf = gas_mcf - gas_other_change_mcf,
  elec_mwh = elec_mwh - elec_other_change_mwh,
  gas_change_mcf = gas_heat_change_mcf,
  elec_change_mwh = elec_heat_change_mwh + elec_cool_change_mwh
  ) %>%
  group_by(scenario, model_vintage_acs, sqft_bin, mc_classification) %>%
  summarize(elec_mwh = mean(elec_mwh),
            elec_change_mwh = mean(elec_change_mwh),
            gas_mcf = mean(gas_mcf),
            gas_change_mcf = mean(gas_change_mcf)
            ) %>%
  ungroup() %>%
  rename(build_year = model_vintage_acs)

#### electrification (appliances) ####
### because cee packages heat pumps with appliance electrification, need to pull out the heat effects here

cee_appliance_sf <- ceestock %>%
  filter(model_heating_fuel == "Natural Gas", # discounting dwellings already using electricity for now
         scenario == "Dual Fuel 80% No Wx") %>% # heat pump and ALL appliances to electricity per CEE
  mutate(
    scenario = "Electric appliances",
    mc_classification = case_when(
    model_geometry_building_type_acs == "Single-Family Detached" ~ "single_family_detached",
    TRUE ~ "single_family_attached"
  ),         sqft_bin = stringr::str_replace_all(model_geometry_floor_area,
                                                 "-", " to "),
  gas_other_mcf = (gas_mcf - gas_heat_mcf),
  gas_change_mcf = gas_change_mcf - gas_heat_change_mcf,
  gas_mcf = gas_mcf - gas_heat_change_mcf,
  elec_mwh = elec_mwh - (elec_heat_change_mwh + elec_cool_change_mwh),
  elec_change_mwh = elec_other_change_mwh
  )  %>%
  group_by(scenario, model_vintage_acs, sqft_bin, mc_classification) %>%
  summarize(elec_mwh = mean(elec_mwh),
            elec_change_mwh = mean(elec_change_mwh),
            gas_mcf = mean(gas_mcf),
            gas_change_mcf = mean(gas_change_mcf)
  ) %>%
  ungroup() %>%
  rename(build_year = model_vintage_acs)

#### electrification + retrofit ####
#### no expectation for appliances to interact with retrofit

cee_combined_sf <- ceestock %>%
  filter(model_heating_fuel == "Natural Gas", # discounting dwellings already using electricity for now
         scenario == "Dual Fuel 80%") %>% # heat pump and ALL appliances to electricity per CEE
  mutate(
    scenario = "Retrofit and heatpump",
    mc_classification = case_when(
    model_geometry_building_type_acs == "Single-Family Detached" ~ "single_family_detached",
    TRUE ~ "single_family_attached"
  ),         sqft_bin = stringr::str_replace_all(model_geometry_floor_area,
                                                 "-", " to "),
  gas_other_mcf = (gas_mcf - gas_heat_mcf),
  gas_other_change_mcf = gas_change_mcf - gas_heat_change_mcf,
  gas_mcf = gas_mcf - gas_other_change_mcf,
  elec_mwh = elec_mwh - elec_other_change_mwh,
  gas_change_mcf = gas_heat_change_mcf,
  elec_change_mwh = elec_heat_change_mwh + elec_cool_change_mwh
  ) %>%
  group_by(scenario, model_vintage_acs, sqft_bin, mc_classification) %>%
  summarize(elec_mwh = mean(elec_mwh),
            elec_change_mwh = mean(elec_change_mwh),
            gas_mcf = mean(gas_mcf),
            gas_change_mcf = mean(gas_change_mcf)
  ) %>%
  ungroup() %>%
  rename(build_year = model_vintage_acs)




# summary list

ceestock_summaries <- list(

  cee_baseline_sf = cee_baseline_sf,
  cee_heatpump_sf = cee_heatpump_sf,
  cee_retrofit_sf = cee_retrofit_sf,
  cee_appliance_sf = cee_appliance_sf,
  cee_combined_sf = cee_combined_sf

)


usethis::use_data(ceestock_summaries, overwrite = TRUE)
