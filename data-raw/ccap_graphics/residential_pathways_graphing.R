#### Run residential buildings BAU

library(purrr)
library(dplyr)

ctu_index <- geog_index %>%
  filter(geog_level != "COUNTY")

# regional density_output
density_output <- run_scenario_land_use()

# bau
bau_results <- purrr::map_dfr(
  ctu_index$geog_name,
  ~ run_scenario_building(
    .baseline_year = 2022,
    .selected_ctu = .x,  # iterates across al ctus
    .density_output = density_output,
  ),
  .id = "ctu"
) %>%
  group_by(inventory_year, scenario) %>%
  summarize(mwh = sum(residential_mwh),
            mcf = sum(residential_mcf),
            electricity_emissions = sum(electricity_emissions),
            natgas_emissions = sum(natural_gas_emissions)) %>%
  ungroup() %>%
  filter(scenario == "bau")


net_zero_results <- purrr::map_dfr(
  ctu_index$geog_name,
  ~ run_scenario_building(
    .baseline_year = 2022,
    .scenario = "net_zero",
    .selected_ctu = .x,  # iterates across al ctus
    .density_output = density_output,
    .new_sf_homes_leed_gold_pct = 1.0,
    .new_mf_homes_leed_gold_pct = 1.0,
    .retrofit_start_year = 2028,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 1.0,
    .existing_mf_retrofit_pct = 1.0,
    # electrification
    .heatpump_start_year = 2028,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 1.0,
    .mf_heat_pump_pct = 1.0
  ),
  .id = "ctu"
) %>%
  group_by(inventory_year, scenario) %>%
  summarize(mwh = sum(residential_mwh),
            mcf = sum(residential_mcf),
            electricity_emissions = sum(electricity_emissions),
            natgas_emissions = sum(natural_gas_emissions)) %>%
  ungroup() %>%
  filter(scenario == "net_zero")

### potential policy pathways

ppp_results <- purrr::map_dfr(
  ctu_index$geog_name,
  ~ run_scenario_building(
    .baseline_year = 2022,
    .scenario = "ppp",
    .selected_ctu = .x,  # iterates across al ctus
    .density_output = density_output,
    .new_sf_homes_leed_gold_pct = 0.5,
    .new_mf_homes_leed_gold_pct = 0.5,
    .retrofit_start_year = 2028,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0.5,
    .existing_mf_retrofit_pct = 0.5,
    # electrification
    .heatpump_start_year = 2028,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0.5,
    .mf_heat_pump_pct = 0.5
  ),
  .id = "ctu"
) %>%
  group_by(inventory_year, scenario) %>%
  summarize(mwh = sum(residential_mwh),
            mcf = sum(residential_mcf),
            electricity_emissions = sum(electricity_emissions),
            natgas_emissions = sum(natural_gas_emissions)) %>%
  ungroup() %>%
  filter(scenario == "ppp")
