#### process MPCA CAF coefficients ####
#### This script uses coefficients from the MN CAF (CometFarm model) to estimate
#### expected GHG reductions from agricultural strategies
#### per conversation with MPCA, ~ 2.5% of land is irrigated, so taking non-irrigated values here

library(readr)

#load in statewide baseline values (county by county available if desired)
cometfarm_baseline <- read_csv("./data-raw/agriculture/cf_baseline_values_county.csv", skip = 1) %>%
  janitor::clean_names() %>%
  filter(irrigated == "N", tillage_starting_point == "intensive") %>%
  select(geog_name = county,
         co2e_per_acre = avg_total_ghg_co2)

# take weighted statewide average for now
cometfarm_coefs <- read_csv("./data-raw/agriculture/ag_emissions_factors_caf.csv") %>%
  filter(county %in% c("DAKOTA", "CARVER", "SCOTT", "HENNEPIN", "ANOKA", "WASHINGTON", "RAMSEY"))

cover_crop_strategies <- cometfarm_coefs %>%
  filter(grepl("Cover Crop",cps_name)) %>%
  distinct(planner_implementation)

cover_crop <- cometfarm_coefs %>%
  filter(planner_implementation == "Add Non-Legume Seasonal Cover Crop to Intensive Till Non-Irrigated Cropland")

tillage_strategies <- cometfarm_coefs %>%
  filter(grepl("Tillage Management",cps_name)) %>%
  distinct(planner_implementation)

no_till <- cometfarm_coefs %>%
  filter(planner_implementation == "Intensive Till to No Till or Strip Till on Non-Irrigated Cropland")
