#### process MPCA CAF coefficients ####
#### This script uses coefficients from the MN CAF (CometFarm model) to estimate
#### expected GHG reductions from agricultural strategies
#### per conversation with MPCA, ~ 2.5% of land is irrigated, so taking non-irrigated values here

library(readr)

#load in statewide baseline values (county by county available if desired)
cometfarm_baseline <- read_csv("./data-raw/agriculture/cf_baseline_values_gas.csv") %>%
  janitor::clean_names() %>%
  filter(irrigated == "N", tillage_starting_point == "intensive") %>%
  select(geog_name = county,
         baseline_co2_per_acre = avg_co2_mean,
         baseline_n2o_per_acre = avg_n2o_mean,
         baseline_co2e_per_acre = avg_total_ghg_co2)

# take weighted statewide average for now
cometfarm_coefs <- read_csv("./data-raw/agriculture/ag_emissions_factors_caf_v3.csv") %>%
  filter(county %in% c("DAKOTA", "CARVER", "SCOTT", "HENNEPIN", "ANOKA", "WASHINGTON", "RAMSEY"))

cover_crop_strategies <- cometfarm_coefs %>%
  filter(grepl("Cover Crop",cps_name)) %>%
  distinct(planner_implementation)

#cover crops
cover_crop <- cometfarm_coefs %>%
  filter(planner_implementation == "Add Non-Legume Seasonal Cover Crop (with 25% Fertilizer N Reduction) to Non-Irrigated Cropland") %>%
  mutate(strategy = "Non-legume cover crop")

tillage_strategies <- cometfarm_coefs %>%
  filter(grepl("Tillage Management",cps_name)) %>%
  distinct(planner_implementation)

#no till
no_till <- cometfarm_coefs %>%
  filter(planner_implementation == "Intensive Till to No Till or Strip Till on Non-Irrigated Cropland") %>%
  mutate(strategy = "No till")

# Reduce till
red_till <- cometfarm_coefs %>%
  filter(planner_implementation == "Intensive Till to Reduced Till on Non-Irrigated Cropland")%>%
  mutate(strategy = "Reduced till")

# CC + no till
combo_strategies <- cometfarm_coefs %>%
  filter(grepl("Multiple Conservation Practices",cps_name)) %>%
  distinct(planner_implementation)

cc_till <- cometfarm_coefs %>%
  filter(planner_implementation == "Intensive Till to No Till or Strip Till (CPS 329) + Add Non-Legume Seasonal Cover Crop (CPS 340) (with 25% Fertilizer N Reduction) on Non-Irrigated Croplands") %>%
  mutate(strategy = "Cover crop and no till")

# Comet farm C sequestration estimates are unreasonably high. Capping them at literature values (which are probably STILL too high)
# No-till 0.3 Mg C ha–1 yr –1 = 0.4453846: https://www.nature.com/articles/nclimate2292
# Cover crop 0.32 Mg C ha-1 yr-1 = 0.4750769 (This is similar to our grassland seq value, but for adding winter crops?): https://doi.org/10.1016/j.agee.2014.10.024
# Cover crops in V3.1 are more reasonable than v4, so cover crop cap is not needed currently
no_till_cap <- 0.445
#cover_crop_cap <- 0.475

no_till_adj <- no_till %>%
  mutate(mean_co2_alt = case_when(
  mean_co2 > no_till_cap ~ no_till_cap,
  TRUE ~ mean_co2),
  co2_adj = mean_co2_alt - mean_co2
) %>%
  select(county, co2_adj)

no_till <- no_till %>%
  left_join(no_till_adj) %>%
  mutate(mean_co2 = mean_co2 + co2_adj)

cc_till <- cc_till %>%
  left_join(no_till_adj) %>%
  mutate(mean_co2 = mean_co2 + co2_adj)

### compare all to baseline

agriculture_regen_ag_caf <- bind_rows(cover_crop %>%
                                   select(county, mean_co2, mean_n2o, strategy),
                                 no_till %>%
                                   select(county, mean_co2, mean_n2o, strategy),
                                 cc_till %>%
                                   select(county, mean_co2, mean_n2o, strategy)) %>%
  mutate(geog_name = stringr::str_to_sentence(county)) %>%
  left_join(cometfarm_baseline) %>%
  ### taking ratios to nitrogen emissions now since that's what is available in our inventory
  mutate(c_seq_ratio = mean_co2 / baseline_n2o_per_acre,
         n2o_emis_ratio = (-1 * mean_n2o) / baseline_n2o_per_acre) #cometfarm inverts emission/sequestration, so changing n2o to match inventory

usethis::use_data(agriculture_regen_ag_caf, overwrite=T)
