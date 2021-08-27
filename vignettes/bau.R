############# Setup BAU scenario ###############################
scen <- "BAU"
bau_summary <- scenario_results(scen, elec_scen, aeo_scen, ch_ctu)

# Change thousands of miles/ton-miles to hundreds millions of miles/ton-miles
# Change metric tonnes of GHG for embodied to thousands of metric tonnes of GHG
bau_summary <- bau_summary %>% mutate(across(all_of(YRS), ~ case_when(
  ((output == "VMT") | (output == "TVMT")) ~ .x / 10^5,
  ((output == "INDIR-GHG") | (output == "PETRO") |
     (output == "ELEC")) ~ .x / 10^3, TRUE ~ .x
)))

# Some CTU do not have BRT, etc. so will have a NaN entry for it. Need to convert to zero so summation can be performed across powertrain classes.
bau_summary <- bau_summary %>% mutate(across(
  all_of(YRS), ~ replace(., is.nan(.), 0)))

####### Create plots for the BAU results #######
## Cumulative VMT plot by mode for personal vehicles
mode_summary <- bau_summary %>%
  filter(type == "P", output == "VMT") %>%
  group_by(mode) %>%
  select(append("mode", YRS)) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(YRS) %>%
  pivot_wider(name, mode)
