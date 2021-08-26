############### Setup alternative scenario ######################
# Selection of certain buttons in the tool should update values in a scenario-specific version of the tables
# Example: pass_transpo could be updated by mutate(AnnualPMT = AnnualPMT*land_use_elasticity)

#### USER INPUTS - will be in a UI when transitioned to Met Council Shiny app ####
# Define a default starting list for treatments
TREATMENT_DEF <- c(rep(0, length(YRS)))
# Change in AVO in final year
transit_avo <- 0 # (1, 10, 50, etc.)
# Interpolate AVO change across years
transit_avo <- calc_elasticity(TREATMENT_DEF, transit_avo, length(INIT_YRS), length(FOR_YRS))
# Change in ridership in final year
transit_rider <- 0 # (1, 10, 50, etc.)
# Interpolate ridership change across years
transit_rider <- calc_elasticity(TREATMENT_DEF, transit_rider, length(INIT_YRS), length(FOR_YRS))

vmt_price <- 0 # (3-25 cents per mile typical in literature)
fvmt_price <- 0 # (3-25 cents per mile typical in literature)
payd_ins <- 0 # (6.6 cents typical - shouldn't allow user to impose both a VMT and PAYD fee)
gas_price <- 0 # Measured in cents/gal. 18 cents per gal is current federal rate and up to 50 cents per gas imposed by some states
park_price <- 0 # Measured in $ per hour
cong_price <- 0 # Typically slightly higher than a baseline VMT price

# Land use changes
# Population density - % increase/decrease relative to BAU
pop_dens <- 0
# Jobs density - % increase/decrease relative to BAU
emp_dens <- 0
# Diversity - % increase/decrease relative to BAU
diverse <- 0
# % 4-way stops - % increase/decrease relative to BAU
design <- 0
# Job accessibility by auto, transit, or walk/bike (accessibility for auto/transit + 1 mile for walk/bike)
job_access <- 0
# Distance to transit - % increase/decrease relative to BAU
trans_dist <- 0
# Combined density - % increase/decrease relative to BAU (NOTE: do not apply in combination with above land use measures)
cpop_dens <- 0
# Check combined 5D effect < 25% reduction in auto VMT (NOTE: employment density has negligible effect on auto VMT according to literature)
comb_5d_impact_dr <- ifelse(cpop_dens > 0, cpop_dens / 100 * ELAST_CDENS_DR[length(ELAST_CDENS_DR)], ((pop_dens / 100 * ELAST_DENS_DR_POP[length(ELAST_DENS_DR_POP)]) * (diverse / 100 * ELAST_DIVER_DR[length(ELAST_DIVER_DR)]) * (design / 100 * ELAST_DES_DR[length(ELAST_DES_DR)]) * (trans_dist / 100 * ELAST_DIST_DR[length(ELAST_DIST_DR)])))
if (comb_5d_impact_dr < MAX_5D_DR) {
  print(paste("WARNING", comb_5d_impact_dr, "is bigger than", MAX_5D_DR, ". Using", MAX_5D_DR))
}
# Telwork: % of additional workers who choose to telecommute on a given day in final year (vs. BAU)
telework <- 0
# Interpolate telework change across years
telework <- calc_elasticity(TREATMENT_DEF, telework, length(INIT_YRS), length(FOR_YRS))

# DRS (percent indicator of whether to introduce DRS in 2025.)
drs <- 0
# What is the fuel type for DRS? Assume you have to buy enough vehicles to operate service. Use for cost, too.
drs_fuel <- "BEV"

# Is AV selected?
av <- 0
# What is the fuel type for AV? Assume you have to buy enough vehicles to operate service. Use for cost, too.
av_fuel <- "BEV"

### VALIDATION - Total of BEV, HEV, and PHEV must be <100%
bev_share <- 0
phev_share <- 0
hev_share <- 0

# # If the user didn't input anything for ev, then use the non-ev treatment numbers
# if (hev_share==0 & phev_share==0 & bev_share==0){
#   hev_share = (pass_transpo %>% filter(mode=="PLDV", var=="HEVSales") %>% select(FIN_YR) /
#                   pass_transpo %>% filter(mode=="PLDV", var=="TotSales") %>% select(FIN_YR)) * 100
#   phev_share = (pass_transpo %>% filter(mode=="PLDV", var=="PHEVSales") %>% select(FIN_YR) /
#                    pass_transpo %>% filter(mode=="PLDV", var=="TotSales") %>% select(FIN_YR)) * 100
#   bev_share = (pass_transpo %>% filter(mode=="PLDV", var=="BEVSales") %>% select(FIN_YR) /
#                   pass_transpo %>% filter(mode=="PLDV", var=="TotSales") %>% select(FIN_YR)) * 100
# }

scen <- "MIT"
############# END OF USER INPUTS ###############################################



# Run the land use treatments
mit_land_summary <- scenario_results(scen, elec_scen, aeo_scen, ch_ctu, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, drs_fuel, av_fuel, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr, 0, bau_summary)
# Change thousands of miles/ton-miles to 100s millions of miles/ton-miles and thousands gal/kWh to millions gal/kWh
mit_land_summary <- mit_land_summary %>% mutate(across(all_of(YRS), ~ case_when(
  ((output == "VMT") | (output == "TVMT")) ~ .x / 10^5,
  ((output == "INDIR-GHG") | (output == "PETRO") | (output == "ELEC")) ~ .x / 10^3, TRUE ~ .x
)))
# Some CTU do not have BRT, etc. so will have a NaN entry for it. Need to convert to zero so summation can be performed across powertrain classes.
mit_land_summary <- mit_land_summary %>% mutate(across(all_of(YRS), ~ replace(., is.nan(.), 0)))

# Run the economic instruments treatments
# Call function to adjust fleet composition in response to user inputs for economic instruments
temp_pass_transpo <- pass_transpo
temp_freight_transpo <- freight_transpo
if (vmt_price != 0 & payd_ins != 0 & gas_price != 0) {
  adj_fleet <- adj_fleet_shares(0, 0, 0, pass_transpo, freight_transpo, vmt_price, payd_ins, gas_price, 0, 0, ch_ctu)
  pass_transpo <- adj_fleet$pass
  freight_transpo <- adj_fleet$freight
}
mit_price_summary <- scenario_results(scen, elec_scen, aeo_scen, ch_ctu, 0, 0, vmt_price, payd_ins, gas_price, park_price, cong_price, fvmt_price, 0, 0, drs_fuel, av_fuel, 0, 0, 0, 0, 0, 0, 0, 0, bau_summary)
# Change thousands of miles/ton-miles to 100s millions of miles/ton-miles and thousands gal/kWh to millions gal/kWh
mit_price_summary <- mit_price_summary %>% mutate(across(all_of(YRS), ~ case_when(
  ((output == "VMT") | (output == "TVMT")) ~ .x / 10^5,
  ((output == "INDIR-GHG") | (output == "PETRO") | (output == "ELEC")) ~ .x / 10^3, TRUE ~ .x
)))
# Some CTU do not have BRT, etc. so will have a NaN entry for it. Need to convert to zero so summation can be performed across powertrain classes.
mit_price_summary <- mit_price_summary %>% mutate(across(all_of(YRS), ~ replace(., is.nan(.), 0)))

# Run the DRS treatments
# Call function to adjust fleet composition in response to user inputs for DRS
# Revert changes from economic treatments
pass_transpo <- temp_pass_transpo
freight_transpo <- temp_freight_transpo
temp_pass_transpo <- pass_transpo
temp_freight_transpo <- freight_transpo
if (drs != 0) {
  adj_fleet <- adj_fleet_shares(0, 0, 0, pass_transpo, freight_transpo, 0, 0, 0, drs, 0, ch_ctu)
  pass_transpo <- adj_fleet$pass
  freight_transpo <- adj_fleet$freight
}
mit_drs_summary <- scenario_results(scen, elec_scen, aeo_scen, ch_ctu, 0, 0, 0, 0, 0, 0, 0, 0, drs, 0, drs_fuel, av_fuel, 0, 0, 0, 0, 0, 0, 0, 0, bau_summary)
# Change thousands of miles/ton-miles to 100s millions of miles/ton-miles and thousands gal/kWh to millions gal/kWh
mit_drs_summary <- mit_drs_summary %>% mutate(across(all_of(YRS), ~ case_when(
  ((output == "VMT") | (output == "TVMT")) ~ .x / 10^5,
  ((output == "INDIR-GHG") | (output == "PETRO") | (output == "ELEC")) ~ .x / 10^3, TRUE ~ .x
)))
# Some CTU do not have BRT, etc. so will have a NaN entry for it. Need to convert to zero so summation can be performed across powertrain classes.
mit_drs_summary <- mit_drs_summary %>% mutate(across(all_of(YRS), ~ replace(., is.nan(.), 0)))

# Run the AV treatments
# Call function to adjust fleet composition in response to user inputs for AVs
# Revert changes from DRS treatments
pass_transpo <- temp_pass_transpo
freight_transpo <- temp_freight_transpo
temp_pass_transpo <- pass_transpo
temp_freight_transpo <- freight_transpo
if (av != 0) {
  adj_fleet <- adj_fleet_shares(0, 0, 0, pass_transpo, freight_transpo, 0, 0, 0, 0, av, ch_ctu)
  pass_transpo <- adj_fleet$pass
  freight_transpo <- adj_fleet$freight
}

mit_av_summary <- scenario_results(scen, elec_scen, aeo_scen, ch_ctu, 0, 0, 0, 0, 0, 0, 0, 0, 0, av, drs_fuel, av_fuel, 0, 0, 0, 0, 0, 0, 0, 0, bau_summary)
# Change thousands of miles/ton-miles to 100s millions of miles/ton-miles and thousands gal/kWh to millions gal/kWh
mit_av_summary <- mit_av_summary %>% mutate(across(all_of(YRS), ~ case_when(
  ((output == "VMT") | (output == "TVMT")) ~ .x / 10^5,
  ((output == "INDIR-GHG") | (output == "PETRO") | (output == "ELEC")) ~ .x / 10^3, TRUE ~ .x
)))
# Some CTU do not have BRT, etc. so will have a NaN entry for it. Need to convert to zero so summation can be performed across powertrain classes.
mit_av_summary <- mit_av_summary %>% mutate(across(all_of(YRS), ~ replace(., is.nan(.), 0)))

# Run the transit treatments
mit_transit_summary <- scenario_results(scen, elec_scen, aeo_scen, ch_ctu, transit_avo, transit_rider, 0, 0, 0, 0, 0, 0, 0, 0, drs_fuel, av_fuel, 0, 0, 0, 0, 0, 0, 0, 0, bau_summary)
# Change thousands of miles/ton-miles to 100s millions of miles/ton-miles and thousands gal/kWh to millions gal/kWh
mit_transit_summary <- mit_transit_summary %>% mutate(across(all_of(YRS), ~ case_when(
  ((output == "VMT") | (output == "TVMT")) ~ .x / 10^5,
  ((output == "INDIR-GHG") | (output == "PETRO") | (output == "ELEC")) ~ .x / 10^3, TRUE ~ .x
)))
# Some CTU do not have BRT, etc. so will have a NaN entry for it. Need to convert to zero so summation can be performed across powertrain classes.
mit_transit_summary <- mit_transit_summary %>% mutate(across(all_of(YRS), ~ replace(., is.nan(.), 0)))

# Run the telework treatments
mit_tele_summary <- scenario_results(scen, elec_scen, aeo_scen, ch_ctu, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, drs_fuel, av_fuel, 0, 0, 0, 0, 0, 0, 0, telework, bau_summary)
# Change thousands of miles/ton-miles to 100s millions of miles/ton-miles and thousands gal/kWh to millions gal/kWh
mit_tele_summary <- mit_tele_summary %>% mutate(across(all_of(YRS), ~ case_when(
  ((output == "VMT") | (output == "TVMT")) ~ .x / 10^5,
  ((output == "INDIR-GHG") | (output == "PETRO") | (output == "ELEC")) ~ .x / 10^3, TRUE ~ .x
)))
# Some CTU do not have BRT, etc. so will have a NaN entry for it. Need to convert to zero so summation can be performed across powertrain classes.
mit_tele_summary <- mit_tele_summary %>% mutate(across(all_of(YRS), ~ replace(., is.nan(.), 0)))

# Run the vehicle electrification treatments
# Revert changes from AV treatments
pass_transpo <- temp_pass_transpo
freight_transpo <- temp_freight_transpo
if (bev_share != 0 & phev_share != 0 & hev_share != 0) {
  # Call function to adjust fleet composition in response to user inputs for EVs
  adj_fleet <- adj_fleet_shares(bev_share, phev_share, hev_share, pass_transpo, freight_transpo, 0, 0, 0, 0, 0, ch_ctu)
  pass_transpo <- adj_fleet$pass
  freight_transpo <- adj_fleet$freight
}

mit_ev_summary <- scenario_results(scen, elec_scen, aeo_scen, ch_ctu, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, drs_fuel, av_fuel, 0, 0, 0, 0, 0, 0, 0, 0, bau_summary)
# Change thousands of miles/ton-miles to 100s millions of miles/ton-miles and thousands gal/kWh to millions gal/kWh
mit_ev_summary <- mit_ev_summary %>% mutate(across(all_of(YRS), ~ case_when(
  ((output == "VMT") | (output == "TVMT")) ~ .x / 10^5,
  ((output == "INDIR-GHG") | (output == "PETRO") | (output == "ELEC")) ~ .x / 10^3, TRUE ~ .x
)))
# Some CTU do not have BRT, etc. so will have a NaN entry for it. Need to convert to zero so summation can be performed across powertrain classes.
mit_ev_summary <- mit_ev_summary %>% mutate(across(all_of(YRS), ~ replace(., is.nan(.), 0)))

###### Some basic calculations based on tool outputs ######
# PMT per person by mode per annum in each year in BAU
temp_pass_transpo %>%
  filter(var == "PMT") %>%
  group_by(mode, ) %>%
  summarise(across(YRS, sum))
