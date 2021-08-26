
# Define years in model-----
# (can be any combination of 2015, 2018, 2020, 2025, 2030, 2035, 2040, 2045, and 2050)
YRS <- c("2015", "2018", "2020", "2025", "2030", "2035", "2040")
# Last forecast year
FIN_YR <- "2040"
# Year that dynamic ridesharing is introduced to the market (if included in scenario)
DRS_YR <- "2025"
# Forecast years
FOR_YRS <- c("2025", "2030", "2035", "2040")
# Years to adjust sales totals
ADJ_YRS <- c("2020", "2025", "2030", "2035")
INIT_YRS <- setdiff(YRS, FOR_YRS)
DAYS <- 340




# Global constants -------
PLDV_TRANSIT_RATIO <- 47 / 100 # 100 transit trips replace 47 LDV trips (APTA, 2009)
# FUEL_COST_MI = 13.13456 # cents per mile according to Barnes et al. (2003) for MN city and CPI for gasoline
# F_FUEL_COST_MI = 43.01378 # cents per mile for freight according to Barnes et al. (2003) for MN city and CPI for gasoline
SI_FUEL_COST_GAL <- 239.8 # in cents per gal https://www.eia.gov/dnav/pet/pet_pri_gnd_a_epm0_pte_dpgal_a.htm (about 8.8 cents per mile, so lower than Barnes estimate because mpg went up)
CI_FUEL_COST_GAL <- 264.0 # in cents per gal https://www.eia.gov/dnav/pet/pet_pri_gnd_a_epm0_pte_dpgal_a.htm (about 44 cents per mile, so about equal to Barnes estimate)
ELEC_FUEL_COST_KWH <- 13 # in cents per kWh https://www.xcelenergy.com/staticfiles/xe/PDF/Marketing/MN-SST-Interim-Rates.pdf
F_FRACT <- 0.27 # Fraction of truck TVMT inside MSP (i.e., under jurisidiction of application for VMT fee)
AUTO_COST_MI <- 61.88 # https://exchange.aaa.com/automotive/driving-costs/#.YG7-L-hKiUk (assume mid-distance of 15,000 miles)
TIME_COST_MI <- 12.814 # cents per mile according to https://www.vtpi.org/tca/tca0502.pdf and adjusted to 2015 using average CPI
F_TIME_COST_MI <- 119.0 # cents per mile according to https://static.tti.tamu.edu/tti.tamu.edu/documents/TTI-2017-10.pdf
# Per mile insurance is total insurance divided by total VMT - gives 9.3 cents per mile in line with inflation from Cambridge Sys estimate of 6 cents and Litman (2019) estimate of 10 cents
INS_COST_MI <- (100 * 808) / 8688 # https://www.forbes.com/advisor/car-insurance/state/minnesota/ and https://www.dot.state.mn.us/traffic/data/reports/vmt/92-17_per_capita_vmt.pdf
# Congested VMT as a proportion of total VMT
CONG_VMT <- 0.1087
# Factors for % change in bus/rail for a 1% change in AV penetration
BUS_AV <- -1.05
RAIL_AV <- -1.13
VMT_AV <- 1.20
EVCS_VMT <- 0.045 # Need to account for additional VMT due to charging for PHEV and BEV DRS
MPG_AV <- 0.85 # 15% reduction in consumption of fuel with AV based on Forecasting the Impact of Connected and Automated Vehicles on Energy
# Use: A Microeconomic Study of Induced Travel and Energy Rebound Morteza Taiebata,b, Samuel Stolpera, Ming Xua,b



# LONG-RUN ELASTICITY - should be made available as a range (slider) for users----
# Harvey and Deakin (1998)
# INFRAS (2000) and Luk (1999) for range and Hymel and Small (2015) for mean
# ELAST_VMT <- readline(prompt="Pick an elasiticty for VMT ricing (-0.1 to -0.8. Mean: -0.34): ")
ELAST_VMT <- c(0, 0, 0, rep(-0.20, length(FOR_YRS)))
# Goodwin, Dargay, and Hanly (2003) for range and Small (2007) for mean
# Small (2007)
# ELAST_GAS <- readline(prompt="Pick an elasiticty for gas tax (-0.05 to -0.17. Mean: -0.1066): ")
ELAST_GAS <- c(0, 0, 0, rep(-0.1066, length(FOR_YRS)))
# Arentze, Hofman and Timmermans (2004) and PSRC 2005
# ELAST_CONG <- readline(prompt="Pick an elasiticty for congestion (-0.04 to -0.16. Mean: -0.10): ")
ELAST_CONG <- c(0, 0, 0, rep(-0.10, length(FOR_YRS)))
# TRACE (1999) and Litman (2019)
# ELAST_PARK <- readline(prompt="Pick an elasiticty for parking cost (-0.03 to -0.17. Mean: -0.07): ")
ELAST_PARK <- c(0, 0, 0, rep(-0.07, length(FOR_YRS)))
CROSS_VMT <- c(0, 0, 0, rep(0.13, length(FOR_YRS))) # for transit/walk/bike wrt PLDV price (VMT) (Litman 2019. https://www.vtpi.org/elasticities.pdf)
# TRACE (1999)
CROSS_PARK_TRANSIT <- c(0, 0, 0, rep(0.01, length(FOR_YRS)))
# TRACE (1999)
CROSS_PARK_ACTIVE <- c(0, 0, 0, rep(0.03, length(FOR_YRS)))
# # GHG wrt AVO according to Naumov et al. (2020)
# ELAST_PLDV_AVO = -0.675
# ELAST_TRANSIT_AVO = -0.055
ELAST_FVMT <- c(0, 0, 0, rep(-0.25, length(FOR_YRS))) # Small and Winston (1999) quoted in (Litman 2011)
# Driving VMT elasticity to 5Ds - calls function that interpolates changes through forecast years for elasticity. Assumes change is linear to final forecast year.
# Define a default starting list for elasticities for 5Ds
ELAST_DEF_5D <- c(rep(0, length(YRS)))
# Density population (RANGE)
ELAST_DENS_DR_POP <- calc_elasticity(ELAST_DEF_5D, -0.04, length(INIT_YRS), length(FOR_YRS))
# Density employment (RANGE)
ELAST_DENS_DR_EMP <- c(rep(0, length(YRS)))
# Diversity (RANGE)
ELAST_DIVER_DR <- calc_elasticity(ELAST_DEF_5D, -0.090, length(INIT_YRS), length(FOR_YRS))
# Design (RANGE)
ELAST_DES_DR <- calc_elasticity(ELAST_DEF_5D, -0.12, length(INIT_YRS), length(FOR_YRS))
# Jobs Access (RANGE)
ELAST_JOBS_DR <- calc_elasticity(ELAST_DEF_5D, -0.200, length(INIT_YRS), length(FOR_YRS))
# Distance (RANGE)
ELAST_DIST_DR <- calc_elasticity(ELAST_DEF_5D, -0.050, length(INIT_YRS), length(FOR_YRS))
# Combined density effect
ELAST_CDENS_DR <- calc_elasticity(ELAST_DEF_5D, -0.22, length(INIT_YRS), length(FOR_YRS))
# Walking VMT elasticity to 5Ds
# Density population (RANGE)
ELAST_DENS_ACT_POP <- calc_elasticity(ELAST_DEF_5D, 0.070, length(INIT_YRS), length(FOR_YRS))
# Density employment (RANGE)
ELAST_DENS_ACT_EMP <- calc_elasticity(ELAST_DEF_5D, 0.040, length(INIT_YRS), length(FOR_YRS))
# Diversity (RANGE)
ELAST_DIVER_ACT <- calc_elasticity(ELAST_DEF_5D, 0.150, length(INIT_YRS), length(FOR_YRS))
# Design (RANGE)
ELAST_DES_ACT <- calc_elasticity(ELAST_DEF_5D, -0.060, length(INIT_YRS), length(FOR_YRS))
# Job Access (RANGE)
ELAST_JOBS_ACT <- calc_elasticity(ELAST_DEF_5D, -0.060, length(INIT_YRS), length(FOR_YRS))
# Distance (RANGE)
ELAST_DIST_ACT <- calc_elasticity(ELAST_DEF_5D, 0.150, length(INIT_YRS), length(FOR_YRS))
# Combined density effect
ELAST_CDENS_ACT <- calc_elasticity(ELAST_DEF_5D, 0.330, length(INIT_YRS), length(FOR_YRS))
# Transit VMT elasticity to 5Ds
# Density population (RANGE)
ELAST_DENS_TRANS_POP <- calc_elasticity(ELAST_DEF_5D, 0.07, length(INIT_YRS), length(FOR_YRS))
# Density employment (RANGE)
ELAST_DENS_TRANS_EMP <- calc_elasticity(ELAST_DEF_5D, 0.01, length(INIT_YRS), length(FOR_YRS))
# Diversity (RANGE)
ELAST_DIVER_TRANS <- calc_elasticity(ELAST_DEF_5D, 0.12, length(INIT_YRS), length(FOR_YRS))
# Job Access (RANGE)
ELAST_DES_TRANS <- calc_elasticity(ELAST_DEF_5D, 0.290, length(INIT_YRS), length(FOR_YRS))
# Design (RANGE)
ELAST_JOBS_TRANS <- calc_elasticity(ELAST_DEF_5D, 0.128, length(INIT_YRS), length(FOR_YRS))
# Distance (RANGE)
ELAST_DIST_TRANS <- calc_elasticity(ELAST_DEF_5D, 0.290, length(INIT_YRS), length(FOR_YRS))
# Combined density effect
ELAST_CDENS_TRANS <- calc_elasticity(ELAST_DEF_5D, 0.620, length(INIT_YRS), length(FOR_YRS))
# Max 5D by mode
MAX_5D_DR <- -0.25
MAX_5D_ACT <- 0.37
MAX_5D_TRANS <- 0.71

# Telework marginal effect percent change in PMT (per household)
MARG_TELEWORK <- -2.749 # From Kim et al. (2015)
# Elasticities for changes in vehicle ownership in response to price changes
ELAST_OWN_PRICE <- c(0, 0, 0, rep(-0.10, length(FOR_YRS)))

## Prompt user for input of electricity assumption-----
elec_scen <- readline_check(prompt = "Enter electricity scenario ('ER' or 'EM'): ", type = "elec_scen")
## Prompt user for input of energy market assumption
aeo_scen <- readline_check(prompt = "Enter AEO scenario ('REF', 'HM', 'HOGS', 'LM', 'HP', 'LP', 'LOGS'): ", type = "aeo_scen")
# ch_ctu <- readline_check(prompt="Select a ctu: ", type="ctu")
#### USER SHOULD SET IN UI #####
ch_ctu <- "St. Paul"

pass_transpo <- pass_transpo %>% filter(ctu == ch_ctu | ctu == "All")
freight_transpo <- freight_transpo %>% filter(ctu == ch_ctu | ctu == "All")




