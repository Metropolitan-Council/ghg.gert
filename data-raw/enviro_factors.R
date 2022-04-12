library(tidyverse)

# values to be modified by user -----

enviro_factors <- list(
  # PLDV_TRANSIT_RATIO = 47 / 100, # 100 transit trips replace 47 LDV trips (Ewing and Hamidi, 2014)
  # NEW 100 transit trips replaces 110 PLDV trips
  PLDV_TRANSIT_RATIO = 1.43,
  SI_FUEL_COST_GAL = 239.8 / 100, # in dollars per gal https://www.eia.gov/dnav/pet/pet_pri_gnd_a_epm0_pte_dpgal_a.htm (about 8.8 cents per mile, so lower than Barnes estimate because mpg went up)
  CI_FUEL_COST_GAL = 264.0 / 100, # in dollars per gal https://www.eia.gov/dnav/pet/pet_pri_gnd_a_epm0_pte_dpgal_a.htm (about 44 cents per mile, so about equal to Barnes estimate)
  ELEC_FUEL_COST_KWH = 13 / 100, # in dollars per kWh https://www.xcelenergy.com/staticfiles/xe/PDF/Marketing/MN-SST-Interim-Rates.pdf
  F_FRACT = 0.27, # Fraction of truck TVMT inside MSP (i.e., under jurisdiction of application for VMT fee)
  AUTO_COST_MI = 61.88 / 100, # 2019 AAA https://exchange.aaa.com/automotive/driving-costs/#.YG7-L-hKiUk (assume mid-distance of 15,000 miles)
  TIME_COST_MI = 12.814 / 100, # dollars per mile according to https://www.vtpi.org/tca/tca0502.pdf and adjusted to 2015 using average CPI
  F_TIME_COST_MI = 119.0 / 100, # dollars per mile according to https://static.tti.tamu.edu/tti.tamu.edu/documents/TTI-2017-10.pdf
  INS_COST_MI = (100 * 808) / 8688, # https://www.forbes.com/advisor/car-insurance/state/minnesota/ and https://www.dot.state.mn.us/traffic/data/reports/vmt/92-17_per_capita_vmt.pdf
  CONG_VMT = 0.1087, # Congested VMT as a proportion of total VMT
  BUS_AV = -1.05, # Factors for % change in bus/rail for a 1% change in AV penetration
  RAIL_AV = -1.13,
  VMT_AV = 1.20,
  EVCS_VMT = 0.045, # Need to account for additional VMT due to charging for PHEV and BEV DRS
  MPG_AV = 0.85, # 15% reduction in consumption of fuel with AV based on Forecasting the Impact of Connected and Automated Vehicles on Energy
  MAX_5D_DR = -0.25,
  MAX_5D_ACT = 0.37,
  MAX_5D_TRANS = 0.71,
  MARG_TELEWORK = -2.749 / 100, # Telework marginal effect percent change in PMT (per household). From Kim et al. (2015)
  # TODO document these values in R/data.R
  KG_CO2E_PER_THERM_BASELINE = 5.31,
  KG_CO2E_PER_THERM_FORECAST = 5.31,
  KG_CO2E_PER_MHW_BASELINE = 566.4,
  KG_CO2E_PER_MHW_FORECAST = 566.4,
  LEED_GOLD_REDUCTION_PCT = 0.64,
  EXISTING_HOME_RETROFIT_REDUCTION_PCT = 0.33,
  EXISTING_HOME_ULTRA_RETROFIT_REDUCTION_PCT = 0.66,
  BEHAVIOR_CHANGE_REDUCTION_PCT = 0.11,
  SMART_GRID_EFFICIENCY_PCT = 0.11,
  THERM_TO_MWH = 0.0293
)


usethis::use_data(enviro_factors, overwrite = TRUE)






# Original values-----
# PLDV_TRANSIT_RATIO <- 47 / 100 # 100 transit trips replace 47 LDV trips (APTA, 2009)
# # FUEL_COST_MI = 13.13456 # cents per mile according to Barnes et al. (2003) for MN city and CPI for gasoline
#
# # F_FUEL_COST_MI = 43.01378 # cents per mile for freight according to Barnes et al. (2003) for MN city and CPI for gasoline
#
# SI_FUEL_COST_GAL <- 239.8 # in cents per gal https://www.eia.gov/dnav/pet/pet_pri_gnd_a_epm0_pte_dpgal_a.htm (about 8.8 cents per mile, so lower than Barnes estimate because mpg went up)
#
# CI_FUEL_COST_GAL <- 264.0 # in cents per gal https://www.eia.gov/dnav/pet/pet_pri_gnd_a_epm0_pte_dpgal_a.htm (about 44 cents per mile, so about equal to Barnes estimate)
#
# ELEC_FUEL_COST_KWH <- 13 # in cents per kWh https://www.xcelenergy.com/staticfiles/xe/PDF/Marketing/MN-SST-Interim-Rates.pdf
# F_FRACT <- 0.27 # Fraction of truck TVMT inside MSP (i.e., under jurisidiction of application for VMT fee)
#
# AUTO_COST_MI <- 61.88 # https://exchange.aaa.com/automotive/driving-costs/#.YG7-L-hKiUk (assume mid-distance of 15,000 miles)
#
# TIME_COST_MI <- 12.814 # cents per mile according to https://www.vtpi.org/tca/tca0502.pdf and adjusted to 2015 using average CPI
#
# F_TIME_COST_MI <- 119.0 # cents per mile according to https://static.tti.tamu.edu/tti.tamu.edu/documents/TTI-2017-10.pdf
# # Per mile insurance is total insurance divided by total VMT - gives 9.3 cents per mile in line with inflation from Cambridge Sys estimate of 6 cents and Litman (2019) estimate of 10 cents
#
# INS_COST_MI <- (100 * 808) / 8688 # https://www.forbes.com/advisor/car-insurance/state/minnesota/ and https://www.dot.state.mn.us/traffic/data/reports/vmt/92-17_per_capita_vmt.pdf
# # Congested VMT as a proportion of total VMT
#
# CONG_VMT <- 0.1087
#
# # Factors for % change in bus/rail for a 1% change in AV penetration
#
# BUS_AV <- -1.05
# RAIL_AV <- -1.13
# VMT_AV <- 1.20
# EVCS_VMT <- 0.045 # Need to account for additional VMT due to charging for PHEV and BEV DRS
#
# MPG_AV <- 0.85 # 15% reduction in consumption of fuel with AV based on Forecasting the Impact of Connected and Automated Vehicles on Energy
# # Use: A Microeconomic Study of Induced Travel and Energy Rebound Morteza Taiebata,b, Samuel Stolpera, Ming Xua,b
