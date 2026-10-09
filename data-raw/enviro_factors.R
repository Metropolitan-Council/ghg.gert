library(tidyverse)

# values to be modified by user -----

enviro_factors <- list(
  # Transit service elasticity
  # Effect of increase in transit service on increase in transit ridership and
  # decrease in PLDV.
  # TODO citation from Metro Transit SI folks
  TRANSIT_SERVICE_ELAST = 0.9,
  # minimum percent of transit service increase that vehicle occupancy
  # must increase by.
  # If .transit_service_pct = 0.2,
  # .transit_avo_pct must be at minimum 0.5 * 0.2 = 0.1
  TRANSIT_SERVICE_AVO_MIN = 0.5,
  # see transportation_data_processing/eia_datasets.R
  SI_FUEL_COST_GAL = 3.163,
  CI_FUEL_COST_GAL = 3.696,
  ELEC_FUEL_COST_KWH = 0.13069, # in dollars per kWh xcelenergyMNResidentialRates2024 https://www.xcelenergy.com/staticfiles/xe-responsive/Company/Rates%20&%20Regulations/24-01-406-MN-Res-ElecRates-MN-Res-E-2002.pdf
  F_FRACT = 0.27, # Fraction of truck TVMT inside MSP (i.e., under jurisdiction of application for VMT fee)
  # Maximum parking pricing policy impact: -30% reduction in VMT @capcoaGHGHandbook2024
  MAX_PARKING_REDUCTION_PCT = -0.3,
  AUTO_COST_MI = 0.72, # 2024 AAA driving cost estimate https://exchange.aaa.com/automotive/aaas-your-driving-costs/ in dollars per mile
  # time cost per mile
  TIME_COST_MI = 0.128, # dollars per mile according to https://www.vtpi.org/tca/tca0502.pdf
  F_TIME_COST_MI = 1.190, # dollars per mile according to https://static.tti.tamu.edu/tti.tamu.edu/documents/TTI-2017-10.pdf, Table 3
  # Average annual minimum coverage insurance ($621 via NerdWallet, 2025) divided by annual VMT per person (daily VMT per person (metro, MnDOT 2023) multiplied by annualization factor of 340)
  INS_COST_MI = (621) / (22.9 * 340), # https://www.nerdwallet.com/insurance/auto/cheap-car-insurance-minnesota and https://metropolitan-council.github.io/tspe-quarto/05-02_reduce_emissions.html#sec-regional-vmt
  CONG_VMT = 0.1087, # Congested VMT as a proportion of total VMT
  BUS_AV = -1.05, # Factors for % change in bus/rail for a 1% change in AV penetration
  RAIL_AV = -1.13,
  VMT_AV = 1.20,
  EVCS_VMT = 0.045, # Need to account for additional VMT due to charging for PHEV and BEV DRS
  MPG_AV = 0.85, # 15% reduction in consumption of fuel with AV based on Forecasting the Impact of Connected and Automated Vehicles on Energy
  MAX_5D_DR = -0.30, # maximum for drive comes from CAPCOA handbook capcoaGHGHandbook2024
  MAX_5D_ACT = 0.37,
  MAX_5D_TRANS = 0.71,
  MARG_TELEWORK = -2.749 / 100, # Telework marginal effect percent change in PMT (per household). From Kim et al. (2015)

  CBTP_PARTICIPATION_PCT = 0.19, # Community Based Travel Planning: proportion of targeted residences that participate
  CBTP_TRIP_REDUCTION_PCT = -0.12, # Community Based Travel Planning: vehicle trip reduction by participating residences
  MAX_TRIP_REDUCTION_PCT = -0.023, # Trip reduction program: maximum VMT reduction cap (2.3%). Citation: CAPCOA Handbook

  COMMUTE_TRIP_REDUCTION_VOLUNTARY_PCT = -0.04, # Commute Trip Reduction Program (Voluntary)
  COMMUTE_TRIP_REDUCTION_MANDATORY_PCT = -0.26, # Commute Trip Reduction Program (Mandatory and monitoring)
  MAX_COMMUTE_TRIP_REDUCTION_PCT = -0.45, # Maximum Commute Trip Reduction Program
  # TODO document these values in R/data.R


  KG_CO2E_PER_THERM_BASELINE = 5.31,
  KG_CO2E_PER_THERM_FORECAST = 5.31,
  KG_CO2E_PER_MHW_BASELINE = 566.4,
  KG_CO2E_PER_MHW_FORECAST = 566.4,
  MT_CO2E_PER_MCF_NATGAS = 0.054440,
  LEED_GOLD_REDUCTION_PCT = 0.64,
  EXISTING_HOME_RETROFIT_REDUCTION_PCT = 0.33,
  EXISTING_HOME_ULTRA_RETROFIT_REDUCTION_PCT = 0.66,
  BEHAVIOR_CHANGE_REDUCTION_PCT = 0.11,
  SMART_GRID_EFFICIENCY_PCT = 0.11,
  THERM_TO_MWH = 0.0293,
  BOILER_TO_HEAT_PUMP_EFFICIENCY_RATIO = 1.59362,
  # Land Use
  AGRI_LAND_CARBON_STOCK = 3, # Mega grams CO2e per hectare https://www.pnas.org/doi/10.1073/pnas.1512542112
  MAX_SOC_ACCUMULATION_UNDER_REDUCED_OR_NO_TILL_AGRI_PCT = 1.54, # https://www.nature.com/articles/s41598-019-47861-7
  W2W_DIESEL_EMISSIONS_FACTOR = 12.50, # in Kilograms of CO2e per gallon https://pubs.acs.org/doi/pdf/10.1021/es9024194
  AVOIDED_EMISSIONS_TRACTOR_USE = 0.1016, # in Mega grams CO2e per hectare https://www.usda.gov/media/blog/2017/11/30/saving-money-time-and-soil-economics-no-till-farming
  URBAN_FORM_SCENARIO = "bau",
  NON_RES_NATURAL_GAS_FOR_WATER_HEATING_PCT = 0.2,
  NON_RES_NATURAL_GAS_FOR_SPACE_HEATING_PCT = 0.69,
  # The percentage of commercial buildings that would be on the smart grid
  COMMERCIAL_SMART_GRID_PCT = 1,
  # The percentage of industrial buildings that would be on the smart grid
  INDUSTRIAL_SMART_GRID_PCT = 1,
  # The percentage of natural gas that is commonly used for space heating in residential buildings.
  RES_NATUAL_GAS_FOR_SPACE_HEATING_PCT = 0.71,
  # The percentage of natural gas that is commonly used for water heating in residential buildings.

  RES_NATURAL_GAS_FOR_WATER_HEATING_PCT = 0.24,
  GRID_DECARBONIZATION_BASELINE = 0, # 2018 baseline decarbonization percent should be taken as 0 for proper calculation of reduction in emissions
  SMART_GRID_IMPACT = 0.11
)


waldo::compare(ghg.gert::enviro_factors, enviro_factors)

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
