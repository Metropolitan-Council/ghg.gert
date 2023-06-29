# elasticities
library(ggplot2)
library(dplyr)
library(stringr)
library(councilR)
library(emo)
library(ghg.sp)


# Define years in model-----
# (can be any combination of 2015, 2018, 2020, 2025, 2030, 2035, 2040, 2045, and 2050)
YRS <- c("2015", "2018", "2020", "2025", "2030", "2035", "2040", "2045", "2050")
# Last forecast year
FIN_YR <- "2050"
# Year that dynamic ridesharing is introduced to the market (if included in scenario)
DRS_YR <- "2025"
# Forecast years
FOR_YRS <- c("2025", "2030", "2035", "2040", "2045", "2050")
# Years to adjust sales totals
ADJ_YRS <- c("2020", "2025", "2030", "2035")
INIT_YRS <- setdiff(YRS, FOR_YRS)
DAYS <- 340

empty_list <- c(rep(0, length(YRS)))

# LONG-RUN ELASTICITY - should be made available as a range (slider) for users----
# Harvey and Deakin (1998)
# INFRAS (2000) and Luk (1999) for range and Hymel and Small (2015) for mean
# ELAST_VMT <- readline(prompt="Pick an elasticity for VMT pricing (-0.1 to -0.8. Mean: -0.34): ")
# 0.34 number verified in Small and Van Dender (2007)
ELAST_VMT <- c(0, 0, 0, rep(-0.34, length(FOR_YRS)))
# ELAST_VMT <- calc_elasticity(empty_list, -0.2, length(INIT_YRS), length(FOR_YRS))

# Goodwin, Dargay, and Hanly (2003) for range and Small (2007) for mean
# Small (2007)
# ELAST_GAS <- readline(prompt="Pick an elasticity for gas tax (-0.05 to -0.17. Mean: -0.1066): ")

# ELAST_GAS <- calc_elasticity(empty_list, -0.1066, length(INIT_YRS), length(FOR_YRS))
ELAST_GAS <- c(0, 0, 0, rep(-0.1066, length(FOR_YRS)))
# Arentze, Hofman and Timmermans (2004) and PSRC 2005
# ELAST_CONG <- readline(prompt="Pick an elasticity for congestion (-0.04 to -0.16. Mean: -0.10): ")

ELAST_CONG <- c(0, 0, 0, rep(-0.10, length(FOR_YRS)))
# ELAST_CONG <- calc_elasticity(empty_list, -0.10, length(INIT_YRS), length(FOR_YRS))

# TRACE (1999) and Litman (2019)
# ELAST_PARK <- readline(prompt="Pick an elasticity for parking cost (-0.03 to -0.17. Mean: -0.07): ")
ELAST_PARK <- c(0, 0, 0, rep(-0.07, length(FOR_YRS)))
# ELAST_PARK <- calc_elasticity(empty_list, -0.03, length(INIT_YRS), length(FOR_YRS))


# CROSS_VMT <- calc_elasticity(empty_list, 0.13, length(INIT_YRS), length(FOR_YRS))
CROSS_VMT <- c(0, 0, 0, rep(0.13, length(FOR_YRS))) # for transit/walk/bike wrt PLDV price (VMT) (Litman 2019. https://www.vtpi.org/elasticities.pdf)

# TRACE (1999)
CROSS_PARK_TRANSIT <- c(0, 0, 0, rep(0.01, length(FOR_YRS)))
# CROSS_PARK_TRANSIT <- calc_elasticity(empty_list, 0.01, length(INIT_YRS), length(FOR_YRS))

# TRACE (1999)
CROSS_PARK_ACTIVE <- c(0, 0, 0, rep(0.03, length(FOR_YRS)))
# CROSS_PARK_ACTIVE <- calc_elasticity(empty_list, 0.03, length(INIT_YRS), length(FOR_YRS))

# # GHG wrt AVO according to Naumov et al. (2020)
# ELAST_PLDV_AVO = -0.675
# ELAST_TRANSIT_AVO = -0.055

# ELAST_FVMT <- calc_elasticity(empty_list, -0.25, length(INIT_YRS), length(FOR_YRS))
ELAST_FVMT <- c(0, 0, 0, rep(-0.25, length(FOR_YRS))) # Small and Winston (1999) quoted in (Litman 2011)


# Elasticities for changes in vehicle ownership in response to price changes
ELAST_OWN_PRICE <- c(0, 0, 0, rep(-0.10, length(FOR_YRS)))
# ELAST_OWN_PRICE <- calc_elasticity(empty_list, -0.10, length(INIT_YRS), length(FOR_YRS))



elast <- tibble(
  year = YRS,
  vmt_elast = ELAST_VMT,
  gas_elast = ELAST_GAS,
  cong_elast = ELAST_CONG,
  park_elast = ELAST_PARK,
  freight_vmt_elast = ELAST_FVMT,
  vehicle_ownership_elast = ELAST_OWN_PRICE,
  vmt_cross = CROSS_VMT,
  park_active = CROSS_PARK_ACTIVE,
  park_transit = CROSS_PARK_TRANSIT
)


# Driving VMT elasticity to 5Ds -----
#
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
# alls function that interpolates changes through forecast years for elasticity.
#  Assumes change is linear to final forecast year.

# Define a default starting list for elasticities for 5Ds
ELAST_DEF_5D <- c(rep(0, length(YRS)))
# Driving elasticity
# All taken from Ewing and Cervero, 2010
# Density population (RANGE)
ELAST_DENS_DR_POP <- calc_elasticity(ELAST_DEF_5D, -0.04, length(INIT_YRS), length(FOR_YRS))

# Density employment (RANGE)
# -0.01 to -0.07 range, taken from Stevens, 2016
# -0.07 is more than the population density decrease, to ensure that
# population density has a lesser effect than job density
# based on peer review session with Metro Transit SI folks
# https://github.com/Metropolitan-Council/ghg.sp/issues/19
ELAST_DENS_DR_EMP <- calc_elasticity(ELAST_DEF_5D, -0.07, length(INIT_YRS), length(FOR_YRS))
# Diversity (RANGE)
ELAST_DIVER_DR <- calc_elasticity(ELAST_DEF_5D, -0.09, length(INIT_YRS), length(FOR_YRS))
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




# combined tables -----


# 5D elasticities -----
transit_5d <- tibble(
  year = YRS,
  type = "TRANSIT",
  population_density = ELAST_DENS_TRANS_POP,
  employment_density = ELAST_DENS_TRANS_EMP,
  diversity = ELAST_DIVER_TRANS,
  design = ELAST_DES_TRANS,
  job_access = ELAST_JOBS_TRANS,
  distance = ELAST_DIST_TRANS,
  combined_density = ELAST_CDENS_TRANS
)

walk_5d <- tibble(
  year = YRS,
  type = "WALK",
  population_density = ELAST_DENS_ACT_POP,
  employment_density = ELAST_DENS_ACT_EMP,
  diversity = ELAST_DIVER_ACT,
  design = ELAST_DES_ACT,
  job_access = ELAST_JOBS_ACT,
  distance = ELAST_DIST_ACT,
  combined_density = ELAST_CDENS_ACT
)

drive_5d <- tibble(
  year = YRS,
  type = "DRIVE",
  population_density = ELAST_DENS_DR_POP,
  employment_density = ELAST_DENS_DR_EMP,
  diversity = ELAST_DIVER_DR,
  design = ELAST_DES_DR,
  job_access = ELAST_JOBS_DR,
  distance = ELAST_DIST_DR,
  combined_density = ELAST_CDENS_DR
)

elast_5d <- bind_rows(
  drive_5d,
  walk_5d,
  transit_5d
) %>%
  bind_rows(
    tibble::tribble(
      ~year, ~type, ~population_density, ~employment_density, ~diversity, ~design, ~job_access, ~distance, ~combined_density,
      "2045", "DRIVE", -0.04, -0.07, -0.09, -0.12, -0.2, -0.05, -0.22,
      "2045", "WALK", 0.07, 0.04, 0.15, -0.06, -0.06, 0.15, 0.33,
      "2045", "TRANSIT", 0.07, 0.01, 0.12, 0.29, 0.128, 0.29, 0.62,
      "2050", "DRIVE", -0.04, -0.07, -0.09, -0.12, -0.2, -0.05, -0.22,
      "2050", "WALK", 0.07, 0.04, 0.15, -0.06, -0.06, 0.15, 0.33,
      "2050", "TRANSIT", 0.07, 0.01, 0.12, 0.29, 0.128, 0.29, 0.62
    )
  )


# save all -----

# waldo::compare(elast_5d, ghg.sp::elast_5d)
usethis::use_data(elast_5d, overwrite = TRUE)


# waldo::compare(elast, ghg.sp::elast)
usethis::use_data(elast, overwrite = TRUE)
