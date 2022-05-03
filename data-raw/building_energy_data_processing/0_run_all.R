library(tidyr)
library(dplyr)
library(magrittr)
library(purrr)
library(ghg.sp)
library(councilR)

# db_tables <- ghg.sp::db_tables
#
# list2env(db_tables, envir = environment())

# demographic
source("data-raw/building_energy_data_processing/demographics_baseline.R")
source("data-raw/building_energy_data_processing/demographics_forecast.R")

# residential energy
source("data-raw/building_energy_data_processing/residential_baseline.R")
source("data-raw/building_energy_data_processing/residential_forecast.R")

# non-residential energy
source("data-raw/building_energy_data_processing/non-residential_baseline.R")
source("data-raw/building_energy_data_processing/non-residential_forecast.R")

# source("data-raw/building_energy_data_processing/emissions_baseline_forecast.R")

source("data-raw/building_energy_data_processing/ctu_residential_energy.R")
source("data-raw/building_energy_data_processing/ctu_non_residential_energy.R")
source("data-raw/building_energy_data_processing/ctu_characteristics.R")
