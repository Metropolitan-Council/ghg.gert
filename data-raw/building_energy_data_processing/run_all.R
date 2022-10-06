library(tidyr)
library(dplyr)
library(magrittr)
library(purrr)
library(ghg.sp)
library(councilR)

# import tables
source("data-raw/building_energy_data_processing/naics_codes.R")
source("data-raw/building_energy_data_processing/00_import_tables.R")

# demographic baseline
source("data-raw/building_energy_data_processing/01a_demographics_baseline_county.R")
source("data-raw/building_energy_data_processing/01b_demographics_baseline_ctu.R")

# demographic forecast
source("data-raw/building_energy_data_processing/02a_demographics_forecast_county.R")
source("data-raw/building_energy_data_processing/02b_demographics_forecast_ctu.R")

# residential energy
source("data-raw/building_energy_data_processing/03_residential_baseline.R")
source("data-raw/building_energy_data_processing/04_residential_forecast.R")

# non-residential energy
source("data-raw/building_energy_data_processing/05a_non-residential_baseline_state.R")
source("data-raw/building_energy_data_processing/05b_non-residential_baseline_county.R")
source("data-raw/building_energy_data_processing/05c_non-residential_baseline_ctu.R")

source("data-raw/building_energy_data_processing/06_non-residential_forecast.R")

source("data-raw/building_energy_data_processing/07_ctu_residential_energy.R")
source("data-raw/building_energy_data_processing/08_ctu_non_residential_energy.R")
source("data-raw/building_energy_data_processing/09_ctu_characteristics.R")


building_data <-
  list(
    "residential" = residential,
    "non_residential" = non_residential
  )

usethis::use_data(building_data, overwrite = T)
