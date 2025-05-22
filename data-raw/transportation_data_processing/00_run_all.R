## code to prepare `00_run_all` dataset goes here

source("data-raw/transportation_data_processing/transportation_data.R")

source("data-raw/transportation_data_processing/fix_bus_fleet.R")
source("data-raw/transportation_data_processing/fix_bus_pmt.R")
source("data-raw/transportation_data_processing/fix_bus_avo.R")
# source("data-raw/transportation_data_processing/fix_bus_fuel.R")
source("data-raw/transportation_data_processing/fix_pmt_tmt.R")
source("data-raw/transportation_data_processing/fix_parking.R")
source("data-raw/transportation_data_processing/fix_fuel_economy.R")
source("data-raw/transportation_data_processing/transportation_index.R")

# building energy
# source("data-raw/building_energy_data_processing/naics_codes.R")

source("data-raw/enviro_factors.R")
source("data-raw/transportation_data_processing/factor_values.R")

write_csv(transportation_data$passenger, "data-raw/transportation_data_processing/csv_copies/transportation_data_passenger.csv")

write_csv(transportation_data$freight, "data-raw/transportation_data_processing/csv_copies/transportation_data_freight.csv")
