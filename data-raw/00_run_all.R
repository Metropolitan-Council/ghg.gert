## code to prepare `00_run_all` dataset goes here

source("data-raw/transportation_data.R")

source("data-raw/fix_bus_fleet.R")
source("data-raw/fix_bus_pmt.R")
# source("data-raw/fix_bus_fuel.R")
source("data-raw/transportation_index.R")

# building energy
source("data-raw/naics_codes.R")


write_csv(transportation_data$passenger, "data-raw/csv_copies/transportation_data_passenger.csv")

write_csv(transportation_data$freight, "data-raw/csv_copies/transportation_data_freight.csv")
