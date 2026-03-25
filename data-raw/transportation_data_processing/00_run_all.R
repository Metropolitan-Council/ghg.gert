## code to prepare `00_run_all` dataset goes here
options(readr.show_col_types = FALSE)
# source("data-raw/transportation_data_processing/transportation_data.R")

# source("data-raw/transportation_data_processing/fix_bus_fleet.R")
# source("data-raw/transportation_data_processing/fix_bus_pmt.R")
# source("data-raw/transportation_data_processing/fix_bus_avo.R")
# # source("data-raw/transportation_data_processing/fix_bus_fuel.R")
# source("data-raw/transportation_data_processing/fix_pmt_tmt.R")
# source("data-raw/transportation_data_processing/fix_fuel_economy.R")
# source("data-raw/transportation_data_processing/transportation_index.R")

# # building energy
# # source("data-raw/building_energy_data_processing/naics_codes.R")

source("data-raw/enviro_factors.R")
source("data-raw/transportation_data_processing/factor_values.R")
# # source("data-raw/fix_names.R")

# transportation_data$passenger <- transportation_data$passenger %>%
#   left_join(geog_index) %>%
#   select(mode, var, geog_name, geog_id, year, value, aeo_mode, type)


# transportation_data$freight <- transportation_data$freight %>%
#   left_join(geog_index) %>%
#   select(mode, var, geog_name, geog_id, year, value, aeo_mode, type)

# saveRDS(transportation_data, "data-raw/transportation_data_processing/clean_part1.rds")

transportation_data <- readRDS("data-raw/transportation_data_processing/clean_part1.rds")
# these need to be run AFTER we fix city names
source("data-raw/transportation_data_processing/fix_pldv_avo.R")
source("data-raw/transportation_data_processing/fix_parking.R")
source("data-raw/transportation_data_processing/fix_update_pmt.R")
source("data-raw/transportation_data_processing/fix_remove_av.R")
source("data-raw/transportation_data_processing/fix_avo_all.R")
source("data-raw/transportation_data_processing/fix_remove_phev.R")

source("data-raw/transportation_data_processing/fix_region_aggregate.R")
source("data-raw/transportation_data_processing/fix_county_aggregate.R")

source("data-raw/transportation_data_processing/transportation_defaults.R")

readr::write_csv(transportation_data$passenger, "data-raw/transportation_data_processing/csv_copies/transportation_data_passenger.csv")

readr::write_csv(transportation_data$freight, "data-raw/transportation_data_processing/csv_copies/transportation_data_freight.csv")
