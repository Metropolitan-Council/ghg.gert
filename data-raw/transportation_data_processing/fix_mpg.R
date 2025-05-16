source("data-raw/transportation_data_processing/eia_datasets.R")

future_fuel_economy <- aeo_fuel_economy %>%
  filter(aeo_scen == "REF") %>%
  select(mode, year = period, var, value)


passenger_fuel_economy <- transportation_data$passenger %>%
  filter(mode == "PLDV",
         var %in% aeo_fuel_economy$var) %>%
  select(mode, var, year, value) %>%
  unique() %>%
  filter(year %in% c(2015, 2018, 2020)) %>%
  bind_rows(future_fuel_economy %>%
              filter(mode == "PLDV")) %>%
  arrange(var, year)

# TODO finish freight
# freight
transportation_data$freight %>%
  filter(var %in% aeo_fuel_economy$var) %>%
  select(mode, var, year, aeo_mode, value) %>%
  unique() %>%
  filter(year %in% c(2015, 2018, 2020)) %>%
  bind_rows(future_fuel_economy %>%
              filter(mode !="PLDV")) %>%
  arrange(var, mode, year)

# remove these variables from transportation_data
transportation_data$passenger <- transportation_data$passenger %>%
  filter(!var %in% fuel_economy$var)

transportation_data$freight <- transportation_data$freight %>%
  filter(!var %in% fuel_economy$var)

usethis::use_data(transportation_data, overwrite = TRUE)

