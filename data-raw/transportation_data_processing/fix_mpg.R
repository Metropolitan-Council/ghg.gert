source("data-raw/transportation_data_processing/eia_datasets.R")

mode_aeo_mode_index <- transportation_data$passenger %>%
  select(mode, aeo_mode) %>%
  unique()

future_fuel_economy <- aeo_fuel_economy %>%
  filter(aeo_scen == "REF") %>%
  mutate(metadata = paste0(
    "EIA Annual Energy Outlook, ",
    aeo_year,
    " ", name, " Scenario. ",
    seriesName,
    " (", seriesId, ")"
  )) %>%
  select(aeo_mode, year = period, var, value, metadata) %>%
  # convert MPG equivalent to miles per kWh for BEVElec only
  mutate(value = ifelse(var == "BEVElec", value / 33.7, value))

pldv_fuel_economy <- transportation_data$passenger %>%
  filter(mode == "PLDV",
         var %in% c("HEVMPG",
                    "SIMPG",
                    "CIMPG",
                    "BEVElec",
                    "PHEVMPG")) %>%
  select(mode, aeo_mode, var, year, value) %>%
  unique() %>%
  filter(year %in% c(2015, 2018, 2020)) %>%
  bind_rows(future_fuel_economy %>%
              filter(aeo_mode == "LDV") %>%
              mutate(mode = "PLDV")) %>%
  arrange(var, year) %>%
  mutate(metadata = ifelse(is.na(metadata), "EIA Annual Energy Outlook, 2021", metadata))


# no updates to PHEVElec value
phev_elec <- transportation_data$passenger %>%
  filter(mode == "PLDV",
         var == "PHEVElec") %>%
  select(mode, aeo_mode, var, year, value) %>%
  unique()

passenger_fuel_economy <-
  transportation_data$passenger %>%
  # get all non-PLDV
  filter(
    mode != "PLDV",
         var %in% c("HEVMPG",
                    "SIMPG",
                    "CIMPG",
                    "BEVElec",
                    "PHEVMPG",
                    "PHEVElec",
                    "BCIMPG",
                    "EVElec"
                    )) %>%
  select(year, mode, aeo_mode, var, value) %>%
  unique() %>%
  bind_rows(phev_elec) %>%
  bind_rows(pldv_fuel_economy)





# freight -----
# no changes to freight fuel economies
freight_fuel_economy <-
  transportation_data$freight %>%
  filter(
    var %in% c("HEVMPG",
               "SIMPG",
               "CIMPG",
               "BEVElec",
               "PHEVElec",
               "PHEVMPG",
               "BCIMPG",
               "EVElec")) %>%
  select(year, mode, aeo_mode, var, value) %>%
  unique()

# combine and export -----

fuel_economy <-
  bind_rows(passenger_fuel_economy,
            freight_fuel_economy)


usethis::use_data(fuel_economy, overwrite = TRUE)


# remove these variables from transportation_data
transportation_data$passenger <- transportation_data$passenger %>%
  filter(!var %in% fuel_economy$var)

transportation_data$freight <- transportation_data$freight %>%
  filter(!var %in% fuel_economy$var)

usethis::use_data(transportation_data, overwrite = TRUE)

