# Update fuel_economy table with latest EIA and BTS data
# This script should be run as part of 00_run_all.R sequence

# Source latest EIA and BTS datasets -----
source("data-raw/transportation_data_processing/eia_datasets.R")
source("data-raw/transportation_data_processing/bts_fuel_economy.R")

# Get mode mapping from existing fuel_economy -----
mode_mapping <- fuel_economy %>%
    select(mode, aeo_mode) %>%
    unique() %>%
    filter(!is.na(mode), !is.na(aeo_mode))

# Prepare EIA updates -----
# Get latest AEO fuel economy projections (REF scenario)
eia_updates <- aeo_fuel_economy %>%
    filter(aeo_scen == "REF" | aeo_scen == "ref2025") %>%
    mutate(
        metadata = paste0(
            "EIA Annual Energy Outlook, ",
            aeo_year,
            " ", name, " Scenario. ",
            seriesName,
            " (", seriesId, ")"
        ),
        # Convert MPG equivalent to miles per kWh for BEVElec only
        value = ifelse(var == "BEVElec", value / 33.7, value)
    ) %>%
    select(aeo_mode, year = period, var, value, metadata) %>%
    # Map to mode
    left_join(mode_mapping, by = "aeo_mode") %>%
    filter(!is.na(mode)) %>%
    mutate(year = as.character(year)) %>%
    select(year, mode, aeo_mode, var, value, metadata)

# Prepare BTS updates -----
# BTS data for historical years (2015, 2018, 2020)
bts_updates <- bts_fuel_economy %>%
    mutate(year = as.character(year)) %>%
    select(year, mode, aeo_mode, var, value, metadata)

# Combine all updates -----
all_updates <- bind_rows(
    eia_updates,
    bts_updates
)

# Update fuel_economy table -----
# Strategy: Remove old records for same year/mode/var combinations, then add updates
fuel_economy <- fuel_economy %>%
    mutate(year = as.character(year)) %>%
    anti_join(
        all_updates %>% select(year, mode, aeo_mode, var),
        by = c("year", "mode", "aeo_mode", "var")
    ) %>%
    # Add new/updated records
    bind_rows(all_updates) %>%
    arrange(mode, var, as.numeric(year))

# Export updated fuel_economy -----
usethis::use_data(fuel_economy, overwrite = TRUE)
