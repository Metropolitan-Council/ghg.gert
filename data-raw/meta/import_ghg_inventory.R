##### import ghg_inventory from ghg_cprg
library(dplyr)
library(readr)

ghg_ctu <- readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/205-ctu-ghg-compiler/_meta/data/ctu_emissions.RDS")

### need to resolve the below in ghg-cprg repo
ghg_ctu %>% filter(is.na(value_emissions)) %>% dplyr::distinct(geog_name,sector, category, source)

ghg_county <- readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/205-ctu-ghg-compiler/_meta/data/cprg_county_emissions.RDS")

### conform and bind

ghg_inventory <- bind_rows(ghg_ctu %>%
                             dplyr::mutate(unit_emissions = "Metric tons CO2e",
                                           geog_name = if_else(ctu_class == "TOWNSHIP",
                                                               paste(geog_name, "Twp."),
                                                               geog_name)) %>%
                             dplyr::rename(population = ctu_population,
                                           fips_id = ctu_id_fips) %>%
                             dplyr::select(-c(ctu_id_gnis)),
                           ghg_county %>%
                             dplyr::mutate(ctu_class = "COUNTY",
                                           category = as.character(category),
                                           geog_name = paste(county_name, "County")) %>%
                             dplyr::rename(
                               population = county_total_population,
                               fips_id = geoid) %>%
                             dplyr::select(-c(data_source, county_name, factor_source,population_data_source))
) %>% # patch category issue (fix in inventory repo later)
  mutate(category = dplyr::case_when(
    sector_alt == "Commercial" ~ "Commercial building fuel",
    sector_alt == "Industrial" & !grepl("processes", category) ~ "Industrial stationary combustion",
    TRUE ~ category),
    sector_alt = dplyr::if_else(sector_alt == "Commercial",
                                "Building fuel",
                                sector_alt)) %>%
  # collapse county commercial/industrial building fuel to business
  mutate(sector = if_else(sector %in% c("Commercial",
                                        "Industrial") &
                            sector_alt != "Industrial",
                          "Business",
                          sector)) %>%
  # reorder sectors
  mutate(sector = factor(sector,
                         levels = c("Transportation",
                                    "Residential",
                                    "Business",
                                    "Industrial",
                                    "Waste",
                                    "Agriculture",
                                    "Natural Systems"
                         )),
         sector_alt = factor(sector_alt,
                             levels = c("Transportation",
                                        "Electricity",
                                        "Building fuel",
                                        "Industrial",
                                        "Waste",
                                        "Agriculture",
                                        "Natural Systems"
                             ))
  ) %>%
  ### hot fix, needs permanent fix in cprg repo
  mutate(value_emissions = if_else(is.na(value_emissions),
                                   0,
                                   value_emissions)) %>%
  #remove refinery emissions
  filter(!(emissions_per_capita > 50 & sector == "Industrial"))

usethis::use_data(ghg_inventory, overwrite = TRUE)
