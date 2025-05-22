##### import ghg_inventory from ghg_cprg

ghg_ctu <- readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/205-ctu-ghg-compiler/_meta/data/ctu_emissions.RDS")

ghg_county <- readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/205-ctu-ghg-compiler/_meta/data/cprg_county_emissions.RDS")

### conform and bind

ghg_inventory <- bind_rows(ghg_ctu %>%
                       dplyr::mutate(unit_emissions = "Metric tons CO2e") %>%
                       dplyr::rename(population = ctu_population,
                                     fips_id = ctu_id_fips) %>%
                       dplyr::select(-c(ctu_id_gnis)),
                     ghg_county %>%
                       dplyr::mutate(ctu_class = "COUNTY",
                                     category = as.character(category)) %>%
                       dplyr::rename(geog_name = county_name,
                                     population = county_total_population,
                                     fips_id = geoid) %>%
                       dplyr::select(-c(data_source, factor_source,population_data_source))
)

usethis::use_data(ghg_inventory, overwrite = TRUE)
