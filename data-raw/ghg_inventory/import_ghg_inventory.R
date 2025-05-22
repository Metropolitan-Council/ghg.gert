##### import ghg_inventory from ghg_cprg

ghg_ctu <- readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/205-ctu-ghg-compiler/_meta/data/ctu_emissions.RDS")

ghg_county <- readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/205-ctu-ghg-compiler/_meta/data/cprg_county_emissions.RDS")

### conform and bind

ghg_inventory <- bind_rows(ghg_ctu %>%
                       dplyr::mutate(emissions_per_capita = value_emissions/ctu_population,
                              unit_emissions = "Metric tons CO2e",
                              category = as.character(category)) %>%
                       dplyr::rename(geog_name = ctu_name,
                              population = ctu_population) %>%
                       dplyr::select(-c(geoid, ctuid)),
                     ghg_county %>%
                       dplyr::mutate(ctu_class = "COUNTY",
                                     category = as.character(category)) %>%
                       dplyr::rename(geog_name = county_name,
                                     population = county_total_population) %>%
                       dplyr::select(-c(data_source, factor_source,geoid,population_data_source))
)

### remap sectors for alternate graphing
ghg_inventory_alt <- ghg_inventory %>%
  mutate(sector = case_when(
    category
  ))


usethis::use_data(ghg_inventory, overwrite = TRUE)
