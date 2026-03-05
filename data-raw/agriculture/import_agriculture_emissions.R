library(dplyr)

county_ag_data <-
  readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_meta/data/cprg_county_emissions.RDS") %>%
  filter(sector == "Agriculture") %>%
  select(
    geog_name = county_name,
    geog_id = geoid,
    geog_level,
    sector,
    category,
    source,
    inventory_year = emissions_year,
    value_emissions
  )

ctu_ag_data <-
  readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_meta/data/ctu_emissions.RDS") %>%
  filter(sector == "Agriculture") %>%
  mutate(geog_name = if_else(ctu_class == "TOWNSHIP",
    paste(geog_name, "Twp."),
    geog_name
  )) %>%
  select(
    geog_name,
    geog_id = ctu_id_gnis,
    geog_level,
    sector,
    category,
    source,
    inventory_year = emissions_year,
    value_emissions
  )

agricultural_emissions <- bind_rows(
  county_ag_data,
  ctu_ag_data
)

usethis::use_data(agricultural_emissions, overwrite = T)
