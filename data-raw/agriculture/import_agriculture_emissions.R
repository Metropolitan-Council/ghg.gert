library(dplyr)
pkgload::load_all()
source("data-raw/meta/compile_townships.R")

county_ag_data <-
  readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_meta/data/cprg_county_emissions.RDS") %>%
  filter(
    sector == "Agriculture",
    !county_name %in% c(
      "St. Croix",
      "Sherburne",
      "Pierce",
      "Chisago"
    )
  ) %>%
  mutate(geog_name = paste(county_name, "County")) %>%
  select(
    geog_name,
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

# township groups: summed from member townships, matched on GNIS id
ctu_ag_data <- twp_group_replace(
  ctu_ag_data,
  twp_group_sum(ctu_ag_data,
    value_cols = "value_emissions",
    group_cols = c("sector", "category", "source", "inventory_year"),
    key = "geog_id",
    .fill = list(geog_level = tolower(TWP_GROUP_LEVEL))
  )
)
twp_group_check_sum(ctu_ag_data, "value_emissions", "agricultural_emissions", key = "geog_id")

agricultural_emissions <- bind_rows(
  county_ag_data,
  ctu_ag_data
)

usethis::use_data(agricultural_emissions, overwrite = T)
