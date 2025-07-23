#### bring in MPCA SCORE data to most recent data year (2021) ----

inpath <- "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_waste/data-raw/solid_waste/"

waste_baseline <- readr::read_rds(paste0(inpath, "mpca_score_allyrs.RDS"))
# state_total is for methane recovery or FOD. we don't need it

#### bring in population data (2020)
population_data <- demographic_data %>%
  dplyr::filter(sp_categories == "population") %>%
  dplyr::right_join(ctu_county, relationship = "many-to-many") %>%
  dplyr::mutate(
    geoid = paste0("27", stringr::str_pad(as.character(co_code), 3, pad = "0"))
  ) %>%
  dplyr::select(
    -c(co_code, pct_land_area)
  )

population_county <- population_data %>%
  dplyr::mutate(co_pop_allocation = round(value * pct_population)) %>%
  dplyr::group_by(inventory_year, geoid) %>%
  dplyr::summarise(
    county_pop = sum(co_pop_allocation)
  ) %>%
  tidyr::drop_na(county_pop)

#### generate per cap activity data for 2020

waste_per_cap <- waste_baseline %>%
  dplyr::right_join(population_county %>% dplyr::filter(inventory_year == 2020)) %>%
  dplyr::mutate(
    value_per_cap = value_activity/county_pop
  ) %>%
  dplyr::select(
    geoid,
    source,
    value_per_cap
  )

#### generate per-county activity projections past 2020

# assuming per cap activity data is constant for each county and consistent with 2020
waste_data_county <- population_county %>%
  dplyr::filter(inventory_year != 2010) %>%
  dplyr::left_join(waste_per_cap, relationship = "many-to-many") %>%
  dplyr::mutate(
    value_activity = county_pop * value_per_cap,
    class = "COUNTY",
    units_activity = "metric tons MSW"
  ) %>%
  dplyr::select(
    inventory_year,
    geoid,
    county_pop,
    source,
    value_activity,
    units_activity
  )

#### allocate to ctus

waste_data_ctu <- population_data %>%
  dplyr::right_join(ctu_county, relationship = "many-to-many") %>%
  dplyr::right_join(waste_data_county, relationship = "many-to-many") %>%
  dplyr::mutate(ctu_percent_of_county_pop = value/county_pop) %>%
  dplyr::mutate(value_activity = value_activity*ctu_percent_of_county_pop,
                units_activity = "metric tons MSW"
  ) %>%
  dplyr::select(
    inventory_year,
    ctu_id,
    ctu_name,
    source,
    value_activity,
    units_activity
  )

# to test: is waste_projections 2020 activity data equal to waste_baseline 2020 data

waste_data <-list()
waste_data$ctu <- waste_data_ctu
waste_data$county <- waste_data_county %>%
  dplyr::select(
    -county_pop
  )

# waste characterization
waste_char <- readr::read_rds(paste0(inpath, "mpca_waste_composition.RDS"))
waste_data$characterization <- waste_char

# usethis::use_data(waste_data, overwrite = TRUE)
