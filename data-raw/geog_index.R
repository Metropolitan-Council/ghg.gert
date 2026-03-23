# create index of geographies
# for joining with

library(dplyr)
library(stringr)
pkgload::load_all()

cprg_ctu <- readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/refs/heads/main/_meta/data/cprg_ctu.RDS") %>%
  sf::st_drop_geometry() %>%
  filter(county_name %in% c(
    "Anoka",
    "Carver",
    "Dakota",
    "Hennepin",
    "Ramsey",
    "Scott",
    "Washington"
  )) %>%
  select(-statefp, -state_abb, -geoid_wis, -cprg_area)

transportation_geog <- transportation_data$passenger %>%
  dplyr::select(geog_name) %>%
  unique() %>%
  mutate(
    ctu_class = case_when(
      geog_name %in% c(
        "Credit River Twp.",
        "Empire Twp."
      ) ~ "CITY",
      geog_name %in% c(
        # Fort Snelling was getting labeled as CITY,
        # and would show an NA for geog_name after joining
        "Fort Snelling"
      ) ~ "UNORGANIZED TERRITORY",
      geog_name %in% c(
        "Twin Cities Region"
      ) ~ "REGION",
      stringr::str_detect(geog_name, "Twp.") ~ "TOWNSHIP",
      stringr::str_detect(geog_name, "unorg.") ~ "UNORGANIZED TERRITORY",
      TRUE ~ "CITY"
    ),
    ctu_name = stringr::str_remove(geog_name, "Twp.") %>%
      str_replace("St. ", "Saint ") %>%
      str_remove("(unorg.)") %>%
      str_remove_all("[:punct:]") %>%
      str_trim()
  ) %>%
  mutate(ctu_name_full = case_when(
    ctu_class == "TOWNSHIP" ~ paste0(ctu_name, " Twp."),
    TRUE ~ ctu_name
  ))


dplyr::anti_join(
  transportation_geog,
  cprg_ctu
)


geog_index_ctu <- dplyr::left_join(
  cprg_ctu,
  transportation_geog
) %>%
  select(geog_name = ctu_name_full, geog_short_name = ctu_name, geog_level = ctu_class, geog_id = gnis) %>%
  mutate(geog_id_type = "ctu_gnis",
         ctu = geog_short_name, # temporary need to keep for transportation functions
         ctu_name = geog_name) %>%
  unique()

### add county data

cprg_county <- readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/refs/heads/main/_meta/data/cprg_county.RDS") %>%
  sf::st_drop_geometry() %>%
  filter(county_name %in% c(
    "Anoka",
    "Carver",
    "Dakota",
    "Hennepin",
    "Ramsey",
    "Scott",
    "Washington"
  )) %>%
  select(geog_name = county_name_full, geog_short_name = county_name, geog_id = geoid) %>%
  mutate(geog_id_type = "county_fips",
         geog_level = "COUNTY")

geog_index <- bind_rows(as_tibble(cprg_county),
                        geog_index_ctu) %>%
  filter(geog_name != "Fort Snelling")

usethis::use_data(geog_index, overwrite = TRUE)
