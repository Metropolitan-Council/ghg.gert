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
  dplyr::select(ctu) %>%
  unique() %>%
  mutate(
    ctu_class = case_when(
      ctu %in% c(
        "Credit River Twp.",
        "Empire Twp."
      ) ~ "CITY",
      stringr::str_detect(ctu, "Twp.") ~ "TOWNSHIP",
      stringr::str_detect(ctu, "unorg.") ~ "UNORGANIZED TERRITORY",
      TRUE ~ "CITY"
    ),
    ctu_name = stringr::str_remove(ctu, "Twp.") %>%
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


geog_index <- dplyr::left_join(
  cprg_ctu,
  transportation_geog
) %>%
  select(ctu, geog_name = ctu_name_full, geog_level = ctu_class, geog_id = gnis) %>%
  mutate(geog_id_type = "ctu_gnis") %>%
  unique()


usethis::use_data(geog_index, overwrite = TRUE)
