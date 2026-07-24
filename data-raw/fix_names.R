library(dplyr)

# transportation

transportation_data$passenger <- transportation_data$passenger %>%
  left_join(geog_index) %>%
  select(mode, var, geog_name, geog_id, year, value, aeo_mode, type)


transportation_data$freight <- transportation_data$freight %>%
  left_join(geog_index) %>%
  select(mode, var, geog_name, geog_id, year, value, aeo_mode, type)


# bulding energy

building_energy_data <- purrr:::map(
  building_energy_data,
  function(x) {
    if ("ctu_name" %in% names(x)) {
      x %>%
        mutate(
          ctu_class = case_when(
            ctu_name %in% c(
              "Credit River Twp.",
              "Empire Twp."
            ) ~ "CITY",
            stringr::str_detect(ctu_name, "Twp.") ~ "TOWNSHIP",
            stringr::str_detect(ctu_name, "unorg.") ~ "UNORGANIZED TERRITORY",
            TRUE ~ "CITY"
          ),
          ctu_name = stringr::str_remove(ctu_name, "Twp.") %>%
            str_replace("St. ", "Saint ") %>%
            str_remove("(unorg.)") %>%
            str_remove_all("[:punct:]") %>%
            str_trim()
        ) %>%
        left_join(geog_index, by = c("ctu_name", "ctu_class" = "geog_level")) %>%
        select(
          geog_name,
          geog_id,
          everything(),
          -ctu_name, -ctu, -ctu_class, -geog_id_type
        )
    } else {
      return(x)
    }
  }
)

# land use -----
land_use_data <- purrr:::map(
  land_use_data,
  function(x) {
    if ("ctu_name" %in% names(x)) {
      x %>%
        mutate(
          ctu_class = case_when(
            ctu_name %in% c(
              "Credit River Twp.",
              "Empire Twp."
            ) ~ "CITY",
            stringr::str_detect(ctu_name, "Twp.") ~ "TOWNSHIP",
            stringr::str_detect(ctu_name, "unorg.") ~ "UNORGANIZED TERRITORY",
            TRUE ~ "CITY"
          ),
          ctu_name = stringr::str_remove(ctu_name, "Twp.") %>%
            str_replace("St. ", "Saint ") %>%
            str_remove("(unorg.)") %>%
            str_remove_all("[:punct:]") %>%
            str_trim()
        ) %>%
        left_join(geog_index, by = c("ctu_name", "ctu_class" = "geog_level")) %>%
        select(
          geog_name,
          geog_id,
          everything(),
          -ctu_name, -ctu, -ctu_class, -geog_id_type
        )
    } else {
      return(x)
    }
  }
)


# save all -----
usethis::use_data(transportation_data, overwrite = TRUE)
usethis::use_data(land_use_data, overwrite = TRUE)
usethis::use_data(building_data, overwrite = TRUE)
usethis::use_data(building_energy_data, overwrite = TRUE)
