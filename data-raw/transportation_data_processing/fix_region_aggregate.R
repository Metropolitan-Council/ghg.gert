pkgload::load_all()

exclude_geogs <- c("Twin Cities Region", "Anoka County", "Carver County", "Dakota County", "Hennepin County", "Ramsey County", "Scott County", "Washington County")

transportation_data$passenger <-
  bind_rows(
    transportation_data$passenger %>%
      filter(geog_name != "Twin Cities Region"),
    transportation_data$passenger %>%
      filter(!geog_name %in% exclude_geogs) %>%
      group_by(mode, var, year, type, aeo_mode) %>%
      unique() %>%
      summarize(value = sum(value, na.rm = T) %>% round(digits = 3)) %>%
      mutate(
        geog_id = "00000000",
        geog_name = "Twin Cities Region"
      ) %>%
      select(names(transportation_data$passenger))
  )


transportation_data$freight <-
  bind_rows(
    transportation_data$freight %>%
      filter(geog_name != "Twin Cities Region"),
    transportation_data$freight %>%
      filter(!geog_name %in% exclude_geogs) %>%
      group_by(mode, var, year, type, aeo_mode) %>%
      summarize(value = sum(value, na.rm = T) %>% round(digits = 3)) %>%
      mutate(
        geog_id = "00000000",
        geog_name = "Twin Cities Region"
      ) %>%
      select(names(transportation_data$freight))
  )

usethis::use_data(transportation_data, overwrite = TRUE)
