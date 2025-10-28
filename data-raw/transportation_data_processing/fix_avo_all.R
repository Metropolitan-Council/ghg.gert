# pull out AVO on its own
pkgload::load_all()

vehicle_occupancy <- transportation_data$passenger %>%
  filter(var == "AVO") %>%
  bind_rows(
    transportation_data$freight %>%
      filter(var == "AVO")
  )


transportation_data$passenger <- transportation_data$passenger %>%
  filter(!(var == "AVO"))

transportation_data$freight <- transportation_data$freight %>%
  filter(!(var == "AVO"))

usethis::use_data(transportation_data, overwrite = TRUE)

usethis::use_data(vehicle_occupancy, overwrite = TRUE)
