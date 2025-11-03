# remove all autonomous vehicle references

transportation_data$passenger <- transportation_data$passenger %>%
  ungroup() %>%
  filter(mode != "AV")

usethis::use_data(transportation_data, overwrite = TRUE)
