devtools::load_all()

transportation_data$passenger <- transportation_data$passenger %>%
  filter(var == "PARK") %>%
  mutate(value = value/10) %>%
  bind_rows(transportation_data$passenger %>%
              filter(var != "PARK"))


usethis::use_data(transportation_data, overwrite = TRUE)
