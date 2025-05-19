devtools::load_all()
# original data was given to us in 1000s of miles,
# so we will convert it to miles
transportation_data$passenger <- transportation_data$passenger %>%
  filter(var == "PMT") %>%
  mutate(value = round(value * 1000, 2)) %>%
  bind_rows(transportation_data$passenger %>%
    filter(var != "PMT"))


transportation_data$freight <- transportation_data$freight %>%
  filter(var == "TMT") %>%
  mutate(value = round(value * 1000, 2)) %>%
  bind_rows(transportation_data$freight %>%
              filter(var != "TMT"))

usethis::use_data(transportation_data, overwrite = TRUE)
