devtools::load_all()

transportation_data$passenger <- transportation_data$passenger %>%
  filter(var == "PMT") %>%
  mutate(value =round( value * 1000, 2))%>%
  bind_rows(transportation_data$passenger %>%
              filter(var != "PMT"))


usethis::use_data(transportation_data, overwrite = TRUE)
