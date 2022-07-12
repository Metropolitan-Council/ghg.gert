st_paul_passenger <- transportation_data$passenger %>%
  filter(ctu == "St. Paul" | ctu == "All")

st_paul_freight <- transportation_data$freight %>%
  filter(ctu == "St. Paul" | ctu == "All")

lake_elmo_res <- building_data$residential %>%
  dplyr::filter(ctu_name == "Lake Elmo")

lake_elmo_non_res <- building_data$non_residential %>%
  dplyr::filter(ctu_name == "Lake Elmo")
