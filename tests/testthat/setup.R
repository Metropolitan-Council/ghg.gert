st_paul_passenger <- transportation_data$passenger %>%
  filter(ctu == "St. Paul" | ctu == "All")

st_paul_freight <- transportation_data$freight %>%
  filter(ctu == "St. Paul" | ctu == "All")

