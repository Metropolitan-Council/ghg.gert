
calc_drs_sales(
  tb = transportation_data$passenger %>%
    filter(ctu == "St. Paul" | ctu == "All"),
  .drs_pct = 0.9,
  .enviro_factors = .enviro_factors
)
