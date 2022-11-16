pass <- suppressMessages(scen_passenger_light_duty(
  .pass_tb = transportation_data$passenger %>%
    filter(ctu == "St. Paul" | ctu == "All")
))


testthat::expect_length(pass, 5)

testthat::expect_named(pass,
  expected = c(
    "vmt",
    "dir_ghg",
    "emb_ghg",
    "fuel_use",
    "cost"
  )
)
