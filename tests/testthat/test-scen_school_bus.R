school_bus <- suppressMessages(
  scen_school_bus(
    .pass_tb = transportation_data$passenger %>%
      filter(ctu == "St. Paul" | ctu == "All")
  )
)

testthat::expect_length(school_bus, 5)

testthat::expect_named(school_bus, expected = c(
  "vmt",
  "dir_ghg",
  "emb_ghg",
  "fuel_use",
  "cost"
))
