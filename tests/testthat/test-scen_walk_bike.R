walk_bike <- suppressMessages(scen_walk_bike(
  .pass_tb = transportation_data$passenger %>%
    filter(ctu == "St. Paul" | ctu == "All")
))

testthat::expect_length(walk_bike, 2)

testthat::expect_named(walk_bike,
  expected = c(
    "vmt",
    "dir_ghg"
  ),
  ignore.order = TRUE
)
