transit_rail <- suppressMessages(
  scen_transit_rail(
    .pass_tb = transportation_data$passenger %>%
      filter(ctu == "St. Paul" | ctu == "All")
  ))

testthat::expect_length(transit_rail, 5)

testthat::expect_named(transit_rail,
                       expected = c("vmt",
                                    "dir_ghg",
                                    "emb_ghg",
                                    "fuel_use",
                                    "cost"))
