school_bus <- suppressMessages(
  scen_school_bus(
    .pass_tb = transportation_data$passenger %>%
      filter(ctu == "St. Paul" | ctu == "All"),
    .calc_transp_cost = TRUE,
    .calc_transp_fuel_use = TRUE,
    .calc_transp_ghg_embodied = TRUE
  )
)

testthat::expect_length(school_bus, 5)

testthat::expect_named(school_bus,
  expected = c(
    "vmt",
    "dir_ghg",
    "emb_ghg",
    "fuel_use",
    "cost"
  ),
  ignore.order = TRUE
)
