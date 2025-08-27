testthat::test_that("Varous AEO calculations align", {
  testthat::expect_equal(
    vmt_annual_energy_outlook(
      tb = st_paul_passenger,
      .mode = "PLDV",
      .aeo_scenario = "REF",
      .enviro_factors = enviro_factors
    ),
    tibble::tribble(
      ~year, ~metric, ~aeo_adj,
      "2015", "VMT", 1,
      "2018", "VMT", 1,
      "2020", "VMT", 1,
      "2025", "VMT", 1,
      "2030", "VMT", 1,
      "2035", "VMT", 1,
      "2040", "VMT", 1,
      "2045", "VMT", 1,
      "2050", "VMT", 1
    )
  )


  testthat::expect_error(
    vmt_annual_energy_outlook(
      tb = st_paul_passenger,
      .mode = "PLDV",
      .aeo_scenario = "foo",
      .enviro_factors = enviro_factors
    )
  )


  # testthat::expect_error(
  #   vmt_annual_energy_outlook(
  #     tb = st_paul_passenger,
  #     .mode = "BRT",
  #     .aeo_scenario = "HOGS",
  #     .enviro_factors = enviro_factors
  #   ),
  #   tibble::tribble(
  #     ~year, ~metric, ~aeo_adj,
  #     "2015", "VMT", 1,
  #     "2018", "VMT", 1,
  #     "2020", "VMT", 0.998807314,
  #     "2025", "VMT", 0.999019465,
  #     "2030", "VMT", 0.998756156,
  #     "2035", "VMT", 0.998025806,
  #     "2040", "VMT", 0.997967067,
  #     "2045", "VMT", 0.99845031,
  #     "2050", "VMT", 0.998769925
  #   ),
  #   tolerance = 0.1
  # )



  # testthat::expect_error(
  #   vmt_annual_energy_outlook(
  #     tb = st_paul_passenger,
  #     .mode = "BRT",
  #     .aeo_scenario = "LOGS",
  #     .enviro_factors = enviro_factors
  #   ),
  #   tibble::tribble(
  #     ~year, ~metric, ~aeo_adj,
  #     "2015", "VMT", 1,
  #     "2018", "VMT", 1,
  #     "2020", "VMT", 1,
  #     "2025", "VMT", 1.00052543,
  #     "2030", "VMT", 1.000251198,
  #     "2035", "VMT", 1.000895809,
  #     "2040", "VMT", 1.001133691,
  #     "2045", "VMT", 1.001101447,
  #     "2050", "VMT", 1.001088814
  #   ),
  #   tolerance = 0.1
  # )


  testthat::expect_error(
    vmt_annual_energy_outlook(
      tb = st_paul_passenger,
      .mode = "SUT",
      .aeo_scenario = "REF",
      .enviro_factors = enviro_factors
    )
  )

  testthat::expect_equal(
    vmt_annual_energy_outlook(
      tb = st_paul_freight,
      .mode = "SUT",
      .aeo_scenario = "HM",
      .enviro_factors = enviro_factors
    ),
    tibble::tribble(
      ~year, ~metric, ~aeo_adj,
      "2015", "VMT", 1,
      "2018", "VMT", 1,
      "2020", "VMT", 1,
      "2025", "VMT", 1.057351259,
      "2030", "VMT", 1.081618183,
      "2035", "VMT", 1.106270204,
      "2040", "VMT", 1.14161273,
      "2045", "VMT", 1.168328806,
      "2050", "VMT", 1.198745275
    ),
    tolerance = 0.1
  )


  testthat::expect_equal(
    vmt_annual_energy_outlook(
      tb = st_paul_freight,
      .mode = "FR",
      .aeo_scenario = "HP",
      .enviro_factors = enviro_factors
    ),
    tibble::tribble(
      ~year, ~metric, ~aeo_adj,
      "2015", "VMT", 1,
      "2018", "VMT", 1,
      "2020", "VMT", 1,
      "2025", "VMT", 0.986491615,
      "2030", "VMT", 0.998672019,
      "2035", "VMT", 0.990958159,
      "2040", "VMT", 1.027664654,
      "2045", "VMT", 1.038708787,
      "2050", "VMT", 1.043669593
    ),
    tolerance = 0.1
  )



  testthat::expect_equal(
    vmt_annual_energy_outlook(
      tb = st_paul_freight,
      .mode = "WAT",
      .aeo_scenario = "LP",
      .enviro_factors = enviro_factors
    ),
    tibble::tribble(
      ~year, ~metric, ~aeo_adj,
      "2015", "VMT", 1,
      "2018", "VMT", 1,
      "2020", "VMT", 1,
      "2025", "VMT", 0.990775014,
      "2030", "VMT", 0.991410027,
      "2035", "VMT", 0.991916921,
      "2040", "VMT", 1.000839403,
      "2045", "VMT", 0.997446366,
      "2050", "VMT", 0.993130447
    ),
    tolerance = 0.1
  )


  testthat::expect_error(vmt_annual_energy_outlook(
    tb = st_paul_freight,
    .mode = "PLDV",
    .aeo_scenario = "LM",
    .enviro_factors = enviro_factors
  ))

  testthat::expect_equal(
    vmt_annual_energy_outlook(
      tb = st_paul_passenger,
      .mode = "PLDV",
      .aeo_scenario = "LM",
      .enviro_factors = enviro_factors
    ),
    tibble::tribble(
      ~year, ~metric, ~aeo_adj,
      "2015", "VMT", 1,
      "2018", "VMT", 1,
      "2020", "VMT", 1,
      "2025", "VMT", 0.970544655,
      "2030", "VMT", 0.942742172,
      "2035", "VMT", 0.921081839,
      "2040", "VMT", 0.905597365,
      "2045", "VMT", 0.888858877,
      "2050", "VMT", 0.871008152
    ),
    tolerance = 0.1
  )
})
