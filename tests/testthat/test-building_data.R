## basic demographics -----

testthat::test_that("Regional population forecast matches January 2023 released", {
  testthat::expect_equal(
    demographic_data %>%
      filter(
        inventory_year == 2050,
        sp_categories == "population",
        geog_level == "CITY"
      ) %>%
      magrittr::extract2("value") %>%
      sum(),
    3703280,
    tolerance = 10000
  )
  # actual regional totals (from https://metrocouncil.org/Data-and-Maps/Publications-And-Resources/Files-and-reports/Thrive-MSP-2040-Local-Forecasts-(January-2023)-(1).aspx)
  #
  # 3,653,000 people
  # 2,016,000 jobs
})


testthat::test_that("Minneapolis population forecast matches January 2023 released", {
  testthat::expect_equal(
    demographic_data %>%
      filter(
        inventory_year == 2040,
        sp_categories == "population",
        geog_name == "Minneapolis"
      ) %>%
      magrittr::extract2("value") %>%
      sum(),
    485000,
    tolerance = 1000
  )
  # matches
})



testthat::test_that("Maple Plain population forecast matches January 2023 released", {
  testthat::expect_equal(
    demographic_data %>%
      filter(
        inventory_year == 2040,
        sp_categories == "population",
        geog_name == "Maple Plain"
      ) %>%
      magrittr::extract2("value") %>%
      sum(),
    2320,
    tolerance = 100
  )
})

testthat::test_that("Medina population forecast matches January 2023 released", {
  testthat::expect_equal(
    demographic_data %>%
      filter(
        inventory_year == 2040,
        sp_categories == "population",
        geog_name == "Medina"
      ) %>%
      magrittr::extract2("value") %>%
      sum(),
    8900,
    tolerance = 1000
  )
})

testthat::test_that("Andover employment forecast matches January 2023 released", {
  testthat::expect_equal(
    demographic_data %>%
      filter(
        inventory_year == 2040,
        sp_categories == "jobs",
        geog_name == "Andover"
      ) %>%
      magrittr::extract2("value") %>%
      sum(),
    7100,
    tolerance = 100
  )
})

testthat::test_that("West Saint Paul population forecast matches January 2023 released", {
  testthat::expect_equal(
    demographic_data %>%
      filter(
        inventory_year == 2040,
        sp_categories == "jobs",
        geog_name == "West Saint Paul"
      ) %>%
      magrittr::extract2("value") %>%
      sum(),
    9300,
    tolerance = 1000
  )
})


testthat::test_that("Rogers total number of jobs available", {
  testthat::expect_equal(
    demographic_data %>%
      filter(
        inventory_year == 2040,
        sp_categories == "jobs",
        geog_name == "Rogers"
      ) %>%
      magrittr::extract2("value") %>%
      sum(),
    17158
  )
})

# multifamily avg sqft county ------
# testthat::test_that("Cities in the same county have same county avg multifamily sqft", {
#   # hennepin
#   testthat::expect_equal(building_data$residential %>%
#     filter(
#       var == "multifamily_average_floor_area_sqft_county",
#       inventory_year == 2040,
#       geog_name %in% c(
#         "Minneapolis",
#         "Eden Prairie",
#         "Edina",
#         "Golden Valley",
#         "Deephaven",
#         "Medina",
#         "Maple Grove",
#         "Maple Plain"
#       )
#     ) %>%
#     magrittr::extract2("value") %>%
#     unique() %>%
#     length(), 1)
#
#   # washington
#   testthat::expect_equal(building_data$residential %>%
#     filter(
#       var == "multifamily_average_floor_area_sqft_county",
#       inventory_year == 2040,
#       geog_name %in% c(
#         "Woodbury",
#         "Lake Elmo",
#         "Scandia",
#         "Forest Lake",
#         "Cottage Grove"
#       )
#     ) %>%
#     magrittr::extract2("value") %>%
#     unique() %>%
#     length(), 1)
#
#   # ramsey
#   testthat::expect_equal(building_data$residential %>%
#     filter(
#       var == "multifamily_average_floor_area_sqft_county",
#       inventory_year == 2040,
#       geog_name %in% c(
#         "Saint Paul",
#         "North Oaks",
#         "Arden Hills",
#         "New Brighton",
#         "Vadnais Heights"
#       )
#     ) %>%
#     magrittr::extract2("value") %>%
#     unique() %>%
#     length(), 1)
# })

testthat::test_that("Lauderdale industrial employment baseline and forecast correct", {
  building_data$non_residential %>%
    dplyr::filter(
      geog_name == "Lauderdale",
      sp_categories == "industrial_jobs",
      inventory_year == 2021
    ) %>%
    magrittr::extract2("value") %>%
    testthat::expect_equal(114.7)
})

testthat::test_that("job counts not in residential dataset", {
  testthat::expect_equal(
    building_data$residential %>%
      filter(sp_categories %in% c(
        "jobs",
        "industrial_jobs",
        "commercial_jobs"
      )) %>%
      dplyr::arrange(geog_name) %>%
      nrow(),
    0
  )
})


testthat::test_that("Generic tests on residential data", {
  testthat::expect_equal(
    building_data$residential$sp_categories %>% unique(),
    c(
      "multifamily_units", "single_family_attached", "single_family_large_lot",
      "single_family_small_lot"
    )
  )
})
