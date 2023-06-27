## basic demographics -----

testthat::test_that("Regional population forecast matches January 2023 released",{

  testthat::expect_equal(
    building_data$residential %>%
      filter(year == 2040,
             var == "population") %>%
      magrittr::extract2("value") %>%
      sum(),
    3703280)
  # actual regional totals (from https://metrocouncil.org/Data-and-Maps/Publications-And-Resources/Files-and-reports/Thrive-MSP-2040-Local-Forecasts-(January-2023)-(1).aspx)
  #
  # 3,653,000 people
  # 2,016,000 jobs

})


testthat::test_that("Minneapolis population forecast matches January 2023 released",{

  testthat::expect_equal(
    building_data$residential %>%
      filter(year == 2040,
             var == "population",
             ctu_name == "Minneapolis") %>%
      magrittr::extract2("value") %>%
      sum(),
    485000)
  # matches
})



testthat::test_that("Maple Plain population forecast matches January 2023 released",{

  testthat::expect_equal(
    building_data$residential %>%
      filter(year == 2040,
             var == "population",
             ctu_name == "Maple Plain") %>%
      magrittr::extract2("value") %>%
      sum(),
    2320)
})

testthat::test_that("Medina population forecast matches January 2023 released",{

  testthat::expect_equal(
    building_data$residential %>%
      filter(year == 2040,
             var == "population",
             ctu_name == "Medina") %>%
      magrittr::extract2("value") %>%
      sum(),
    8900)
})

testthat::test_that("Andover employment forecast matches January 2023 released",{

  testthat::expect_equal(
    building_data$residential %>%
      filter(year == 2040,
             var == "jobs",
             ctu_name == "Andover") %>%
      magrittr::extract2("value") %>%
      sum(),
    7100)
})

testthat::test_that("West St. Paul population forecast matches January 2023 released",{

  testthat::expect_equal(
    building_data$residential %>%
      filter(year == 2040,
             var == "jobs",
             ctu_name == "West St. Paul") %>%
      magrittr::extract2("value") %>%
      sum(),
    9300)
})


testthat::test_that("Rogers total number of jobs missing",{

  testthat::expect_equal(
    building_data$residential %>%
      filter(year == 2040,
             var == "jobs",
             ctu_name == "Rogers") %>%
      magrittr::extract2("value") %>%
      sum(),
    0)
})

# multifamily avg sqft county ------
testthat::test_that("Cities in the same county have same county avg multifamily sqft", {

  # hennepin
  testthat::expect_equal(building_data$residential %>%
                           filter(var == "multifamily_average_floor_area_sqft_county",
                                  year == 2040,
                                  ctu_name %in% c("Minneapolis",
                                                  "Eden Prairie",
                                                  "Edina",
                                                  "Golden Valley",
                                                  "Deephaven",
                                                  "Medina",
                                                  "Maple Grove",
                                                  "Maple Plain")
                           ) %>%
                           magrittr::extract2("value") %>%
                           unique() %>%
                           length(), 1)

  # washington
  testthat::expect_equal(building_data$residential %>%
                           filter(var == "multifamily_average_floor_area_sqft_county",
                                  year == 2040,
                                  ctu_name %in% c("Woodbury",
                                                  "Lake Elmo",
                                                  "Scandia",
                                                  "Forest Lake",
                                                  "Cottage Grove")
                           ) %>%
                           magrittr::extract2("value") %>%
                           unique() %>%
                           length(), 1)

  # ramsey
  testthat::expect_equal(building_data$residential %>%
                           filter(var == "multifamily_average_floor_area_sqft_county",
                                  year == 2040,
                                  ctu_name %in% c("St. Paul",
                                                  "North Oaks",
                                                  "Arden Hills",
                                                  "New Brighton",
                                                  "Vadnais Heights")
                           ) %>%
                           magrittr::extract2("value") %>%
                           unique() %>%
                           length(), 1)


})

testthat::test_that("Lauderdale industrial employment baseline and forecast correct", {

  building_data$non_residential %>%
    dplyr::filter(ctu_name == "Lauderdale",
                  var == "industrial_jobs",
                  year == 2018) %>%
    magrittr::extract2("value") %>%
    testthat::expect_equal(0)



})
