testthat::test_that("bus mpg correct", {

  testthat::expect_equal(
    st_paul_passenger %>%
      filter(mode == "BU",
             var == "BCIMPG") %>%
      select(year, var, value),
    tibble::tribble(
      ~year,     ~var,  ~value,
      "2015", "BCIMPG",  4.6805,
      "2018", "BCIMPG", 4.69454,
      "2020", "BCIMPG",  4.7039,
      "2025", "BCIMPG", 4.75106,
      "2030", "BCIMPG", 4.82268,
      "2035", "BCIMPG", 4.91986,
      "2040", "BCIMPG", 5.04409,
      "2045", "BCIMPG", 5.19732,
      "2050", "BCIMPG", 5.38198
    )
  )

})
