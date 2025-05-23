testthat::test_that("Geog index joins smoothly", {
  check_join <- function(x) {
    if ("geog_name" %in% names(x)) {
      test_val <- geog_index %>%
        left_join(x, by = c("geog_name", "geog_id")) %>%
        filter(is.na(geog_level) | is.na(geog_id) | is.na(geog_id_type)) %>%
        nrow()
    } else {
      test_val <- 0
    }

    testthat::expect_equal(test_val, 0)
  }


  purrr::map(
    transportation_data,
    check_join
  )

  purrr::map(
    building_data,
    check_join
  )

  purrr::map(
    building_energy_data,
    check_join
  )

  purrr::map(
    land_use_data,
    check_join
  )
})
