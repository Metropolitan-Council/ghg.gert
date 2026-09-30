testthat::test_that("Geog index joins smoothly", {
  check_join <- function(x) {
    if ("geog_name" %in% names(x)) {
      join_columns <- intersect(names(geog_index), names(x))

      test_val <- geog_index %>%
        left_join(x, by = join_columns) %>%
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
    building_energy_data,
    check_join
  )

  purrr::map(
    land_use_data,
    check_join
  )
})

testthat::test_that("Credit River and Empire are correct", {
  check_townships <- function(x) {
    if ("geog_name" %in% names(x)) {
      test_val <- x %>%
        dplyr::filter(geog_name %in% c(
          "Credit River Twp.",
          "Empire Twp.",
          "Credit River Township",
          "Empire Township",
          "Fort Snelling (unorg.)"
        )) %>%
        nrow()
    } else {
      test_val <- 0
    }

    testthat::expect_equal(test_val, 0)
  }

  check_townships(geog_index)

  purrr::map(
    transportation_data,
    check_townships
  )


  purrr::map(
    building_energy_data,
    check_townships
  )

  purrr::map(
    land_use_data,
    check_townships
  )
})
