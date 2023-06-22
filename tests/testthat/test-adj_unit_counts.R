# Lake Elmo 50%
## 4646 single family in 2040 - 3084 single family in 2018 = 1562
## 3259 multi family in 2040 - 236.0 single family in 2018 = 3023
## 1562 * 0.5 = 781
##
##
## 3259 + 781 = 4040 multi
## 4646 - 781 = 3865 single

test_that("Lake Elmo unit counts", {
  new_25 <- adj_unit_counts(
    res_tb = lake_elmo_res,
    .selected_ctu = "Lake Elmo",
    .new_homes_to_multifamily_pct = 0.50
  ) %>%
    filter(
      var %in% c(
        "single_family_units",
        "multifamily_units"
      ),
      year == 2040
    ) %>%
    ungroup()


  expected_25_res_tb <- tibble::tribble(
    ~ctu_name, ~year, ~var, ~value,
    "Lake Elmo", 2040, "single_family_units", 3865,
    "Lake Elmo", 2040, "multifamily_units", 4040
  )

  testthat::expect_equal(expected_25_res_tb, new_25)
})


# Lake Elmo 50%
## 4646 single family in 2040 - 3084 single family in 2018 = 1562
## 1562 * 0.5 = 781
## 3259 + 781 = 4040 multi
## 4646 - 781 = 3865 single


# Minneapolis
# 76935 SF units in 2018, 76494 SF units in 2040 = -441 decrease
# 123504 MF units in 2018, 145653 MF units in 2040 = 22149 increase
# -441 * 0.5 = -220.5
#
# new SF units = -220
# new MF units  = 22149 + -220.5 = 21928
# BUT 22149 + 220.5 = 22369
#
# 2040 total single family = 76494 - 220 = 76274
# 2040 total multi family = 145873 + 220 = 146093


test_that("Minneapolis unit counts", {
  mpls_50 <- adj_unit_counts(
    res_tb = building_energy_bau_data$residential,
    .selected_ctu = "Minneapolis",
    .new_homes_to_multifamily_pct = 0.50
  ) %>%
    suppressWarnings() %>%
    filter(
      var %in% c(
        "single_family_units",
        "multifamily_units"
      ),
      year == 2040
    ) %>%
    ungroup()

  expected_mpls_res_tb <- tibble::tribble(
    ~ctu_name, ~year, ~var, ~value,
    "Minneapolis", 2040, "multifamily_units", 145873.5,
    "Minneapolis", 2040, "single_family_units", 76273.5
  )

  testthat::expect_warning(
    adj_unit_counts(
      res_tb = building_energy_bau_data$residential,
      .selected_ctu = "Minneapolis",
      .new_homes_to_multifamily_pct = 0.0
    ) %>%
      filter(
        var %in% c(
          "single_family_units",
          "multifamily_units"
        )
      ) %>%
      ungroup()
  )


  mpls_0 <- adj_unit_counts(
    res_tb = building_energy_bau_data$residential,
    .selected_ctu = "Minneapolis",
    .new_homes_to_multifamily_pct = 0.0
  ) %>%
    suppressWarnings() %>%
    filter(
      var %in% c(
        "single_family_units",
        "multifamily_units"
      )
    ) %>%
    ungroup()


  bau_forecast <- building_energy_bau_data$residential %>%
    filter(
      ctu_name == "Minneapolis",
      var %in% c(
        "single_family_units",
        "multifamily_units"
      )
    )

  # minneapolis has no effect, because they are already
  # projected to reduce the number of SF housing units
  testthat::expect_equal(mpls_0, bau_forecast)

  testthat::expect_equal(mpls_50, expected_mpls_res_tb)
})
