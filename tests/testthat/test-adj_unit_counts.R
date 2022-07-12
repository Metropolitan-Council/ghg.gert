# Lake Elmo 50%
## 4646 single family in 2040 - 3084 single family in 2018 = 1562
## 1562 * 0.5 = 781
## 3259 + 781 = 4040 multi
## 4646 - 781 = 3865 single

new_res_tb <- adj_unit_counts(
  res_tb = lake_elmo_res,
  .new_homes_to_multifamily_pct = 0.5
) %>%
  filter(
    var %in% c(
      "single_family_units",
      "multifamily_units"
    ),
    year == 2040
  ) %>%
  ungroup()

expected_res_tb <- tibble::tribble(
  ~ctu_name, ~year, ~var, ~value,
  "Lake Elmo", 2040, "multifamily_units", 4040,
  "Lake Elmo", 2040, "single_family_units", 3865
)

testthat::expect_equal(expected_res_tb, new_res_tb)

lake_elmo_res %>%
  filter(var %in% c(
    "single_family_units",
    "multifamily_units"
  ))

## 25%
# 1562 * 0.25 = 390.5
## 3259 + 390.5 = 3649.5 multi
## 4646 - 390.5 = 4255.5 single
new_25 <- adj_unit_counts(
  res_tb = lake_elmo_res,
  .new_homes_to_multifamily_pct = 0.25
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
  "Lake Elmo", 2040, "multifamily_units", 3649.5,
  "Lake Elmo", 2040, "single_family_units", 4255.5
)

testthat::expect_equal(expected_25_res_tb, new_25)
