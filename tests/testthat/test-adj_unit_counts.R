# Lake Elmo 50%
## 4646 single family in 2040 - 3084 single family in 2018 = 1562
## 1562 * 0.5 = 781
## 3259 + 781 = 4040 multi
## 4646 - 781 = 3865 single

new_25 <- adj_unit_counts(
  res_tb = lake_elmo_res,
  .selected_ctu = "Lake Elmo",
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
  "Lake Elmo", 2040, "single_family_units", 4255.5,
  "Lake Elmo", 2040, "multifamily_units", 3649.5
)

testthat::expect_equal(expected_25_res_tb, new_25)
