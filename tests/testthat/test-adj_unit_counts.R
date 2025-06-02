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
    dplyr::filter(sp_categories %in% c(
      "multifamily_units",
      "single_family_units",
      "single_family_attached",
      "single_family_small_lot",
      "single_family_large_lot"
    )) %>%
    filter(inventory_year == max(inventory_year)) %>%
    ungroup()


  expected_25_res_tb <- tibble::tribble(
    ~geog_name, ~geog_id, ~geog_id_type, ~sp_categories, ~geog_level, ~inventory_year, ~value, ~value_change_from_base,
    "Lake Elmo", "02395589", "ctu_gnis", "multifamily_units", "CITY", 2050, 1712.78333333333, 852.983333333333,
    "Lake Elmo", "02395589", "ctu_gnis", "single_family_attached", "CITY", 2050, 480.183333333333, 154.083333333333,
    "Lake Elmo", "02395589", "ctu_gnis", "single_family_large_lot", "CITY", 2050, 3517.51666666667, 22.4166666666667,
    "Lake Elmo", "02395589", "ctu_gnis", "single_family_small_lot", "CITY", 2050, 2036.51666666667, 187.416666666667
  )%>%
    select(-geog_level, -geog_id_type)

  testthat::expect_equal(expected_25_res_tb, new_25)

  testthat::expect_equal(
    # test that total number of housing units is consistent
    lake_elmo_res %>%
      dplyr::filter(sp_categories %in% c(
        "multifamily_units",
        "single_family_units",
        "single_family_attached",
        "single_family_small_lot",
        "single_family_large_lot"
      )) %>%
      filter(inventory_year == max(inventory_year)) %>%
      ungroup() %>%
      group_by(geog_name, geog_id, inventory_year) %>%
      summarize(
        value = sum(value),
        value_change_from_base = sum(value_change_from_base), .groups = "keep"
      ),
    new_25 %>%
      dplyr::filter(sp_categories %in% c(
        "multifamily_units",
        "single_family_units",
        "single_family_attached",
        "single_family_small_lot",
        "single_family_large_lot"
      )) %>%
      filter(inventory_year == max(inventory_year)) %>%
      ungroup() %>%
      group_by(geog_name, geog_id, inventory_year) %>%
      summarize(
        value = sum(value),
        value_change_from_base = sum(value_change_from_base), .groups = "keep"
      )
  )
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
  t_50 <- adj_unit_counts(
    res_tb = building_data$residential,
    .selected_ctu = "Minneapolis",
    .new_homes_to_multifamily_pct = 0.50
  ) %>%
    suppressWarnings() %>%
    dplyr::filter(sp_categories %in% c(
      "multifamily_units",
      "single_family_units",
      "single_family_attached",
      "single_family_small_lot",
      "single_family_large_lot"
    )) %>%
    filter(inventory_year == max(inventory_year)) %>%
    ungroup()

  expected_mpls_res_tb <- tibble::tribble(
    ~geog_name, ~geog_id, ~geog_id_type, ~sp_categories, ~geog_level, ~inventory_year, ~value, ~value_change_from_base,
    "Minneapolis", "02395345", "ctu_gnis", "multifamily_units", "CITY", 2050, 135652.55, 16196.05,
    "Minneapolis", "02395345", "ctu_gnis", "single_family_attached", "CITY", 2050, 35540.1833333333, -1342.71666666667,
    "Minneapolis", "02395345", "ctu_gnis", "single_family_large_lot", "CITY", 2050, 1561.51666666667, 52.4166666666667,
    "Minneapolis", "02395345", "ctu_gnis", "single_family_small_lot", "CITY", 2050, 66312.75, -2955.75
  ) %>%
    select(-geog_level, -geog_id_type)



  testthat::expect_warning(
    adj_unit_counts(
      res_tb = building_data$residential,
      .selected_ctu = "Minneapolis",
      .new_homes_to_multifamily_pct = 0.0
    ) %>%
      dplyr::filter(sp_categories %in% c(
        "multifamily_units",
        "single_family_units",
        "single_family_attached",
        "single_family_small_lot",
        "single_family_large_lot"
      )) %>%
      filter(inventory_year == max(inventory_year)) %>%
      ungroup()
  )


  t_0 <- adj_unit_counts(
    res_tb = building_data$residential,
    .selected_ctu = "Minneapolis",
    .new_homes_to_multifamily_pct = 0.0
  ) %>%
    suppressWarnings() %>%
    dplyr::filter(sp_categories %in% c(
      "multifamily_units",
      "single_family_units",
      "single_family_attached",
      "single_family_small_lot",
      "single_family_large_lot"
    )) %>%
    filter(inventory_year == max(inventory_year)) %>%
    ungroup()

  bau_forecast <- building_data$residential %>%
    filter(
      geog_name == "Minneapolis"
    ) %>%
    suppressWarnings() %>%
    dplyr::filter(sp_categories %in% c(
      "multifamily_units",
      "single_family_units",
      "single_family_attached",
      "single_family_small_lot",
      "single_family_large_lot"
    )) %>%
    filter(inventory_year == max(inventory_year)) %>%
    ungroup()

  # minneapolis has no effect, because they are already
  # projected to reduce the number of SF housing units
  testthat::expect_equal(t_0, bau_forecast)

  testthat::expect_equal(t_50, expected_mpls_res_tb)


  testthat::expect_equal(
    # test that total number of housing units is consistent
    bau_forecast %>%
      dplyr::filter(sp_categories %in% c(
        "multifamily_units",
        "single_family_units",
        "single_family_attached",
        "single_family_small_lot",
        "single_family_large_lot"
      )) %>%
      filter(inventory_year == max(inventory_year)) %>%
      ungroup() %>%
      group_by(geog_name, geog_id, inventory_year) %>%
      summarize(
        value = sum(value),
        value_change_from_base = sum(value_change_from_base), .groups = "keep"
      ),
    t_50 %>%
      dplyr::filter(sp_categories %in% c(
        "multifamily_units",
        "single_family_units",
        "single_family_attached",
        "single_family_small_lot",
        "single_family_large_lot"
      )) %>%
      filter(inventory_year == max(inventory_year)) %>%
      ungroup() %>%
      group_by(geog_name, geog_id, inventory_year) %>%
      summarize(
        value = sum(value),
        value_change_from_base = sum(value_change_from_base), .groups = "keep"
      )
  )
})





test_that("Coon Rapids unit counts", {
  t_50 <- adj_unit_counts(
    res_tb = building_data$residential,
    .selected_ctu = "Coon Rapids",
    .new_homes_to_multifamily_pct = 0.50
  ) %>%
    suppressWarnings() %>%
    dplyr::filter(sp_categories %in% c(
      "multifamily_units",
      "single_family_units",
      "single_family_attached",
      "single_family_small_lot",
      "single_family_large_lot"
    )) %>%
    filter(inventory_year == max(inventory_year)) %>%
    ungroup()

  expected_res_tb <- tibble::tribble(
    ~geog_name, ~geog_id, ~geog_id_type, ~sp_categories, ~geog_level, ~inventory_year, ~value, ~value_change_from_base,
    "Coon Rapids", "02393628", "ctu_gnis", "multifamily_units", "CITY", 2050, 9563.85, 3705.95,
    "Coon Rapids", "02393628", "ctu_gnis", "single_family_attached", "CITY", 2050, 2618.1, 199.5,
    "Coon Rapids", "02393628", "ctu_gnis", "single_family_large_lot", "CITY", 2050, 556.383333333333, -18.3166666666667,
    "Coon Rapids", "02393628", "ctu_gnis", "single_family_small_lot", "CITY", 2050, 14482.6666666667, -1906.33333333333
  )%>%
    select(-geog_level, -geog_id_type)

  testthat::expect_equal(t_50, expected_res_tb)

  testthat::expect_lt(
    t_50 %>%
      filter(sp_categories == "single_family_small_lot") %>%
      pull("value_change_from_base"),
    0
  )


  testthat::expect_warning(
    adj_unit_counts(
      res_tb = building_data$residential,
      .selected_ctu = "Coon Rapids",
      .new_homes_to_multifamily_pct = 0.0
    ) %>%
      dplyr::filter(sp_categories %in% c(
        "multifamily_units",
        "single_family_units",
        "single_family_attached",
        "single_family_small_lot",
        "single_family_large_lot"
      )) %>%
      filter(inventory_year == max(inventory_year)) %>%
      ungroup()
  )


  t_0 <- adj_unit_counts(
    res_tb = building_data$residential,
    .selected_ctu = "Coon Rapids",
    .new_homes_to_multifamily_pct = 0.0
  ) %>%
    suppressWarnings() %>%
    dplyr::filter(sp_categories %in% c(
      "multifamily_units",
      "single_family_units",
      "single_family_attached",
      "single_family_small_lot",
      "single_family_large_lot"
    )) %>%
    filter(inventory_year == max(inventory_year)) %>%
    ungroup()

  bau_forecast <- building_data$residential %>%
    filter(
      geog_name == "Coon Rapids"
    ) %>%
    suppressWarnings() %>%
    dplyr::filter(sp_categories %in% c(
      "multifamily_units",
      "single_family_units",
      "single_family_attached",
      "single_family_small_lot",
      "single_family_large_lot"
    )) %>%
    filter(inventory_year == max(inventory_year)) %>%
    ungroup()

  # Coon Rapids has no effect, because they are already
  # projected to reduce the number of SF housing units
  testthat::expect_equal(t_0, bau_forecast)

  testthat::expect_equal(
    # test that total number of housing units is consistent
    bau_forecast %>%
      dplyr::filter(sp_categories %in% c(
        "multifamily_units",
        "single_family_units",
        "single_family_attached",
        "single_family_small_lot",
        "single_family_large_lot"
      )) %>%
      filter(inventory_year == max(inventory_year)) %>%
      ungroup() %>%
      group_by(geog_name, geog_id, inventory_year) %>%
      summarize(
        value = sum(value),
        value_change_from_base = sum(value_change_from_base), .groups = "keep"
      ),
    t_50 %>%
      dplyr::filter(sp_categories %in% c(
        "multifamily_units",
        "single_family_units",
        "single_family_attached",
        "single_family_small_lot",
        "single_family_large_lot"
      )) %>%
      filter(inventory_year == max(inventory_year)) %>%
      ungroup() %>%
      group_by(geog_name, geog_id, inventory_year) %>%
      summarize(
        value = sum(value),
        value_change_from_base = sum(value_change_from_base), .groups = "keep"
      )
  )
})



test_that("Champlin unit counts", {
  t_50 <- adj_unit_counts(
    res_tb = building_data$residential,
    .selected_ctu = "Champlin",
    .new_homes_to_multifamily_pct = 0.50
  ) %>%
    suppressWarnings() %>%
    dplyr::filter(sp_categories %in% c(
      "multifamily_units",
      "single_family_units",
      "single_family_attached",
      "single_family_small_lot",
      "single_family_large_lot"
    )) %>%
    filter(inventory_year == max(inventory_year)) %>%
    ungroup()

  expected_res_tb <- tibble::tribble(
    ~geog_name,   ~geog_id, ~geog_id_type,            ~sp_categories, ~geog_level, ~inventory_year,           ~value, ~value_change_from_base,
    "Champlin", "02393797",    "ctu_gnis",       "multifamily_units",      "CITY",            2050, 2025.53333333333,        141.233333333333,
    "Champlin", "02393797",    "ctu_gnis",  "single_family_attached",      "CITY",            2050, 1209.63333333333,        11.8333333333334,
    "Champlin", "02393797",    "ctu_gnis", "single_family_large_lot",      "CITY",            2050,            502.3,                    28.5,
    "Champlin", "02393797",    "ctu_gnis", "single_family_small_lot",      "CITY",            2050, 6060.53333333333,       -255.266666666667
  )%>%
    select(-geog_level, -geog_id_type)

  testthat::expect_equal(t_50, expected_res_tb)

  testthat::expect_lt(
    t_50 %>%
      filter(sp_categories == "single_family_small_lot") %>%
      pull("value_change_from_base"),
    0
  )


  testthat::expect_warning(
    adj_unit_counts(
      res_tb = building_data$residential,
      .selected_ctu = "Champlin",
      .new_homes_to_multifamily_pct = 0.0
    ) %>%
      dplyr::filter(sp_categories %in% c(
        "multifamily_units",
        "single_family_units",
        "single_family_attached",
        "single_family_small_lot",
        "single_family_large_lot"
      )) %>%
      filter(inventory_year == max(inventory_year)) %>%
      ungroup()
  )


  t_0 <- adj_unit_counts(
    res_tb = building_data$residential,
    .selected_ctu = "Champlin",
    .new_homes_to_multifamily_pct = 0.0
  ) %>%
    suppressWarnings() %>%
    dplyr::filter(sp_categories %in% c(
      "multifamily_units",
      "single_family_units",
      "single_family_attached",
      "single_family_small_lot",
      "single_family_large_lot"
    )) %>%
    filter(inventory_year == max(inventory_year)) %>%
    ungroup()

  bau_forecast <- building_data$residential %>%
    filter(
      geog_name == "Champlin"
    ) %>%
    suppressWarnings() %>%
    dplyr::filter(sp_categories %in% c(
      "multifamily_units",
      "single_family_units",
      "single_family_attached",
      "single_family_small_lot",
      "single_family_large_lot"
    )) %>%
    filter(inventory_year == max(inventory_year)) %>%
    ungroup()

  # Champlin has no effect, because they are already
  # projected to reduce the number of SF housing units
  testthat::expect_equal(t_0, bau_forecast)

  testthat::expect_equal(
    # test that total number of housing units is consistent
    bau_forecast %>%
      dplyr::filter(sp_categories %in% c(
        "multifamily_units",
        "single_family_units",
        "single_family_attached",
        "single_family_small_lot",
        "single_family_large_lot"
      )) %>%
      filter(inventory_year == max(inventory_year)) %>%
      ungroup() %>%
      group_by(geog_name, geog_id, inventory_year) %>%
      summarize(
        value = sum(value),
        value_change_from_base = sum(value_change_from_base), .groups = "keep"
      ),
    t_50 %>%
      dplyr::filter(sp_categories %in% c(
        "multifamily_units",
        "single_family_units",
        "single_family_attached",
        "single_family_small_lot",
        "single_family_large_lot"
      )) %>%
      filter(inventory_year == max(inventory_year)) %>%
      ungroup() %>%
      group_by(geog_name, geog_id, inventory_year) %>%
      summarize(
        value = sum(value),
        value_change_from_base = sum(value_change_from_base), .groups = "keep"
      )
  )
})


test_that("Deephaven unit counts", {
  t_50 <- adj_unit_counts(
    res_tb = building_data$residential,
    .selected_ctu = "Deephaven",
    .new_homes_to_multifamily_pct = 0.50
  ) %>%
    suppressWarnings() %>%
    dplyr::filter(sp_categories %in% c(
      "multifamily_units",
      "single_family_units",
      "single_family_attached",
      "single_family_small_lot",
      "single_family_large_lot"
    )) %>%
    filter(inventory_year == max(inventory_year)) %>%
    ungroup()

  expected_res_tb <- tibble::tribble(
    ~geog_name, ~geog_id, ~geog_id_type, ~sp_categories, ~geog_level, ~inventory_year, ~value, ~value_change_from_base,
    "Deephaven", "02394486", "ctu_gnis", "multifamily_units", "CITY", 2050, 161.733333333333, -17.3666666666667,
    "Deephaven", "02394486", "ctu_gnis", "single_family_attached", "CITY", 2050, 74.25, -12.25,
    "Deephaven", "02394486", "ctu_gnis", "single_family_large_lot", "CITY", 2050, 373.05, -48.65,
    "Deephaven", "02394486", "ctu_gnis", "single_family_small_lot", "CITY", 2050, 844.966666666667, 5.16666666666671
  )%>%
    select(-geog_level, -geog_id_type)


  testthat::expect_equal(t_50, expected_res_tb)

  # expect number of MF units to still decrease
  testthat::expect_lt(
    t_50 %>%
      filter(sp_categories == "multifamily_units") %>%
      pull("value_change_from_base"),
    0
  )


  testthat::expect_warning(
    adj_unit_counts(
      res_tb = building_data$residential,
      .selected_ctu = "Deephaven",
      .new_homes_to_multifamily_pct = 0.0
    ) %>%
      dplyr::filter(sp_categories %in% c(
        "multifamily_units",
        "single_family_units",
        "single_family_attached",
        "single_family_small_lot",
        "single_family_large_lot"
      )) %>%
      filter(inventory_year == max(inventory_year)) %>%
      ungroup()
  )


  t_0 <- adj_unit_counts(
    res_tb = building_data$residential,
    .selected_ctu = "Deephaven",
    .new_homes_to_multifamily_pct = 0.0
  ) %>%
    suppressWarnings() %>%
    dplyr::filter(sp_categories %in% c(
      "multifamily_units",
      "single_family_units",
      "single_family_attached",
      "single_family_small_lot",
      "single_family_large_lot"
    )) %>%
    filter(inventory_year == max(inventory_year)) %>%
    ungroup()

  bau_forecast <- building_data$residential %>%
    filter(
      geog_name == "Deephaven"
    ) %>%
    suppressWarnings() %>%
    dplyr::filter(sp_categories %in% c(
      "multifamily_units",
      "single_family_units",
      "single_family_attached",
      "single_family_small_lot",
      "single_family_large_lot"
    )) %>%
    filter(inventory_year == max(inventory_year)) %>%
    ungroup()

  #  has no effect, because they are already
  # projected to reduce the number of SF housing units
  testthat::expect_equal(t_0, bau_forecast)

  testthat::expect_equal(
    # test that total number of housing units is consistent
    bau_forecast %>%
      dplyr::filter(sp_categories %in% c(
        "multifamily_units",
        "single_family_units",
        "single_family_attached",
        "single_family_small_lot",
        "single_family_large_lot"
      )) %>%
      filter(inventory_year == max(inventory_year)) %>%
      ungroup() %>%
      group_by(geog_name, geog_id, inventory_year) %>%
      summarize(
        value = sum(value),
        value_change_from_base = sum(value_change_from_base), .groups = "keep"
      ),
    t_50 %>%
      dplyr::filter(sp_categories %in% c(
        "multifamily_units",
        "single_family_units",
        "single_family_attached",
        "single_family_small_lot",
        "single_family_large_lot"
      )) %>%
      filter(inventory_year == max(inventory_year)) %>%
      ungroup() %>%
      group_by(geog_name, geog_id, inventory_year) %>%
      summarize(
        value = sum(value),
        value_change_from_base = sum(value_change_from_base), .groups = "keep"
      )
  )
})
