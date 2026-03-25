# Test County and CTU Aggregation ----
# These tests verify that transportation data aggregates correctly:
# 1. County-level totals should equal Region totals
# 2. CTU-level totals should equal Region totals
# 3. County TotStock/Sales/Exist should equal sum of fuel-specific types
#
# Tests cover:
# - Passenger: VMT, PMT, and vehicle counts (Stock, Sales, Exist) for PLDV mode
# - Freight: TMT (ton-miles traveled) and vehicle Stock for CUT and SUT modes
# All tests are performed across all years in the dataset.

testthat::test_that("County VMT totals equal Region VMT", {
  # Get region total VMT
  region_vmt <- transportation_data$passenger %>%
    filter(
      geog_name == "Twin Cities Region",
      var == "VMT",
      mode == "PLDV"
    ) %>%
    select(year, value) %>%
    rename(region_value = value)

  # Sum county VMT
  county_vmt_sum <- transportation_data$passenger %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level == "COUNTY",
      var == "VMT",
      mode == "PLDV"
    ) %>%
    group_by(year) %>%
    summarize(county_total = sum(value, na.rm = TRUE)) %>%
    ungroup()

  # Join and compare
  comparison <- region_vmt %>%
    left_join(county_vmt_sum, by = "year")

  # Test that county totals equal region totals with small tolerance for rounding
  testthat::expect_equal(
    comparison$region_value,
    comparison$county_total,
    tolerance = 1
  )
})


testthat::test_that("County PMT totals equal Region PMT", {
  # Get region total PMT for all modes with county aggregation
  region_pmt <- transportation_data$passenger %>%
    filter(
      geog_name == "Twin Cities Region",
      var == "PMT",
      mode %in% c("PLDV", "WALK", "BIKE", "BU", "BRT", "RU", "RI", "BS", "AT")
    ) %>%
    select(year, mode, value) %>%
    rename(region_value = value)

  # Sum county PMT
  county_pmt_sum <- transportation_data$passenger %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level == "COUNTY",
      var == "PMT",
      mode %in% c("PLDV", "WALK", "BIKE", "BU", "BRT", "RU", "RI", "BS", "AT")
    ) %>%
    group_by(year, mode) %>%
    summarize(county_total = sum(value, na.rm = TRUE), .groups = "keep") %>%
    ungroup()

  # Join and compare
  comparison <- region_pmt %>%
    left_join(county_pmt_sum, by = c("year", "mode"))

  # Test that county totals equal region totals with small tolerance for rounding
  testthat::expect_equal(
    comparison$region_value,
    comparison$county_total,
    tolerance = 10
  )
})


testthat::test_that("County vehicle Stock totals equal Region Stock", {
  # Get region total Stock values for different fuel types
  region_stock <- transportation_data$passenger %>%
    filter(
      geog_name == "Twin Cities Region",
      str_detect(var, "Stock"),
      mode == "PLDV"
    ) %>%
    select(year, var, value) %>%
    rename(region_value = value)

  # Sum county Stock values
  county_stock_sum <- transportation_data$passenger %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level == "COUNTY",
      str_detect(var, "Stock"),
      mode == "PLDV"
    ) %>%
    group_by(year, var) %>%
    summarize(county_total = sum(value, na.rm = TRUE), .groups = "keep")

  # Join and compare
  comparison <- region_stock %>%
    left_join(county_stock_sum, by = c("year", "var"))

  # Test that county totals equal region totals with tolerance for rounding
  testthat::expect_equal(
    comparison$region_value,
    comparison$county_total,
    tolerance = 1
  )
})


testthat::test_that("County vehicle Sales totals equal Region Sales", {
  # Get region total Sales values
  region_sales <- transportation_data$passenger %>%
    filter(
      geog_name == "Twin Cities Region",
      str_detect(var, "Sales"),
      mode == "PLDV"
    ) %>%
    select(year, var, value) %>%
    rename(region_value = value)

  # Sum county Sales values
  county_sales_sum <- transportation_data$passenger %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level == "COUNTY",
      str_detect(var, "Sales"),
      mode == "PLDV"
    ) %>%
    group_by(year, var) %>%
    summarize(county_total = sum(value, na.rm = TRUE), .groups = "keep")

  # Join and compare
  comparison <- region_sales %>%
    left_join(county_sales_sum, by = c("year", "var"))

  # Test that county totals equal region totals with tolerance for rounding
  testthat::expect_equal(
    comparison$region_value,
    comparison$county_total,
    tolerance = 1
  )
})


testthat::test_that("County vehicle Exist totals equal Region Exist", {
  # Get region total Exist values
  region_exist <- transportation_data$passenger %>%
    filter(
      geog_name == "Twin Cities Region",
      str_detect(var, "Exist"),
      mode == "PLDV"
    ) %>%
    select(year, var, value) %>%
    rename(region_value = value)

  # Sum county Exist values
  county_exist_sum <- transportation_data$passenger %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level == "COUNTY",
      str_detect(var, "Exist"),
      mode == "PLDV"
    ) %>%
    group_by(year, var) %>%
    summarize(county_total = sum(value, na.rm = TRUE), .groups = "drop")

  # Join and compare
  comparison <- region_exist %>%
    left_join(county_exist_sum, by = c("year", "var"))

  # Test that county totals equal region totals with tolerance for rounding
  testthat::expect_equal(
    comparison$region_value,
    comparison$county_total,
    tolerance = 1
  )
})


testthat::test_that("CTU VMT totals equal Region VMT", {
  # Get region total VMT
  region_vmt <- transportation_data$passenger %>%
    filter(
      geog_name == "Twin Cities Region",
      var == "VMT",
      mode == "PLDV"
    ) %>%
    select(year, value) %>%
    rename(region_value = value)

  # Sum CTU VMT
  ctu_vmt_sum <- transportation_data$passenger %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level %in% c("CITY", "TOWNSHIP", "UNORGANIZED TERRITORY"),
      var == "VMT",
      mode == "PLDV"
    ) %>%
    group_by(year) %>%
    summarize(ctu_total = sum(value, na.rm = TRUE)) %>%
    ungroup()

  # Join and compare
  comparison <- region_vmt %>%
    left_join(ctu_vmt_sum, by = "year")

  # Test that CTU totals equal region totals with small tolerance for rounding
  testthat::expect_equal(
    comparison$region_value,
    comparison$ctu_total,
    tolerance = 1
  )
})


testthat::test_that("CTU PMT totals equal Region PMT", {
  # Get region total PMT
  region_pmt <- transportation_data$passenger %>%
    filter(
      geog_name == "Twin Cities Region",
      var == "PMT",
      mode == "PLDV"
    ) %>%
    select(year, value) %>%
    rename(region_value = value)

  # Sum CTU PMT
  ctu_pmt_sum <- transportation_data$passenger %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level %in% c("CITY", "TOWNSHIP", "UNORGANIZED TERRITORY"),
      var == "PMT",
      mode == "PLDV"
    ) %>%
    group_by(year) %>%
    summarize(ctu_total = sum(value, na.rm = TRUE)) %>%
    ungroup()

  # Join and compare
  comparison <- region_pmt %>%
    left_join(ctu_pmt_sum, by = "year")

  # Test that CTU totals equal region totals with tolerance for rounding
  testthat::expect_equal(
    comparison$region_value,
    comparison$ctu_total,
    tolerance = 10
  )
})


testthat::test_that("CTU vehicle Stock totals equal Region Stock", {
  # Get region total Stock values
  region_stock <- transportation_data$passenger %>%
    filter(
      geog_name == "Twin Cities Region",
      str_detect(var, "Stock"),
      mode == "PLDV"
    ) %>%
    select(year, var, value) %>%
    rename(region_value = value)

  # Sum CTU Stock values
  ctu_stock_sum <- transportation_data$passenger %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level %in% c("CITY", "TOWNSHIP", "UNORGANIZED TERRITORY"),
      str_detect(var, "Stock"),
      mode == "PLDV"
    ) %>%
    group_by(year, var) %>%
    summarize(ctu_total = sum(value, na.rm = TRUE), .groups = "drop")

  # Join and compare
  comparison <- region_stock %>%
    left_join(ctu_stock_sum, by = c("year", "var"))

  # Test that CTU totals equal region totals with tolerance for rounding
  testthat::expect_equal(
    comparison$region_value,
    comparison$ctu_total,
    tolerance = 1
  )
})


testthat::test_that("CTU vehicle Sales totals equal Region Sales", {
  # Get region total Sales values
  region_sales <- transportation_data$passenger %>%
    filter(
      geog_name == "Twin Cities Region",
      str_detect(var, "Sales"),
      mode == "PLDV"
    ) %>%
    select(year, var, value) %>%
    rename(region_value = value)

  # Sum CTU Sales values
  ctu_sales_sum <- transportation_data$passenger %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level %in% c("CITY", "TOWNSHIP", "UNORGANIZED TERRITORY"),
      str_detect(var, "Sales"),
      mode == "PLDV"
    ) %>%
    group_by(year, var) %>%
    summarize(ctu_total = sum(value, na.rm = TRUE), .groups = "drop")

  # Join and compare
  comparison <- region_sales %>%
    left_join(ctu_sales_sum, by = c("year", "var"))

  # Test that CTU totals equal region totals with tolerance for rounding
  testthat::expect_equal(
    comparison$region_value,
    comparison$ctu_total,
    tolerance = 1
  )
})


testthat::test_that("CTU vehicle Exist totals equal Region Exist", {
  # Get region total Exist values
  region_exist <- transportation_data$passenger %>%
    filter(
      geog_name == "Twin Cities Region",
      str_detect(var, "Exist"),
      mode == "PLDV"
    ) %>%
    select(year, var, value) %>%
    rename(region_value = value)

  # Sum CTU Exist values
  ctu_exist_sum <- transportation_data$passenger %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level %in% c("CITY", "TOWNSHIP", "UNORGANIZED TERRITORY"),
      str_detect(var, "Exist"),
      mode == "PLDV"
    ) %>%
    group_by(year, var) %>%
    summarize(ctu_total = sum(value, na.rm = TRUE), .groups = "drop")

  # Join and compare
  comparison <- region_exist %>%
    left_join(ctu_exist_sum, by = c("year", "var"))

  # Test that CTU totals equal region totals with tolerance for rounding
  testthat::expect_equal(
    comparison$region_value,
    comparison$ctu_total,
    tolerance = 1
  )
})


testthat::test_that("County TotStock equals sum of fuel-specific Stock types", {
  # Get county TotStock values
  county_tot_stock <- transportation_data$passenger %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level == "COUNTY",
      var == "TotStock",
      mode == "PLDV"
    ) %>%
    select(geog_id, year, value) %>%
    rename(tot_stock = value)

  # Sum fuel-specific Stock values by county
  county_fuel_stock_sum <- transportation_data$passenger %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level == "COUNTY",
      str_detect(var, "Stock"),
      var != "TotStock",
      mode == "PLDV"
    ) %>%
    group_by(geog_id, year) %>%
    summarize(fuel_stock_sum = sum(value, na.rm = TRUE), .groups = "keep")

  # Join and compare
  comparison <- county_tot_stock %>%
    left_join(county_fuel_stock_sum, by = c("geog_id", "year"))

  # Test that TotStock equals sum of fuel-specific stocks
  testthat::expect_equal(
    comparison$tot_stock,
    comparison$fuel_stock_sum,
    tolerance = 0.1
  )
})


testthat::test_that("County data exists for all 7 counties", {
  county_geogs <- transportation_data$passenger %>%
    left_join(
      geog_index %>% select(geog_id, geog_level, geog_name),
      by = c("geog_id", "geog_name")
    ) %>%
    filter(geog_level == "COUNTY") %>%
    select(geog_name) %>%
    unique() %>%
    arrange(geog_name)

  expected_counties <- c(
    "Anoka County",
    "Carver County",
    "Dakota County",
    "Hennepin County",
    "Ramsey County",
    "Scott County",
    "Washington County"
  )

  testthat::expect_equal(
    county_geogs$geog_name,
    expected_counties
  )
})


# Freight TMT Tests ----

testthat::test_that("County freight TMT totals equal Region TMT for CUT", {
  # Get region total TMT for CUT (combination unit truck)
  region_tmt <- transportation_data$freight %>%
    filter(
      geog_name == "Twin Cities Region",
      var == "TMT",
      mode == "CUT"
    ) %>%
    select(year, value) %>%
    rename(region_value = value)

  # Sum county TMT for CUT
  county_tmt_sum <- transportation_data$freight %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level == "COUNTY",
      var == "TMT",
      mode == "CUT"
    ) %>%
    group_by(year) %>%
    summarize(county_total = sum(value, na.rm = TRUE)) %>%
    ungroup()

  # Join and compare
  comparison <- region_tmt %>%
    left_join(county_tmt_sum, by = "year")

  # Test that county totals equal region totals with tolerance for rounding
  testthat::expect_equal(
    comparison$region_value,
    comparison$county_total,
    tolerance = 10
  )
})


testthat::test_that("County freight TMT totals equal Region TMT for SUT", {
  # Get region total TMT for SUT (single unit truck)
  region_tmt <- transportation_data$freight %>%
    filter(
      geog_name == "Twin Cities Region",
      var == "TMT",
      mode == "SUT"
    ) %>%
    select(year, value) %>%
    rename(region_value = value)

  # Sum county TMT for SUT
  county_tmt_sum <- transportation_data$freight %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level == "COUNTY",
      var == "TMT",
      mode == "SUT"
    ) %>%
    group_by(year) %>%
    summarize(county_total = sum(value, na.rm = TRUE)) %>%
    ungroup()

  # Join and compare
  comparison <- region_tmt %>%
    left_join(county_tmt_sum, by = "year")

  # Test that county totals equal region totals with tolerance for rounding
  testthat::expect_equal(
    comparison$region_value,
    comparison$county_total,
    tolerance = 10
  )
})


testthat::test_that("CTU freight TMT totals equal Region TMT for CUT", {
  # Get region total TMT for CUT
  region_tmt <- transportation_data$freight %>%
    filter(
      geog_name == "Twin Cities Region",
      var == "TMT",
      mode == "CUT"
    ) %>%
    select(year, value) %>%
    rename(region_value = value)

  # Sum CTU TMT for CUT
  ctu_tmt_sum <- transportation_data$freight %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level %in% c("CITY", "TOWNSHIP", "UNORGANIZED TERRITORY"),
      var == "TMT",
      mode == "CUT"
    ) %>%
    group_by(year) %>%
    summarize(ctu_total = sum(value, na.rm = TRUE)) %>%
    ungroup()

  # Join and compare
  comparison <- region_tmt %>%
    left_join(ctu_tmt_sum, by = "year")

  # Test that CTU totals equal region totals with tolerance for rounding
  testthat::expect_equal(
    comparison$region_value,
    comparison$ctu_total,
    tolerance = 10
  )
})


testthat::test_that("CTU freight TMT totals equal Region TMT for SUT", {
  # Get region total TMT for SUT
  region_tmt <- transportation_data$freight %>%
    filter(
      geog_name == "Twin Cities Region",
      var == "TMT",
      mode == "SUT"
    ) %>%
    select(year, value) %>%
    rename(region_value = value)

  # Sum CTU TMT for SUT
  ctu_tmt_sum <- transportation_data$freight %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level %in% c("CITY", "TOWNSHIP", "UNORGANIZED TERRITORY"),
      var == "TMT",
      mode == "SUT"
    ) %>%
    group_by(year) %>%
    summarize(ctu_total = sum(value, na.rm = TRUE)) %>%
    ungroup()

  # Join and compare
  comparison <- region_tmt %>%
    left_join(ctu_tmt_sum, by = "year")

  # Test that CTU totals equal region totals with tolerance for rounding
  testthat::expect_equal(
    comparison$region_value,
    comparison$ctu_total,
    tolerance = 10
  )
})


testthat::test_that("County freight TotStock totals equal Region TotStock for CUT", {
  # Get region total TotStock for CUT
  region_stock <- transportation_data$freight %>%
    filter(
      geog_name == "Twin Cities Region",
      var == "TotStock",
      mode == "CUT"
    ) %>%
    select(year, value) %>%
    rename(region_value = value)

  # Sum county TotStock for CUT
  county_stock_sum <- transportation_data$freight %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level == "COUNTY",
      var == "TotStock",
      mode == "CUT"
    ) %>%
    group_by(year) %>%
    summarize(county_total = sum(value, na.rm = TRUE)) %>%
    ungroup()

  # Join and compare
  comparison <- region_stock %>%
    left_join(county_stock_sum, by = "year")

  # Test that county totals equal region totals with tolerance for rounding
  testthat::expect_equal(
    comparison$region_value,
    comparison$county_total,
    tolerance = 1
  )
})


testthat::test_that("County freight TotStock totals equal Region TotStock for SUT", {
  # Get region total TotStock for SUT
  region_stock <- transportation_data$freight %>%
    filter(
      geog_name == "Twin Cities Region",
      var == "TotStock",
      mode == "SUT"
    ) %>%
    select(year, value) %>%
    rename(region_value = value)

  # Sum county TotStock for SUT
  county_stock_sum <- transportation_data$freight %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level == "COUNTY",
      var == "TotStock",
      mode == "SUT"
    ) %>%
    group_by(year) %>%
    summarize(county_total = sum(value, na.rm = TRUE)) %>%
    ungroup()

  # Join and compare
  comparison <- region_stock %>%
    left_join(county_stock_sum, by = "year")

  # Test that county totals equal region totals with tolerance for rounding
  testthat::expect_equal(
    comparison$region_value,
    comparison$county_total,
    tolerance = 1
  )
})


testthat::test_that("County freight fuel-specific Stock totals equal Region for CUT", {
  # Get region fuel-specific Stock for CUT
  region_stock <- transportation_data$freight %>%
    filter(
      geog_name == "Twin Cities Region",
      str_detect(var, "Stock"),
      var != "TotStock",
      mode == "CUT"
    ) %>%
    select(year, var, value) %>%
    rename(region_value = value)

  # Sum county fuel-specific Stock for CUT
  county_stock_sum <- transportation_data$freight %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level == "COUNTY",
      str_detect(var, "Stock"),
      var != "TotStock",
      mode == "CUT"
    ) %>%
    group_by(year, var) %>%
    summarize(county_total = sum(value, na.rm = TRUE), .groups = "drop")

  # Join and compare
  comparison <- region_stock %>%
    left_join(county_stock_sum, by = c("year", "var"))

  # Test that county totals equal region totals with tolerance for rounding
  testthat::expect_equal(
    comparison$region_value,
    comparison$county_total,
    tolerance = 1
  )
})


testthat::test_that("CTU freight TotStock totals equal Region TotStock for CUT", {
  # Get region total TotStock for CUT
  region_stock <- transportation_data$freight %>%
    filter(
      geog_name == "Twin Cities Region",
      var == "TotStock",
      mode == "CUT"
    ) %>%
    select(year, value) %>%
    rename(region_value = value)

  # Sum CTU TotStock for CUT
  ctu_stock_sum <- transportation_data$freight %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level %in% c("CITY", "TOWNSHIP", "UNORGANIZED TERRITORY"),
      var == "TotStock",
      mode == "CUT"
    ) %>%
    group_by(year) %>%
    summarize(ctu_total = sum(value, na.rm = TRUE)) %>%
    ungroup()

  # Join and compare
  comparison <- region_stock %>%
    left_join(ctu_stock_sum, by = "year")

  # Test that CTU totals equal region totals with tolerance for rounding
  testthat::expect_equal(
    comparison$region_value,
    comparison$ctu_total,
    tolerance = 1
  )
})


# Parking Cost Tests ----

testthat::test_that("All counties have parking cost set to minimum observed value", {
  # Get minimum parking cost from all CTUs
  min_parking <- parking_cost %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(geog_level != "COUNTY") %>%
    pull(value) %>%
    min(na.rm = TRUE)

  # Get county parking data
  county_parking <- parking_cost %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(geog_level == "COUNTY")

  # Check that all 7 counties exist
  testthat::expect_equal(nrow(county_parking), 7)

  # Check that all values equal the minimum parking cost
  testthat::expect_true(all(county_parking$value == min_parking))

  # Check that all are PLDV mode
  testthat::expect_true(all(county_parking$mode == "PLDV"))

  # Check that all are PARK var
  testthat::expect_true(all(county_parking$var == "PARK"))
})
