# test-combustion_ef.R

test_that("combustion_ef has expected structure", {
  expect_s3_class(combustion_ef, "tbl_df")
  expect_equal(sort(names(combustion_ef)), sort(c("fuel_type", "mt_co2e_per_unit", "unit")))
  expect_equal(nrow(combustion_ef), 3)
})

test_that("combustion_ef contains all required fuel types", {
  expect_true(all(c("Natural Gas", "Propane", "Kerosene") %in% combustion_ef$fuel_type))
})

test_that("combustion_ef values are positive and reasonable", {
  expect_true(all(combustion_ef$mt_co2e_per_unit > 0))
  expect_true(all(combustion_ef$mt_co2e_per_unit < 1)) # all should be well under 1 mt per unit

  # natural gas ~ 0.0545 mt/mcf
  ef_ng <- combustion_ef$mt_co2e_per_unit[combustion_ef$fuel_type == "Natural Gas"]
  expect_equal(ef_ng, 0.0545, tolerance = 0.005)

  # propane ~ 0.0631 mt/mmbtu
  ef_prop <- combustion_ef$mt_co2e_per_unit[combustion_ef$fuel_type == "Propane"]
  expect_equal(ef_prop, 0.0631, tolerance = 0.005)
})

test_that("combustion_ef units match expected activity units", {
  units <- setNames(combustion_ef$unit, combustion_ef$fuel_type)
  expect_equal(units[["Natural Gas"]], "mcf")
  expect_equal(units[["Propane"]], "mmbtu")
  expect_equal(units[["Kerosene"]], "mmbtu")
})
