# test-build_ramp_schedule.R

test_that("build_ramp_schedule returns correct structure", {
  ramp <- build_ramp_schedule(2030, 2040, 0.5, "test_pct")

  expect_s3_class(ramp, "tbl_df")
  expect_equal(nrow(ramp), 46) # 2005:2050
  expect_true("test_pct" %in% names(ramp))
  expect_true("emissions_year" %in% names(ramp))
})

test_that("ramp is zero before start, target after end, linear between", {
  ramp <- build_ramp_schedule(2030, 2040, 0.5, "pct")

  # zero before start
  pre <- ramp$pct[ramp$emissions_year < 2030]
  expect_true(all(pre == 0))

  # target after end
  post <- ramp$pct[ramp$emissions_year > 2040]
  expect_true(all(post == 0.5))

  # monotonically increasing during ramp
  during <- ramp$pct[ramp$emissions_year >= 2030 & ramp$emissions_year <= 2040]
  expect_equal(length(during), 11)
  expect_true(all(diff(during) >= 0))

  # first ramp year > 0, last ramp year == target
  expect_gt(during[1], 0)
  expect_equal(during[length(during)], 0.5)
})

test_that("ramp works at boundary: single-year ramp", {
  ramp <- build_ramp_schedule(2035, 2035, 0.8, "pct")

  expect_equal(ramp$pct[ramp$emissions_year == 2035], 0.8)
  expect_equal(ramp$pct[ramp$emissions_year == 2034], 0)
  expect_equal(ramp$pct[ramp$emissions_year == 2036], 0.8)
})

test_that("ramp with zero target is all zeros", {
  ramp <- build_ramp_schedule(2028, 2050, 0, "pct")
  expect_true(all(ramp$pct == 0))
})
