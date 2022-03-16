base <- floor_area_growth(
  res_tb = lake_elmo_res,
  .single_family_floor_area_growth_pct = 0.05,
  .new_homes_affected_pct = 0.5,
  .enviro_factors = enviro_factors
)

testthat::expect_warning(floor_area_growth(
  res_tb = lake_elmo_res,
  .single_family_floor_area_growth_pct = 0,
  .new_homes_affected_pct = 0,
  .enviro_factors = enviro_factors
))

testthat::expect_equal(nrow(base), 22)
