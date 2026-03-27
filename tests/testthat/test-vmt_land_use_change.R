testthat::test_that("Land use adjustment values over time are correct", {
  ten_pct <- vmt_land_use_change(
    .type = "DRIVE",
    .comb_5d_impact_pct_change = 0.25,
    .pop_dens_pct_change = 0.10,
    .emp_dens_pct_change = 0.10,
    .land_use_diversity_pct_change = 0.10,
    .intersection_design_pct_change = 0.10,
    .job_access_pct_change = 0.10,
    .transit_dist_pct_change = -0.10,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  testthat::expect_equal(
    ten_pct$land_use_adj,
    c(
      1, 1, 1, 0.982860992276631, 0.965942690805545, 0.94924318398327,
      0.932760568029827, 0.932760568029827, 0.932760568029827
    )
  )

  # 10% increase in pop dens  = -0.4% decrease in auto VMT
  walk <- vmt_land_use_change(
    .type = "WALK",
    .comb_5d_impact_pct_change = 0.25,
    .pop_dens_pct_change = 0.10,
    .emp_dens_pct_change = 0.1,
    .land_use_diversity_pct_change = 0.1,
    .intersection_design_pct_change = 0.1,
    .job_access_pct_change = 0.1,
    .transit_dist_pct_change = -0.1,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  testthat::expect_equal(
    walk$land_use_adj,
    c(
      1, 1, 1, 1.00797947847595, 1.01591732862345, 1.02381267438514,
      1.03166464223698, 1.03166464223698, 1.03166464223698
    )
  )


  transit <- vmt_land_use_change(
    .type = "TRANSIT",
    .comb_5d_impact_pct_change = 0.25,
    .pop_dens_pct_change = 0.10,
    .emp_dens_pct_change = 0.1,
    .land_use_diversity_pct_change = 0.1,
    .intersection_design_pct_change = 0.1,
    .job_access_pct_change = 0.1,
    .transit_dist_pct_change = -0.1,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )


  testthat::expect_equal(
    transit$land_use_adj,
    c(
      1, 1, 1, 1.02379609142689, 1.04778077081956, 1.07194850845425,
      1.09629358954158, 1.09629358954158, 1.09629358954158
    )
  )


  # test error -----

  testthat::expect_error(vmt_land_use_change(
    .mode = "PLDV",
    .type = "scoot",
    .comb_5d_impact_pct_change = 0.25,
    .pop_dens_pct_change = 0.10,
    .emp_dens_pct_change = 0.1,
    .land_use_diversity_pct_change = 0.1,
    .intersection_design_pct_change = 0.1,
    .job_access_pct_change = 0.1,
    .transit_dist_pct_change = -0.1,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  ))
})


testthat::test_that("DRIVE mode with varying density changes", {
  # 5% density change
  drive_5pct <- vmt_land_use_change(
    .type = "DRIVE",
    .comb_5d_impact_pct_change = 0.25,
    .pop_dens_pct_change = 0.05,
    .emp_dens_pct_change = 0.05,
    .land_use_diversity_pct_change = 0.05,
    .intersection_design_pct_change = 0.05,
    .job_access_pct_change = 0.05,
    .transit_dist_pct_change = -0.05,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  # 20% density change
  drive_20pct <- vmt_land_use_change(
    .type = "DRIVE",
    .comb_5d_impact_pct_change = 0.25,
    .pop_dens_pct_change = 0.20,
    .emp_dens_pct_change = 0.20,
    .land_use_diversity_pct_change = 0.20,
    .intersection_design_pct_change = 0.20,
    .job_access_pct_change = 0.20,
    .transit_dist_pct_change = -0.20,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  # Higher density changes should produce lower VMT adjustment (more reduction)
  testthat::expect_lt(
    drive_20pct$land_use_adj[9],
    drive_5pct$land_use_adj[9]
  )

  # Early years should have no adjustment
  testthat::expect_equal(
    drive_5pct$land_use_adj[1:3],
    c(1, 1, 1)
  )
})


testthat::test_that("DRIVE mode with population density only", {
  drive_pop_only <- vmt_land_use_change(
    .type = "DRIVE",
    .comb_5d_impact_pct_change = 0,
    .pop_dens_pct_change = 0.15,
    .emp_dens_pct_change = 0,
    .land_use_diversity_pct_change = 0,
    .intersection_design_pct_change = 0,
    .job_access_pct_change = 0,
    .transit_dist_pct_change = 0,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  # Should show reduction in later years
  testthat::expect_lt(drive_pop_only$land_use_adj[9], 1)

  # Baseline years remain at 1
  testthat::expect_equal(drive_pop_only$land_use_adj[1:3], c(1, 1, 1))
})


testthat::test_that("WALK mode with varying density changes", {
  # 10% density change
  walk_10pct <- vmt_land_use_change(
    .type = "WALK",
    .comb_5d_impact_pct_change = 0.25,
    .pop_dens_pct_change = 0.10,
    .emp_dens_pct_change = 0.10,
    .land_use_diversity_pct_change = 0.10,
    .intersection_design_pct_change = 0.10,
    .job_access_pct_change = 0.10,
    .transit_dist_pct_change = -0.10,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  # 25% density change
  walk_25pct <- vmt_land_use_change(
    .type = "WALK",
    .comb_5d_impact_pct_change = 0.25,
    .pop_dens_pct_change = 0.25,
    .emp_dens_pct_change = 0.25,
    .land_use_diversity_pct_change = 0.25,
    .intersection_design_pct_change = 0.25,
    .job_access_pct_change = 0.25,
    .transit_dist_pct_change = -0.25,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  # Higher density changes should produce higher walk adjustment (more walking)
  testthat::expect_gt(
    walk_25pct$land_use_adj[9],
    walk_10pct$land_use_adj[9]
  )

  # Walk adjustment should be greater than 1 (increase in walking)
  testthat::expect_gt(walk_10pct$land_use_adj[9], 1)
  testthat::expect_gt(walk_25pct$land_use_adj[9], 1)
})


testthat::test_that("WALK mode with employment density only", {
  walk_emp_only <- vmt_land_use_change(
    .type = "WALK",
    .comb_5d_impact_pct_change = 0,
    .pop_dens_pct_change = 0,
    .emp_dens_pct_change = 0.20,
    .land_use_diversity_pct_change = 0,
    .intersection_design_pct_change = 0,
    .job_access_pct_change = 0,
    .transit_dist_pct_change = 0,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  # Should show increase in walk trips in later years
  testthat::expect_gt(walk_emp_only$land_use_adj[9], 1)

  # Baseline years remain at 1
  testthat::expect_equal(walk_emp_only$land_use_adj[1:3], c(1, 1, 1))
})


testthat::test_that("TRANSIT mode with varying density changes", {
  # 10% density change
  transit_10pct <- vmt_land_use_change(
    .type = "TRANSIT",
    .comb_5d_impact_pct_change = 0.25,
    .pop_dens_pct_change = 0.10,
    .emp_dens_pct_change = 0.10,
    .land_use_diversity_pct_change = 0.10,
    .intersection_design_pct_change = 0.10,
    .job_access_pct_change = 0.10,
    .transit_dist_pct_change = -0.10,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  # 30% density change
  transit_30pct <- vmt_land_use_change(
    .type = "TRANSIT",
    .comb_5d_impact_pct_change = 0.25,
    .pop_dens_pct_change = 0.30,
    .emp_dens_pct_change = 0.30,
    .land_use_diversity_pct_change = 0.30,
    .intersection_design_pct_change = 0.30,
    .job_access_pct_change = 0.30,
    .transit_dist_pct_change = -0.30,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  # Higher density changes should produce higher transit adjustment (more transit use)
  testthat::expect_gt(
    transit_30pct$land_use_adj[9],
    transit_10pct$land_use_adj[9]
  )

  # Transit adjustment should be greater than 1 (increase in transit)
  testthat::expect_gt(transit_10pct$land_use_adj[9], 1)
  testthat::expect_gt(transit_30pct$land_use_adj[9], 1)
})


testthat::test_that("TRANSIT mode with transit distance improvement only", {
  transit_dist_only <- vmt_land_use_change(
    .type = "TRANSIT",
    .comb_5d_impact_pct_change = 0,
    .pop_dens_pct_change = 0,
    .emp_dens_pct_change = 0,
    .land_use_diversity_pct_change = 0,
    .intersection_design_pct_change = 0,
    .job_access_pct_change = 0,
    .transit_dist_pct_change = -0.25,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  # Transit distance change alone affects transit adjustment
  testthat::expect_false(all(transit_dist_only$land_use_adj[4:9] == 1))

  # Baseline years remain at 1
  testthat::expect_equal(transit_dist_only$land_use_adj[1:3], c(1, 1, 1))
})


testthat::test_that("All modes with combined 5D impact", {
  # Test with high combined 5D impact
  drive_combined <- vmt_land_use_change(
    .type = "DRIVE",
    .comb_5d_impact_pct_change = 0.50,
    .pop_dens_pct_change = 0.15,
    .emp_dens_pct_change = 0.15,
    .land_use_diversity_pct_change = 0.15,
    .intersection_design_pct_change = 0.15,
    .job_access_pct_change = 0.15,
    .transit_dist_pct_change = -0.15,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  walk_combined <- vmt_land_use_change(
    .type = "WALK",
    .comb_5d_impact_pct_change = 0.50,
    .pop_dens_pct_change = 0.15,
    .emp_dens_pct_change = 0.15,
    .land_use_diversity_pct_change = 0.15,
    .intersection_design_pct_change = 0.15,
    .job_access_pct_change = 0.15,
    .transit_dist_pct_change = -0.15,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  transit_combined <- vmt_land_use_change(
    .type = "TRANSIT",
    .comb_5d_impact_pct_change = 0.50,
    .pop_dens_pct_change = 0.15,
    .emp_dens_pct_change = 0.15,
    .land_use_diversity_pct_change = 0.15,
    .intersection_design_pct_change = 0.15,
    .job_access_pct_change = 0.15,
    .transit_dist_pct_change = -0.15,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  # DRIVE should decrease, WALK and TRANSIT should increase
  testthat::expect_lt(drive_combined$land_use_adj[9], 1)
  testthat::expect_gt(walk_combined$land_use_adj[9], 1)
  testthat::expect_gt(transit_combined$land_use_adj[9], 1)
})


testthat::test_that("Zero changes produce no adjustment", {
  drive_zero <- vmt_land_use_change(
    .type = "DRIVE",
    .comb_5d_impact_pct_change = 0,
    .pop_dens_pct_change = 0,
    .emp_dens_pct_change = 0,
    .land_use_diversity_pct_change = 0,
    .intersection_design_pct_change = 0,
    .job_access_pct_change = 0,
    .transit_dist_pct_change = 0,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  walk_zero <- vmt_land_use_change(
    .type = "WALK",
    .comb_5d_impact_pct_change = 0,
    .pop_dens_pct_change = 0,
    .emp_dens_pct_change = 0,
    .land_use_diversity_pct_change = 0,
    .intersection_design_pct_change = 0,
    .job_access_pct_change = 0,
    .transit_dist_pct_change = 0,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  transit_zero <- vmt_land_use_change(
    .type = "TRANSIT",
    .comb_5d_impact_pct_change = 0,
    .pop_dens_pct_change = 0,
    .emp_dens_pct_change = 0,
    .land_use_diversity_pct_change = 0,
    .intersection_design_pct_change = 0,
    .job_access_pct_change = 0,
    .transit_dist_pct_change = 0,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  # All adjustments should be 1 (no change) for all years
  testthat::expect_equal(drive_zero$land_use_adj, rep(1, 9))
  testthat::expect_equal(walk_zero$land_use_adj, rep(1, 9))
  testthat::expect_equal(transit_zero$land_use_adj, rep(1, 9))
})
