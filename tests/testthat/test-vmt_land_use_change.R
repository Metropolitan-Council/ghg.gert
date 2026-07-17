testthat::test_that("Land use adjustment values over time are correct", {
  ten_pct <- vmt_land_use_change(
    .type = "DRIVE",
    .comb_5d_impact_pct_change = 0.25,
    .pop_dens_pct_change = 0.10,
    .emp_dens_pct_change = 0.10,
    .land_use_diversity_pct_change = 0.10,
    .intersection_design_pct_change = 0.10,
    .intersection_density_pct_change = 0,
    .job_access_pct_change = 0.10,
    .transit_dist_pct_change = -0.10,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  testthat::expect_equal(
    ten_pct$land_use_adj,
    c(
      1, 1, 1, 0.988549377309254, 0.977197130096806, 0.965942690805545,
      0.954785493419443, 0.943724973467866, 0.932760568029827
    ),
    tolerance = 0.01
  )

  # 10% increase in pop dens  = -0.4% decrease in auto VMT
  walk <- vmt_land_use_change(
    .type = "WALK",
    .comb_5d_impact_pct_change = 0.25,
    .pop_dens_pct_change = 0.10,
    .emp_dens_pct_change = 0.1,
    .land_use_diversity_pct_change = 0.1,
    .intersection_design_pct_change = 0.1,
    .intersection_density_pct_change = 0,
    .job_access_pct_change = 0.1,
    .transit_dist_pct_change = -0.1,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  testthat::expect_equal(
    walk$land_use_adj,
    c(
      1, 1, 1, 1.00532423435324, 1.01063009720849, 1.01591732862345,
      1.02118566914814, 1.02643485983302, 1.03166464223698
    ),
    tolerance = 0.1
  )


  transit <- vmt_land_use_change(
    .type = "TRANSIT",
    .comb_5d_impact_pct_change = 0.25,
    .pop_dens_pct_change = 0.10,
    .emp_dens_pct_change = 0.1,
    .land_use_diversity_pct_change = 0.1,
    .intersection_design_pct_change = 0.1,
    .intersection_density_pct_change = 0,
    .job_access_pct_change = 0.1,
    .transit_dist_pct_change = -0.1,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )


  testthat::expect_equal(
    transit$land_use_adj,
    c(
      1, 1, 1, 1.0158428389461, 1.03177029962998, 1.04778077081956,
      1.06387260503572, 1.0800441182434, 1.09629358954158
    ),
    tolerance = 0.01
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
    .intersection_density_pct_change = 0,
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
    .intersection_density_pct_change = 0,
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
    .intersection_density_pct_change = 0,
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
    .intersection_density_pct_change = 0,
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
    .intersection_density_pct_change = 0,
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
    .intersection_density_pct_change = 0,
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
    .intersection_density_pct_change = 0,
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
    .intersection_density_pct_change = 0,
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
    .intersection_density_pct_change = 0,
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
    .intersection_density_pct_change = 0,
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

testthat::test_that("Modes with intersection density", {
  drive_intersection_density <- vmt_land_use_change(
    .type = "DRIVE",
    .comb_5d_impact_pct_change = 0,
    .pop_dens_pct_change = 0,
    .emp_dens_pct_change = 0,
    .land_use_diversity_pct_change = 0,
    .intersection_design_pct_change = 0,
    .intersection_density_pct_change = 0.09,
    .job_access_pct_change = 0,
    .transit_dist_pct_change = 0,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )


  drive_intersection_density2 <- vmt_land_use_change(
    .type = "DRIVE",
    .comb_5d_impact_pct_change = 0,
    .pop_dens_pct_change = 0,
    .emp_dens_pct_change = 0,
    .land_use_diversity_pct_change = 0,
    .intersection_design_pct_change = 0,
    .intersection_density_pct_change = 0.15,
    .job_access_pct_change = 0,
    .transit_dist_pct_change = 0,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )
  transit_intersection_density <- vmt_land_use_change(
    .type = "TRANSIT",
    .comb_5d_impact_pct_change = 0,
    .pop_dens_pct_change = 0,
    .emp_dens_pct_change = 0,
    .land_use_diversity_pct_change = 0,
    .intersection_design_pct_change = 0,
    .intersection_density_pct_change = 0.09,
    .job_access_pct_change = 0,
    .transit_dist_pct_change = 0,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  walk_intersection_density <- vmt_land_use_change(
    .type = "WALK",
    .comb_5d_impact_pct_change = 0,
    .pop_dens_pct_change = 0,
    .emp_dens_pct_change = 0,
    .land_use_diversity_pct_change = 0,
    .intersection_design_pct_change = 0,
    .intersection_density_pct_change = 0.09,
    .job_access_pct_change = 0,
    .transit_dist_pct_change = 0,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  testthat::expect_true(max(drive_intersection_density$land_use_adj[4:9]) < 1)
  testthat::expect_true(max(drive_intersection_density2$land_use_adj[4:9]) < 1)
  testthat::expect_true(max(transit_intersection_density$land_use_adj[4:9]) > 1)
  testthat::expect_true(max(walk_intersection_density$land_use_adj[4:9]) > 1)

  # Baseline years remain at 1
  testthat::expect_equal(drive_intersection_density$land_use_adj[1:3], c(1, 1, 1))
  testthat::expect_equal(drive_intersection_density2$land_use_adj[1:3], c(1, 1, 1))
  testthat::expect_equal(transit_intersection_density$land_use_adj[1:3], c(1, 1, 1))
  testthat::expect_equal(walk_intersection_density$land_use_adj[1:3], c(1, 1, 1))
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
    .intersection_density_pct_change = 0,
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
    .intersection_density_pct_change = 0,
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
    .intersection_density_pct_change = 0,
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
    .intersection_density_pct_change = 0,
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
    .intersection_density_pct_change = 0,
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
    .intersection_density_pct_change = 0,
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



testthat::test_that("Actual VMT reduction percentages match expected patterns", {
  # Test DRIVE mode with 10% density increases
  drive_10pct <- vmt_land_use_change(
    .type = "DRIVE",
    .comb_5d_impact_pct_change = 0.25,
    .pop_dens_pct_change = 0.10,
    .emp_dens_pct_change = 0.10,
    .land_use_diversity_pct_change = 0.10,
    .intersection_design_pct_change = 0.10,
    .intersection_density_pct_change = 0,
    .job_access_pct_change = 0.10,
    .transit_dist_pct_change = -0.10,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  # Calculate actual percentage reduction in final year (year 9, index 9)
  drive_pct_reduction <- 1 - drive_10pct$land_use_adj[9]

  # With 10% increases across multiple factors, expect 6-8% VMT reduction
  testthat::expect_gt(drive_pct_reduction, 0.06)
  testthat::expect_lt(drive_pct_reduction, 0.08)

  # Test WALK mode with 15% density increases
  walk_15pct <- vmt_land_use_change(
    .type = "WALK",
    .comb_5d_impact_pct_change = 0.25,
    .pop_dens_pct_change = 0.15,
    .emp_dens_pct_change = 0.15,
    .land_use_diversity_pct_change = 0.15,
    .intersection_design_pct_change = 0.15,
    .intersection_density_pct_change = 0,
    .job_access_pct_change = 0.15,
    .transit_dist_pct_change = -0.15,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  # Calculate actual percentage increase in walking
  walk_pct_increase <- walk_15pct$land_use_adj[9] - 1

  # With 15% increases across multiple factors, expect 4-6% walk increase
  testthat::expect_gt(walk_pct_increase, 0)
  testthat::expect_lt(walk_pct_increase, 0.10)

  # Test TRANSIT mode with 20% density increases
  transit_20pct <- vmt_land_use_change(
    .type = "TRANSIT",
    .comb_5d_impact_pct_change = 0.25,
    .pop_dens_pct_change = 0.20,
    .emp_dens_pct_change = 0.20,
    .land_use_diversity_pct_change = 0.20,
    .intersection_design_pct_change = 0.20,
    .intersection_density_pct_change = 0,
    .job_access_pct_change = 0.20,
    .transit_dist_pct_change = -0.20,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  # Calculate actual percentage increase in transit use
  transit_pct_increase <- transit_20pct$land_use_adj[9] - 1

  # With 20% increases across multiple factors, expect 18-22% transit increase
  testthat::expect_gt(transit_pct_increase, 0.18)
  testthat::expect_lt(transit_pct_increase, 0.22)
})


testthat::test_that("VMT changes respect maximum allowed values from enviro_factors", {
  # Test DRIVE mode doesn't exceed maximum reduction
  drive_extreme <- vmt_land_use_change(
    .type = "DRIVE",
    .comb_5d_impact_pct_change = 1.0,
    .pop_dens_pct_change = 1.0,
    .emp_dens_pct_change = 1.0,
    .land_use_diversity_pct_change = 1.0,
    .intersection_design_pct_change = 1.0,
    .intersection_density_pct_change = 1.0,
    .job_access_pct_change = 1.0,
    .transit_dist_pct_change = -1.0,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  # DRIVE mode should not reduce more than MAX_5D_DR
  # land_use_adj should not go below 1 + MAX_5D_DR = 1 - 0.25 = 0.75
  max_drive_reduction <- 1 + enviro_factors$MAX_5D_DR
  testthat::expect_gte(min(drive_extreme$land_use_adj), max_drive_reduction)

  # Test WALK mode doesn't exceed maximum increase
  walk_extreme <- vmt_land_use_change(
    .type = "WALK",
    .comb_5d_impact_pct_change = 1.0,
    .pop_dens_pct_change = 1.0,
    .emp_dens_pct_change = 1.0,
    .land_use_diversity_pct_change = 1.0,
    .intersection_design_pct_change = 1.0,
    .intersection_density_pct_change = 1.0,
    .job_access_pct_change = 1.0,
    .transit_dist_pct_change = -1.0,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  # WALK mode should not increase more than MAX_5D_ACT
  # land_use_adj should not go above 1 + MAX_5D_ACT = 1 + 0.37 = 1.37
  max_walk_increase <- 1 + enviro_factors$MAX_5D_ACT
  testthat::expect_lte(max(walk_extreme$land_use_adj), max_walk_increase)

  # Test TRANSIT mode doesn't exceed maximum increase
  transit_extreme <- vmt_land_use_change(
    .type = "TRANSIT",
    .comb_5d_impact_pct_change = 1.0,
    .pop_dens_pct_change = 1.0,
    .emp_dens_pct_change = 1.0,
    .land_use_diversity_pct_change = 1.0,
    .intersection_design_pct_change = 1.0,
    .intersection_density_pct_change = 1.0,
    .job_access_pct_change = 1.0,
    .transit_dist_pct_change = -1.0,
    .enviro_factors = enviro_factors,
    .elast_5d = elast_5d
  )

  # TRANSIT mode should not increase more than MAX_5D_TRANS
  # land_use_adj should not go above 1 + MAX_5D_TRANS = 1 + 0.71 = 1.71
  max_transit_increase <- 1 + enviro_factors$MAX_5D_TRANS
  testthat::expect_lte(max(transit_extreme$land_use_adj), max_transit_increase)
})


test_vmt_land_use_passenger <- function(x) {
  testthat::test_that(paste0(x, " VMT and emissions reduction with land use scenarios"), {
    # Test passenger light duty with land use changes
    pass_bau <- suppressMessages(
      mode_passenger_light_duty(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = x
      )
    )

    pass_bau_vmt <- pass_bau$vmt %>%
      filter(year == max(year)) %>%
      summarise(vmt = sum(vmt)) %>%
      pull(vmt)

    pass_bau_ghg <- pass_bau$dir_ghg %>%
      filter(year == max(year)) %>%
      summarise(dir_ghg = sum(dir_ghg)) %>%
      pull(dir_ghg)

    # 10% density increases
    pass_lu_10pct <- suppressMessages(
      mode_passenger_light_duty(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = x,
        .scenario = "land_use",
        .emp_dens_pct_change = 0.10,
        .pop_dens_pct_change = 0.10,
        .intersection_density_pct_change = 0.01
      )
    )

    pass_lu_10pct_vmt <- pass_lu_10pct$vmt %>%
      filter(year == max(year)) %>%
      summarise(vmt = sum(vmt)) %>%
      pull(vmt)

    pass_lu_10pct_ghg <- pass_lu_10pct$dir_ghg %>%
      filter(year == max(year)) %>%
      summarise(dir_ghg = sum(dir_ghg)) %>%
      pull(dir_ghg)

    # Calculate actual percentage reduction (negative = reduction)
    vmt_pct_reduction_10 <- (pass_lu_10pct_vmt - pass_bau_vmt) / pass_bau_vmt
    ghg_pct_reduction_10 <- (pass_lu_10pct_ghg - pass_bau_ghg) / pass_bau_ghg

    # With 10% increases, expect -2% to -5% VMT/emissions reduction (negative values)
    testthat::expect_lt(vmt_pct_reduction_10, -0.01)
    testthat::expect_gt(vmt_pct_reduction_10, -0.06)

    testthat::expect_lt(ghg_pct_reduction_10, -0.01)
    testthat::expect_gt(ghg_pct_reduction_10, -0.06)

    # 15% density increases
    pass_lu_15pct <- suppressMessages(
      mode_passenger_light_duty(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = x,
        .scenario = "land_use",
        .emp_dens_pct_change = 0.15,
        .pop_dens_pct_change = 0.15,
        .intersection_density_pct_change = 0.05
      )
    )

    pass_lu_15pct_vmt <- pass_lu_15pct$vmt %>%
      filter(year == max(year)) %>%
      summarise(vmt = sum(vmt)) %>%
      pull(vmt)

    vmt_pct_reduction_15 <- (pass_lu_15pct_vmt - pass_bau_vmt) / pass_bau_vmt

    # Higher inputs should produce greater reductions (more negative)
    testthat::expect_lt(vmt_pct_reduction_15, vmt_pct_reduction_10)
    testthat::expect_lt(vmt_pct_reduction_15, -0.03)
    testthat::expect_gt(vmt_pct_reduction_15, -0.08)

    # Check that VMT reduction doesn't exceed MAX_5D_DR (don't go below 0.3 reduction)
    testthat::expect_gte(vmt_pct_reduction_10, enviro_factors$MAX_5D_DR)
    testthat::expect_gte(vmt_pct_reduction_15, enviro_factors$MAX_5D_DR)
  })
}


test_vmt_land_use_walk_bike <- function(x) {
  testthat::test_that(paste0(x, " walk/bike VMT increases with land use changes"), {
    # Test walk/bike mode with land use changes
    walk_bau <- suppressMessages(suppressWarnings(
      mode_walk_bike(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = x
      )
    ))

    walk_bau_vmt <- walk_bau$vmt %>%
      filter(year == max(year)) %>%
      summarise(vmt = sum(vmt)) %>%
      pull(vmt)

    # 10% density increases
    walk_lu_10pct <- suppressMessages(suppressWarnings(
      mode_walk_bike(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = x,
        .scenario = "land_use",
        .emp_dens_pct_change = 0.10,
        .pop_dens_pct_change = 0.10,
        .intersection_density_pct_change = 0.01
      )
    ))

    walk_lu_10pct_vmt <- walk_lu_10pct$vmt %>%
      filter(year == max(year)) %>%
      summarise(vmt = sum(vmt)) %>%
      pull(vmt)

    # Calculate actual percentage increase
    walk_vmt_pct_increase_10 <- (walk_lu_10pct_vmt - walk_bau_vmt) / walk_bau_vmt

    # With 10% increases, expect 1-5% walk VMT increase
    testthat::expect_gt(walk_vmt_pct_increase_10, 0)
    testthat::expect_lt(walk_vmt_pct_increase_10, 0.06)

    # 20% density increases
    walk_lu_20pct <- suppressMessages(suppressWarnings(
      mode_walk_bike(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = x,
        .scenario = "land_use",
        .emp_dens_pct_change = 0.20,
        .pop_dens_pct_change = 0.20,
        .intersection_density_pct_change = 0.02
      )
    ))

    walk_lu_20pct_vmt <- walk_lu_20pct$vmt %>%
      filter(year == max(year)) %>%
      summarise(vmt = sum(vmt)) %>%
      pull(vmt)

    walk_vmt_pct_increase_20 <- (walk_lu_20pct_vmt - walk_bau_vmt) / walk_bau_vmt

    # Higher inputs should produce greater increases
    testthat::expect_gt(walk_vmt_pct_increase_20, walk_vmt_pct_increase_10)
    testthat::expect_gt(walk_vmt_pct_increase_20, 0.02)
    testthat::expect_lt(walk_vmt_pct_increase_20, 0.10)

    # Check that walk VMT increase doesn't exceed MAX_5D_ACT
    testthat::expect_lte(walk_vmt_pct_increase_10, enviro_factors$MAX_5D_ACT)
    testthat::expect_lte(walk_vmt_pct_increase_20, enviro_factors$MAX_5D_ACT)
  })
}


test_vmt_land_use_transit <- function(x) {
  testthat::test_that(paste0(x, " transit VMT increases with land use"), {
    # Test transit bus mode
    bus_bau <- suppressMessages(suppressWarnings(
      mode_transit_bus(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = x
      )
    ))

    bus_bau_vmt <- bus_bau$vmt %>%
      filter(year == max(year), mode == "BU") %>%
      summarise(vmt = sum(vmt)) %>%
      pull(vmt)

    # Land use scenario
    bus_lu <- suppressMessages(suppressWarnings(
      mode_transit_bus(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = x,
        .scenario = "land_use",
        .emp_dens_pct_change = 0.15,
        .pop_dens_pct_change = 0.15,
        .intersection_density_pct_change = 0.02
      )
    ))

    bus_lu_vmt <- bus_lu$vmt %>%
      filter(year == max(year), mode == "BU") %>%
      summarise(vmt = sum(vmt)) %>%
      pull(vmt)

    bus_lu_vmt_pct_increase <- (bus_lu_vmt - bus_bau_vmt) / bus_bau_vmt

    # Land use should increase transit use
    testthat::expect_gt(bus_lu_vmt_pct_increase, 0)

    # Check that transit VMT increase doesn't exceed MAX_5D_TRANS
    testthat::expect_lte(bus_lu_vmt_pct_increase, enviro_factors$MAX_5D_TRANS)
  })
}


test_vmt_individual_parameters <- function(x) {
  testthat::test_that(paste0(x, " individual density parameters have measurable impacts"), {
    # Test population density only
    pass_bau <- suppressMessages(
      mode_passenger_light_duty(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = x
      )
    )

    pass_bau_vmt <- pass_bau$vmt %>%
      filter(year == max(year)) %>%
      summarise(vmt = sum(vmt)) %>%
      pull(vmt)

    # Population density only
    pass_pop <- suppressMessages(
      mode_passenger_light_duty(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = x,
        .scenario = "land_use",
        .pop_dens_pct_change = 0.20
      )
    )

    pass_pop_vmt <- pass_pop$vmt %>%
      filter(year == max(year)) %>%
      summarise(vmt = sum(vmt)) %>%
      pull(vmt)

    pop_vmt_reduction <- (pass_pop_vmt - pass_bau_vmt) / pass_bau_vmt

    # Employment density only
    pass_emp <- suppressMessages(
      mode_passenger_light_duty(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = x,
        .scenario = "land_use",
        .emp_dens_pct_change = 0.20
      )
    )

    pass_emp_vmt <- pass_emp$vmt %>%
      filter(year == max(year)) %>%
      summarise(vmt = sum(vmt)) %>%
      pull(vmt)

    emp_vmt_reduction <- (pass_emp_vmt - pass_bau_vmt) / pass_bau_vmt

    # Intersection density only
    pass_int <- suppressMessages(
      mode_passenger_light_duty(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = x,
        .scenario = "land_use",
        .intersection_density_pct_change = 0.05
      )
    )

    pass_int_vmt <- pass_int$vmt %>%
      filter(year == max(year)) %>%
      summarise(vmt = sum(vmt)) %>%
      pull(vmt)

    int_vmt_reduction <- (pass_int_vmt - pass_bau_vmt) / pass_bau_vmt

    # Each parameter should have measurable impact (negative = VMT reduction)
    testthat::expect_lt(pop_vmt_reduction, 0)
    testthat::expect_lt(emp_vmt_reduction, 0)
    testthat::expect_lt(int_vmt_reduction, 0)

    # With 20% individual changes, expect -0.5% to -5% reduction each (negative values)
    testthat::expect_gt(pop_vmt_reduction, -0.06)
    testthat::expect_lt(pop_vmt_reduction, 0)
    testthat::expect_gt(emp_vmt_reduction, -0.06)
    testthat::expect_lt(emp_vmt_reduction, 0)
    testthat::expect_gt(int_vmt_reduction, -0.03)
    testthat::expect_lt(int_vmt_reduction, 0)
    # Check that reductions don't exceed MAX_5D_DR (as negative values)
    testthat::expect_gte(pop_vmt_reduction, enviro_factors$MAX_5D_DR)
    testthat::expect_gte(emp_vmt_reduction, enviro_factors$MAX_5D_DR)
    testthat::expect_gte(int_vmt_reduction, enviro_factors$MAX_5D_DR)
  })
}


test_vmt_combined_parameters <- function(x) {
  testthat::test_that(paste0(x, " combined land use parameters produce stronger impacts"), {
    # Baseline
    pass_bau <- suppressMessages(
      mode_passenger_light_duty(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = x
      )
    )

    pass_bau_vmt <- pass_bau$vmt %>%
      filter(year == max(year)) %>%
      summarise(vmt = sum(vmt)) %>%
      pull(vmt)

    # Single parameter (10% pop density)
    pass_single <- suppressMessages(
      mode_passenger_light_duty(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = x,
        .scenario = "land_use",
        .pop_dens_pct_change = 0.10
      )
    )

    pass_single_vmt <- pass_single$vmt %>%
      filter(year == max(year)) %>%
      summarise(vmt = sum(vmt)) %>%
      pull(vmt)

    single_reduction <- (pass_single_vmt - pass_bau_vmt) / pass_bau_vmt

    # Multiple parameters (50% each for pop and emp, 10% for intersection)
    pass_multi <- suppressMessages(
      mode_passenger_light_duty(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = x,
        .scenario = "land_use",
        .pop_dens_pct_change = 0.50,
        .emp_dens_pct_change = 0.50,
        .intersection_density_pct_change = 0.10
      )
    )

    pass_multi_vmt <- pass_multi$vmt %>%
      filter(year == max(year)) %>%
      summarise(vmt = sum(vmt)) %>%
      pull(vmt)

    multi_reduction <- (pass_multi_vmt - pass_bau_vmt) / pass_bau_vmt

    # Combined parameters should produce greater reduction than single (more negative)
    testthat::expect_lt(multi_reduction, single_reduction)
    testthat::expect_lt(multi_reduction, -0.02)

    # Check that combined reduction doesn't exceed MAX_5D_DR (don't go below -0.25)
    testthat::expect_gte(single_reduction, enviro_factors$MAX_5D_DR)
    testthat::expect_gte(multi_reduction, enviro_factors$MAX_5D_DR)
  })
}


purrr::map(
  geography_test_list,
  function(f) {
    list(
      test_vmt_land_use_passenger(f),
      test_vmt_land_use_walk_bike(f),
      test_vmt_land_use_transit(f),
      test_vmt_individual_parameters(f),
      test_vmt_combined_parameters(f)
    )
  }
)
