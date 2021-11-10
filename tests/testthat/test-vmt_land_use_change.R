
ten_pct <- vmt_land_use_change(
                               .type = "DRIVE",
                               .comb_5d_impact_pct_change = 0.25,
                               .pop_dens_pct_change = 0.10,
                               .emp_dens_pct_change = 0.1,
                               .land_use_pct_change =0.1,
                               .intersection_design_pct_change = 0.1,
                               .job_access_pct_change = 0.1,
                               .transit_dist_pct_change = -0.1,
                               .enviro_factors = enviro_factors)

testthat::expect_equal(
  ten_pct$land_use_adj,
  c(1, 1, 1, 0.98970403201327, 0.979482589472242, 0.96933536458158,
    0.959262049651148, 0.949262337100955, 0.93933591946609))

# 10% increase in pop dens  = -0.4% decrease in auto VMT


walk <- vmt_land_use_change(
                            .type = "WALK",
                            .comb_5d_impact_pct_change = 0.25,
                            .pop_dens_pct_change = 0.10,
                            .emp_dens_pct_change = 0.1,
                            .land_use_pct_change = 0.1,
                            .intersection_design_pct_change = 0.1,
                            .job_access_pct_change = 0.1,
                            .transit_dist_pct_change = -0.1,
                            .enviro_factors = enviro_factors)

testthat::expect_equal(
  walk$land_use_adj,
  c(1, 1, 1, 1.00532423435324, 1.01063009720849, 1.01591732862345,
    1.02118566914814, 1.02643485983302, 1.03166464223698))


transit <- vmt_land_use_change(
                            .type = "TRANSIT" ,
                            .comb_5d_impact_pct_change = 0.25,
                            .pop_dens_pct_change = 0.10,
                            .emp_dens_pct_change = 0.1,
                            .land_use_pct_change = 0.1,
                            .intersection_design_pct_change = 0.1,
                            .job_access_pct_change = 0.1,
                            .transit_dist_pct_change = -0.1,
                            .enviro_factors = enviro_factors)


testthat::expect_equal(
  transit$land_use_adj,
c(1, 1, 1, 1.0158428389461, 1.03177029962998, 1.04778077081956,
  1.06387260503572, 1.0800441182434, 1.09629358954158))


# test error -----

testthat::expect_error(vmt_land_use_change(.mode = "PLDV",
                                           .type = "scoot",
                                           .comb_5d_impact_pct_change = 0.25,
                                           .pop_dens_pct_change = 0.10,
                                           .emp_dens_pct_change = 0.1,
                                           .land_use_pct_change = 0.1,
                                           .intersection_design_pct_change = 0.1,
                                           .job_access_pct_change = 0.1,
                                           .transit_dist_pct_change = -0.1,
                                           .enviro_factors = enviro_factors))
