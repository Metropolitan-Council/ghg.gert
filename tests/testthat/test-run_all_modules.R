# testthat::test_that("All modules run when together", {
#   testthat::expect_no_error({
#     bk_park <- run_all_modules(
#       .scenario = "bau",
#       .selected_ctu = "Brooklyn Park",
#       pass_tb = ghg.ccap::transportation_data$passenger,
#       freight_tb = ghg.ccap::transportation_data$freight,
#       .factor_values = ghg.ccap::factor_values,
#       .enviro_factors = ghg.ccap::enviro_factors,
#       .elast = ghg.ccap::elast,
#       .elast_5d = ghg.ccap::elast_5d,
#       .fuel_economy = ghg.ccap::fuel_economy,
#       tb = ghg.ccap::land_use_data,
#       non_res_tb = ghg.ccap::building_data$non_residential,
#       res_tb = ghg.ccap::building_data$residential,
#       res_tb_bau = ghg.ccap::building_data$residential,
#       non_res_tb_bau = ghg.ccap::building_data$non_residential,
#       run_non_residential = FALSE
#     ) %>%
#       suppressWarnings() %>%
#       suppressMessages()
#   })
#
#   testthat::expect_no_error({
#     afton <- run_all_modules(
#       .scenario = "bau",
#       .selected_ctu = "Afton",
#       pass_tb = ghg.ccap::transportation_data$passenger,
#       freight_tb = ghg.ccap::transportation_data$freight,
#       .factor_values = ghg.ccap::factor_values,
#       .enviro_factors = ghg.ccap::enviro_factors,
#       .elast = ghg.ccap::elast,
#       .elast_5d = ghg.ccap::elast_5d,
#       .fuel_economy = ghg.ccap::fuel_economy,
#       tb = ghg.ccap::land_use_data,
#       non_res_tb = ghg.ccap::building_data$non_residential,
#       res_tb = ghg.ccap::building_data$residential,
#       res_tb_bau = ghg.ccap::building_data$residential,
#       non_res_tb_bau = ghg.ccap::building_data$non_residential,
#       run_non_residential = FALSE
#     ) %>%
#       suppressWarnings() %>%
#       suppressMessages()
#   })
#
#   testthat::expect_no_error({
#     ft_snelling <- run_all_modules(
#       .scenario = "bau",
#       .selected_ctu = "Fort Snelling",
#       pass_tb = ghg.ccap::transportation_data$passenger,
#       freight_tb = ghg.ccap::transportation_data$freight,
#       .factor_values = ghg.ccap::factor_values,
#       .enviro_factors = ghg.ccap::enviro_factors,
#       .elast = ghg.ccap::elast,
#       .elast_5d = ghg.ccap::elast_5d,
#       .fuel_economy = ghg.ccap::fuel_economy,
#       tb = ghg.ccap::land_use_data,
#       non_res_tb = ghg.ccap::building_data$non_residential,
#       res_tb = ghg.ccap::building_data$residential,
#       res_tb_bau = ghg.ccap::building_data$residential,
#       non_res_tb_bau = ghg.ccap::building_data$non_residential,
#       run_non_residential = FALSE
#     ) %>%
#       suppressWarnings() %>%
#       suppressMessages()
#   })
# })
