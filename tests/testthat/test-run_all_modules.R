testthat::test_that("All modules run when together", {
  testthat::expect_no_error({
    bk_park <- run_all_modules(
      .scenario = "bau",
      .selected_ctu = "Brooklyn Park",
      pass_tb = ghg.gert::transportation_data$passenger,
      freight_tb = ghg.gert::transportation_data$freight,
      .factor_values = ghg.gert::factor_values,
      .enviro_factors = ghg.gert::enviro_factors,
      .elast = ghg.gert::elast,
      .elast_5d = ghg.gert::elast_5d,
      .fuel_economy = ghg.gert::fuel_economy,
      tb = ghg.gert::land_use_data,
      non_res_tb = ghg.gert::building_energy_data$non_residential,
      res_tb = ghg.gert::building_energy_data$residential,
      res_tb_bau = ghg.gert::building_energy_data$residential,
      non_res_tb_bau = ghg.gert::building_energy_data$non_residential,
      run_non_residential = FALSE
    ) %>%
      suppressWarnings() %>%
      suppressMessages()
  })

  testthat::expect_no_error({
    afton <- run_all_modules(
      .scenario = "bau",
      .selected_ctu = "Afton",
      pass_tb = ghg.gert::transportation_data$passenger,
      freight_tb = ghg.gert::transportation_data$freight,
      .factor_values = ghg.gert::factor_values,
      .enviro_factors = ghg.gert::enviro_factors,
      .elast = ghg.gert::elast,
      .elast_5d = ghg.gert::elast_5d,
      .fuel_economy = ghg.gert::fuel_economy,
      tb = ghg.gert::land_use_data,
      non_res_tb = ghg.gert::building_energy_data$non_residential,
      res_tb = ghg.gert::building_energy_data$residential,
      res_tb_bau = ghg.gert::building_energy_data$residential,
      non_res_tb_bau = ghg.gert::building_energy_data$non_residential,
      run_non_residential = FALSE
    ) %>%
      suppressWarnings() %>%
      suppressMessages()
  })

  testthat::expect_no_error({
    hennepin <- run_all_modules(
      .scenario = "bau",
      .selected_ctu = "Hennepin County",
      pass_tb = ghg.gert::transportation_data$passenger,
      freight_tb = ghg.gert::transportation_data$freight,
      .factor_values = ghg.gert::factor_values,
      .enviro_factors = ghg.gert::enviro_factors,
      .elast = ghg.gert::elast,
      .elast_5d = ghg.gert::elast_5d,
      .fuel_economy = ghg.gert::fuel_economy,
      tb = ghg.gert::land_use_data,
      non_res_tb = ghg.gert::building_energy_data$non_residential,
      res_tb = ghg.gert::building_energy_data$residential,
      res_tb_bau = ghg.gert::building_energy_data$residential,
      non_res_tb_bau = ghg.gert::building_energy_data$non_residential,
      run_non_residential = FALSE
    ) %>%
      suppressWarnings() %>%
      suppressMessages()
  })

  testthat::expect_no_error({
    ft_snelling <- run_all_modules(
      .scenario = "bau",
      .selected_ctu = "Fort Snelling",
      pass_tb = ghg.gert::transportation_data$passenger,
      freight_tb = ghg.gert::transportation_data$freight,
      .factor_values = ghg.gert::factor_values,
      .enviro_factors = ghg.gert::enviro_factors,
      .elast = ghg.gert::elast,
      .elast_5d = ghg.gert::elast_5d,
      .fuel_economy = ghg.gert::fuel_economy,
      tb = ghg.gert::land_use_data,
      non_res_tb = ghg.gert::building_energy_data$non_residential,
      res_tb = ghg.gert::building_energy_data$residential,
      res_tb_bau = ghg.gert::building_energy_data$residential,
      non_res_tb_bau = ghg.gert::building_energy_data$non_residential,
      run_non_residential = FALSE
    ) %>%
      suppressWarnings() %>%
      suppressMessages()
  })
})
