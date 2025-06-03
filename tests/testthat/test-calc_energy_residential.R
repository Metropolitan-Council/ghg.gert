testthat::test_that("calc_energy_residential", {

  expect_true(exists("calc_energy_residential"))  # prevent "empty test" notification

  test_energy <- function(en_table){
    # MWH should decreas
    en_table %>%
      filter(inventory_year == 2050) %>%
      select(-residential_mcf) %>%
      pivot_wider(names_from = scenario,
                  values_from = residential_mwh) %>%
      filter(bau < alt) %>%
      nrow() %>%
      testthat::expect_equal(1)

    # MCF should increase
    en_table %>%
      filter(inventory_year == 2050) %>%
      select(-residential_mwh) %>%
      pivot_wider(names_from = scenario,
                  values_from = residential_mcf) %>%
      filter(bau > alt) %>%
      nrow() %>%
      testthat::expect_equal(1)
  }


  testthat::test_that("Energy residential should reduce with interventions - Minneapolis", {

    leed_table <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Minneapolis",
      .new_sf_homes_leed_gold_pct = 0,
      .new_mf_homes_leed_gold_pct = 0,
      .enviro_factors = ghg.ccap::enviro_factors
    ) %>%
      suppressWarnings()

    t_hp50 <- calc_energy_residential(
      res_tb = leed_table,
      res_tb_bau = building_data$residential,
      .mwh_coefficients = mwh_coefficients,
      .mcf_coefficients = mcf_coefficients,
      .sf_heat_pump_pct = 0.5,
      .mf_heat_pump_pct = 0.5,
      .scenario = "alt",
      .selected_ctu = "Minneapolis",
      .enviro_factors = enviro_factors
    )


    t_hp60 <- calc_energy_residential(
      res_tb = leed_table,
      res_tb_bau = building_data$residential,
      .mwh_coefficients = mwh_coefficients,
      .mcf_coefficients = mcf_coefficients,
      .sf_heat_pump_pct = 0.60,
      .mf_heat_pump_pct = 0.60,
      .scenario = "alt",
      .selected_ctu = "Minneapolis",
      .enviro_factors = enviro_factors
    )


    t_hp70 <- calc_energy_residential(
      res_tb = leed_table,
      res_tb_bau = building_data$residential,
      .mwh_coefficients = mwh_coefficients,
      .mcf_coefficients = mcf_coefficients,
      .sf_heat_pump_pct = 0.70,
      .mf_heat_pump_pct = 0.70,
      .scenario = "alt",
      .selected_ctu = "Minneapolis",
      .enviro_factors = enviro_factors
    )

    purrr::map(
      list(t_hp50, t_hp60, t_hp70),
      test_energy
    )

  })


  testthat::test_that("Energy residential should reduce with interventions - Maple Plain", {

    leed_table <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Maple Plain",
      .new_sf_homes_leed_gold_pct = 0,
      .new_mf_homes_leed_gold_pct = 0,
      .enviro_factors = ghg.ccap::enviro_factors
    ) %>%
      suppressWarnings()

    t_hp50 <- calc_energy_residential(
      res_tb = leed_table,
      res_tb_bau = building_data$residential,
      .mwh_coefficients = mwh_coefficients,
      .mcf_coefficients = mcf_coefficients,
      .sf_heat_pump_pct = 0.5,
      .mf_heat_pump_pct = 0.5,
      .scenario = "alt",
      .selected_ctu = "Maple Plain",
      .enviro_factors = enviro_factors
    )


    t_hp60 <- calc_energy_residential(
      res_tb = leed_table,
      res_tb_bau = building_data$residential,
      .mwh_coefficients = mwh_coefficients,
      .mcf_coefficients = mcf_coefficients,
      .sf_heat_pump_pct = 0.60,
      .mf_heat_pump_pct = 0.60,
      .scenario = "alt",
      .selected_ctu = "Maple Plain",
      .enviro_factors = enviro_factors
    )


    t_hp70 <- calc_energy_residential(
      res_tb = leed_table,
      res_tb_bau = building_data$residential,
      .mwh_coefficients = mwh_coefficients,
      .mcf_coefficients = mcf_coefficients,
      .sf_heat_pump_pct = 0.70,
      .mf_heat_pump_pct = 0.70,
      .scenario = "alt",
      .selected_ctu = "Maple Plain",
      .enviro_factors = enviro_factors
    )



    purrr::map(
      list(t_hp50, t_hp60, t_hp70),
      test_energy
    )

  })



  testthat::test_that("Energy residential should reduce with interventions - New Germany", {

    leed_table <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "New Germany",
      .new_sf_homes_leed_gold_pct = 0.6,
      .new_mf_homes_leed_gold_pct = 0.6,
      .enviro_factors = ghg.ccap::enviro_factors
    ) %>%
      suppressWarnings()

    t_hp50 <- calc_energy_residential(
      res_tb = leed_table,
      res_tb_bau = building_data$residential,
      .mwh_coefficients = mwh_coefficients,
      .mcf_coefficients = mcf_coefficients,
      .sf_heat_pump_pct = 0.5,
      .mf_heat_pump_pct = 0.5,
      .scenario = "alt",
      .selected_ctu = "New Germany",
      .enviro_factors = enviro_factors
    )


    t_hp60 <- calc_energy_residential(
      res_tb = leed_table,
      res_tb_bau = building_data$residential,
      .mwh_coefficients = mwh_coefficients,
      .mcf_coefficients = mcf_coefficients,
      .sf_heat_pump_pct = 0.60,
      .mf_heat_pump_pct = 0.60,
      .scenario = "alt",
      .selected_ctu = "New Germany",
      .enviro_factors = enviro_factors
    )


    t_hp70 <- calc_energy_residential(
      res_tb = leed_table,
      res_tb_bau = building_data$residential,
      .mwh_coefficients = mwh_coefficients,
      .mcf_coefficients = mcf_coefficients,
      .sf_heat_pump_pct = 0.70,
      .mf_heat_pump_pct = 0.70,
      .scenario = "alt",
      .selected_ctu = "New Germany",
      .enviro_factors = enviro_factors
    )


    purrr::map(
      list(t_hp50, t_hp60, t_hp70),
      test_energy
    )

  })
})

