test_that("calc_ghg_residential", {
  testthat::expect_true(exists("calc_ghg_residential"))

  test_emissions <- function(ghg_table) {
    # MWH should decreas
    ghg_table %>%
      filter(inventory_year == 2050) %>%
      group_by(geog_name, scenario) %>%
      summarize(
        emissions = sum(electricity_emissions, natural_gas_emissions),
        .groups = "keep"
      ) %>%
      pivot_wider(
        names_from = scenario,
        values_from = emissions
      ) %>%
      filter(bau >= alt) %>%
      nrow() %>%
      testthat::expect_equal(1)
  }


  testthat::test_that("Minneapolis  emissions", {
    leed_table <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Minneapolis",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0,
      .new_mf_homes_leed_gold_pct = 0,
      .enviro_factors = ghg.ccap::enviro_factors
    ) %>%
      suppressWarnings()%>%
      calc_residential_retrofit(res_tb = .,
                                .selected_ctu = "Minneapolis",
                                .retrofit_start_year = 2025,
                                .existing_sf_retrofit_pct = 0,
                                .existing_mf_retrofit_pct = 0,
      )%>%
      suppressWarnings()


    energy_table <- calc_energy_residential(
      res_tb = leed_table,
      res_tb_bau = building_data$residential,
      .mwh_coefficients = mwh_coefficients,
      .mcf_coefficients = mcf_coefficients,
      .heatpump_start_year = 2025,
      .baseline_year = 2021,
      .sf_heat_pump_pct = 0,
      .mf_heat_pump_pct = 0,
      .scenario = "alt",
      .selected_ctu = "Minneapolis",
      .enviro_factors = enviro_factors
    )


    ghg_table <- calc_ghg_residential(
      res_energy = energy_table,
      .selected_ctu = "Minneapolis",
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    test_emissions(ghg_table)
  })

  testthat::test_that("Minneapolis  emissions interventions 1", {
    leed_table <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Minneapolis",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0.5,
      .new_mf_homes_leed_gold_pct = 0.5,
      .enviro_factors = ghg.ccap::enviro_factors
    ) %>%
      suppressWarnings() %>%
      calc_residential_retrofit(res_tb = .,
                                .selected_ctu = "Minneapolis",
                                .retrofit_start_year = 2025,
                                .existing_sf_retrofit_pct = 0,
                                .existing_mf_retrofit_pct = 0,
      )%>%
      suppressWarnings()


    energy_table <- calc_energy_residential(
      res_tb = leed_table,
      res_tb_bau = building_data$residential,
      .mwh_coefficients = mwh_coefficients,
      .mcf_coefficients = mcf_coefficients,
      .heatpump_start_year = 2025,
      .baseline_year = 2021,
      .sf_heat_pump_pct = 0.5,
      .mf_heat_pump_pct = 0.5,
      .scenario = "alt",
      .selected_ctu = "Minneapolis",
      .enviro_factors = enviro_factors
    )%>%
      suppressWarnings()


    ghg_table <- calc_ghg_residential(
      res_energy = energy_table,
      .selected_ctu = "Minneapolis",
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    test_emissions(ghg_table)
  })


  testthat::test_that("Minneapolis  emissions interventions 2", {
    leed_table <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Minneapolis",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0.1,
      .new_mf_homes_leed_gold_pct = 0.1,
      .enviro_factors = ghg.ccap::enviro_factors
    ) %>%
      suppressWarnings() %>%
      calc_residential_retrofit(res_tb = .,
                                .selected_ctu = "Minneapolis",
                                .retrofit_start_year = 2025,
                                .existing_sf_retrofit_pct = 0,
                                .existing_mf_retrofit_pct = 0,
      )%>%
      suppressWarnings()


    energy_table <- calc_energy_residential(
      res_tb = leed_table,
      res_tb_bau = building_data$residential,
      .mwh_coefficients = mwh_coefficients,
      .mcf_coefficients = mcf_coefficients,
      .heatpump_start_year = 2025,
      .baseline_year = 2021,
      .sf_heat_pump_pct = 0.25,
      .mf_heat_pump_pct = 0.15,
      .scenario = "alt",
      .selected_ctu = "Minneapolis",
      .enviro_factors = enviro_factors
    )%>%
      suppressWarnings()


    ghg_table <- calc_ghg_residential(
      res_energy = energy_table,
      .selected_ctu = "Minneapolis",
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    test_emissions(ghg_table)
  })


  testthat::test_that("Minneapolis  emissions interventions 3", {
    leed_table <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Minneapolis",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0.60,
      .new_mf_homes_leed_gold_pct = 0.1,
      .enviro_factors = ghg.ccap::enviro_factors
    ) %>%
      suppressWarnings() %>%
      calc_residential_retrofit(res_tb = .,
                                .selected_ctu = "Minneapolis",
                                .retrofit_start_year = 2025,
                                .existing_sf_retrofit_pct = 0,
                                .existing_mf_retrofit_pct = 0,
      )%>%
      suppressWarnings()


    energy_table <- calc_energy_residential(
      res_tb = leed_table,
      res_tb_bau = building_data$residential,
      .mwh_coefficients = mwh_coefficients,
      .mcf_coefficients = mcf_coefficients,
      .heatpump_start_year = 2025,
      .baseline_year = 2021,
      .sf_heat_pump_pct = 0.8,
      .mf_heat_pump_pct = 0.0,
      .scenario = "alt",
      .selected_ctu = "Minneapolis",
      .enviro_factors = enviro_factors
    )%>%
      suppressWarnings()


    ghg_table <- calc_ghg_residential(
      res_energy = energy_table,
      .selected_ctu = "Minneapolis",
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    test_emissions(ghg_table)
  })





  testthat::test_that("Saint Paul  emissions", {
    leed_table <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Saint Paul",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0,
      .new_mf_homes_leed_gold_pct = 0,
      .enviro_factors = ghg.ccap::enviro_factors
    ) %>%
      suppressWarnings() %>%
      calc_residential_retrofit(res_tb = .,
                                .selected_ctu = "Saint Paul",
                                .retrofit_start_year = 2025,
                                .existing_sf_retrofit_pct = 0,
                                .existing_mf_retrofit_pct = 0,
      )%>%
      suppressWarnings()


    energy_table <- calc_energy_residential(
      res_tb = leed_table,
      res_tb_bau = building_data$residential,
      .mwh_coefficients = mwh_coefficients,
      .mcf_coefficients = mcf_coefficients,
      .heatpump_start_year = 2025,
      .baseline_year = 2021,
      .sf_heat_pump_pct = 0,
      .mf_heat_pump_pct = 0,
      .scenario = "alt",
      .selected_ctu = "Saint Paul",
      .enviro_factors = enviro_factors
    )%>%
      suppressWarnings()


    ghg_table <- calc_ghg_residential(
      res_energy = energy_table,
      .selected_ctu = "Saint Paul",
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    test_emissions(ghg_table)
  })

  testthat::test_that("Saint Paul  emissions interventions 1", {
    leed_table <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Saint Paul",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0.5,
      .new_mf_homes_leed_gold_pct = 0.5,
      .enviro_factors = ghg.ccap::enviro_factors
    ) %>%
      suppressWarnings() %>%
      calc_residential_retrofit(res_tb = .,
                                .selected_ctu = "Saint Paul",
                                .retrofit_start_year = 2025,
                                .existing_sf_retrofit_pct = 0,
                                .existing_mf_retrofit_pct = 0,
      )%>%
      suppressWarnings()


    energy_table <- calc_energy_residential(
      res_tb = leed_table,
      res_tb_bau = building_data$residential,
      .mwh_coefficients = mwh_coefficients,
      .mcf_coefficients = mcf_coefficients,
      .heatpump_start_year = 2025,
      .baseline_year = 2021,
      .sf_heat_pump_pct = 0.5,
      .mf_heat_pump_pct = 0.5,
      .scenario = "alt",
      .selected_ctu = "Saint Paul",
      .enviro_factors = enviro_factors
    )%>%
      suppressWarnings()


    ghg_table <- calc_ghg_residential(
      res_energy = energy_table,
      .selected_ctu = "Saint Paul",
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    test_emissions(ghg_table)
  })


  testthat::test_that("Saint Paul  emissions interventions 2", {
    leed_table <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Saint Paul",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0.1,
      .new_mf_homes_leed_gold_pct = 0.1,
      .enviro_factors = ghg.ccap::enviro_factors
    ) %>%
      suppressWarnings() %>%
      calc_residential_retrofit(res_tb = .,
                                .selected_ctu = "Saint Paul",
                                .retrofit_start_year = 2025,
                                .existing_sf_retrofit_pct = 0,
                                .existing_mf_retrofit_pct = 0,
      )%>%
      suppressWarnings()


    energy_table <- calc_energy_residential(
      res_tb = leed_table,
      res_tb_bau = building_data$residential,
      .mwh_coefficients = mwh_coefficients,
      .mcf_coefficients = mcf_coefficients,
      .heatpump_start_year = 2025,
      .baseline_year = 2021,
      .sf_heat_pump_pct = 0.25,
      .mf_heat_pump_pct = 0.15,
      .scenario = "alt",
      .selected_ctu = "Saint Paul",
      .enviro_factors = enviro_factors
    )%>%
      suppressWarnings()


    ghg_table <- calc_ghg_residential(
      res_energy = energy_table,
      .selected_ctu = "Saint Paul",
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    test_emissions(ghg_table)
  })


  testthat::test_that("Saint Paul  emissions interventions 3", {
    leed_table <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Saint Paul",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0.60,
      .new_mf_homes_leed_gold_pct = 0.1,
      .enviro_factors = ghg.ccap::enviro_factors
    ) %>%
      suppressWarnings() %>%
      calc_residential_retrofit(res_tb = .,
                                .selected_ctu = "Saint Paul",
                                .retrofit_start_year = 2025,
                                .existing_sf_retrofit_pct = 0,
                                .existing_mf_retrofit_pct = 0,
      )%>%
      suppressWarnings()


    energy_table <- calc_energy_residential(
      res_tb = leed_table,
      res_tb_bau = building_data$residential,
      .mwh_coefficients = mwh_coefficients,
      .mcf_coefficients = mcf_coefficients,
      .heatpump_start_year = 2025,
      .baseline_year = 2021,
      .sf_heat_pump_pct = 0.8,
      .mf_heat_pump_pct = 0.0,
      .scenario = "alt",
      .selected_ctu = "Saint Paul",
      .enviro_factors = enviro_factors
    )%>%
      suppressWarnings()


    ghg_table <- calc_ghg_residential(
      res_energy = energy_table,
      .selected_ctu = "Saint Paul",
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    test_emissions(ghg_table)
  })



  testthat::test_that("Brooklyn Park  emissions", {
    leed_table <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Brooklyn Park",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0,
      .new_mf_homes_leed_gold_pct = 0,
      .enviro_factors = ghg.ccap::enviro_factors
    ) %>%
      suppressWarnings() %>%
      calc_residential_retrofit(res_tb = .,
                                .selected_ctu = "Brooklyn Park",
                                .retrofit_start_year = 2025,
                                .existing_sf_retrofit_pct = 0,
                                .existing_mf_retrofit_pct = 0,
      )%>%
      suppressWarnings()


    energy_table <- calc_energy_residential(
      res_tb = leed_table,
      res_tb_bau = building_data$residential,
      .mwh_coefficients = mwh_coefficients,
      .mcf_coefficients = mcf_coefficients,
      .heatpump_start_year = 2025,
      .baseline_year = 2021,
      .sf_heat_pump_pct = 0,
      .mf_heat_pump_pct = 0,
      .scenario = "alt",
      .selected_ctu = "Brooklyn Park",
      .enviro_factors = enviro_factors
    )%>%
      suppressWarnings()


    ghg_table <- calc_ghg_residential(
      res_energy = energy_table,
      .selected_ctu = "Brooklyn Park",
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    test_emissions(ghg_table)
  })

  testthat::test_that("Brooklyn Park  emissions interventions 1", {
    leed_table <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Brooklyn Park",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0.5,
      .new_mf_homes_leed_gold_pct = 0.5,
      .enviro_factors = ghg.ccap::enviro_factors
    ) %>%
      suppressWarnings() %>%
      calc_residential_retrofit(res_tb = .,
                                .selected_ctu = "Brooklyn Park",
                                .retrofit_start_year = 2025,
                                .existing_sf_retrofit_pct = 0,
                                .existing_mf_retrofit_pct = 0,
      )%>%
      suppressWarnings()


    energy_table <- calc_energy_residential(
      res_tb = leed_table,
      res_tb_bau = building_data$residential,
      .mwh_coefficients = mwh_coefficients,
      .mcf_coefficients = mcf_coefficients,
      .heatpump_start_year = 2025,
      .baseline_year = 2021,
      .sf_heat_pump_pct = 0.5,
      .mf_heat_pump_pct = 0.5,
      .scenario = "alt",
      .selected_ctu = "Brooklyn Park",
      .enviro_factors = enviro_factors
    )%>%
      suppressWarnings()


    ghg_table <- calc_ghg_residential(
      res_energy = energy_table,
      .selected_ctu = "Brooklyn Park",
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    test_emissions(ghg_table)
  })


  testthat::test_that("Brooklyn Park  emissions interventions 2", {
    leed_table <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Brooklyn Park",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0.1,
      .new_mf_homes_leed_gold_pct = 0.1,
      .enviro_factors = ghg.ccap::enviro_factors
    ) %>%
      suppressWarnings() %>%
      calc_residential_retrofit(res_tb = .,
                                .selected_ctu = "Brooklyn Park",
                                .retrofit_start_year = 2025,
                                .existing_sf_retrofit_pct = 0,
                                .existing_mf_retrofit_pct = 0,
      )%>%
      suppressWarnings()


    energy_table <- calc_energy_residential(
      res_tb = leed_table,
      res_tb_bau = building_data$residential,
      .mwh_coefficients = mwh_coefficients,
      .mcf_coefficients = mcf_coefficients,
      .heatpump_start_year = 2025,
      .baseline_year = 2021,
      .sf_heat_pump_pct = 0.25,
      .mf_heat_pump_pct = 0.15,
      .scenario = "alt",
      .selected_ctu = "Brooklyn Park",
      .enviro_factors = enviro_factors
    )%>%
      suppressWarnings()


    ghg_table <- calc_ghg_residential(
      res_energy = energy_table,
      .selected_ctu = "Brooklyn Park",
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    test_emissions(ghg_table)
  })


  testthat::test_that("Brooklyn Park  emissions interventions 3", {
    leed_table <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Brooklyn Park",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0.60,
      .new_mf_homes_leed_gold_pct = 0.1,
      .enviro_factors = ghg.ccap::enviro_factors
    ) %>%
      suppressWarnings() %>%
      calc_residential_retrofit(res_tb = .,
                                .selected_ctu = "Brooklyn Park",
                                .retrofit_start_year = 2025,
                                .existing_sf_retrofit_pct = 0,
                                .existing_mf_retrofit_pct = 0,
      )%>%
      suppressWarnings()


    energy_table <- calc_energy_residential(
      res_tb = leed_table,
      res_tb_bau = building_data$residential,
      .mwh_coefficients = mwh_coefficients,
      .mcf_coefficients = mcf_coefficients,
      .heatpump_start_year = 2025,
      .baseline_year = 2021,
      .sf_heat_pump_pct = 0.8,
      .mf_heat_pump_pct = 0.0,
      .scenario = "alt",
      .selected_ctu = "Brooklyn Park",
      .enviro_factors = enviro_factors
    )%>%
      suppressWarnings()


    ghg_table <- calc_ghg_residential(
      res_energy = energy_table,
      .selected_ctu = "Brooklyn Park",
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    test_emissions(ghg_table)
  })
})
