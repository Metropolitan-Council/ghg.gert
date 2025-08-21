testthat::test_that("calc_energy_residential", {
  expect_true(exists("calc_energy_residential")) # prevent "empty test" notification

  test_energy_efficiency <- function(en_table) {
    # MWH should decrease
    en_table %>%
      filter(inventory_year == 2050) %>%
      select(geog_name, scenario, residential_mwh) %>%
      pivot_wider(
        names_from = scenario,
        values_from = residential_mwh
      ) %>%
      filter(bau > alt) %>%
      nrow() %>%
      testthat::expect_equal(1)

    # MCF should decrease
    en_table %>%
      filter(inventory_year == 2050) %>%
      select(geog_name, scenario, residential_mcf) %>%
      pivot_wider(
        names_from = scenario,
        values_from = residential_mcf
      ) %>%
      filter(bau > alt) %>%
      nrow() %>%
      testthat::expect_equal(1)
  }

  test_energy_electrification <- function(en_table) {
    # MWH should decrease
    en_table %>%
      filter(inventory_year == 2050) %>%
      select(geog_name, scenario, residential_mwh) %>%
      pivot_wider(
        names_from = scenario,
        values_from = residential_mwh
      ) %>%
      filter(bau < alt) %>%
      nrow() %>%
      testthat::expect_equal(1)

    # MCF should decrease
    en_table %>%
      filter(inventory_year == 2050) %>%
      select(geog_name, scenario, residential_mcf) %>%
      pivot_wider(
        names_from = scenario,
        values_from = residential_mcf
      ) %>%
      filter(bau > alt) %>%
      nrow() %>%
      testthat::expect_equal(1)
  }


  testthat::test_that("Energy residential should reduce with interventions - Minneapolis", {
    ### Efficient new buildings

    leed_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(),
      .scenario = "alt",
      .selected_ctu = "Minneapolis",
      .baseline_year = 2022,
      .leed_start_year = 2028,
      .new_sf_homes_leed_gold_pct = 0.3,
      .new_mf_homes_leed_gold_pct = 0.3,
      .retrofit_start_year = 2028,
      .retrofit_end_year = 2050,
      .existing_sf_retrofit_pct = 0,
      .existing_mf_retrofit_pct = 0,
      .heatpump_start_year = 2028,
      .heatpump_end_year = 2050,
      .sf_heat_pump_pct = 0,
      .mf_heat_pump_pct = 0,
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )


    ### Retrofit existing buildings

    retro_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(),
      .scenario = "alt",
      .selected_ctu = "Minneapolis",
      .baseline_year = 2022,
      .leed_start_year = 2028,
      .new_sf_homes_leed_gold_pct = 0,
      .new_mf_homes_leed_gold_pct = 0,
      .retrofit_start_year = 2028,
      .retrofit_end_year = 2050,
      .existing_sf_retrofit_pct = 0.3,
      .existing_mf_retrofit_pct = 0.3,
      .heatpump_start_year = 2028,
      .heatpump_end_year = 2050,
      .sf_heat_pump_pct = 0,
      .mf_heat_pump_pct = 0,
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    # install heat pumps

    heatpump_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(),
      .scenario = "alt",
      .selected_ctu = "Minneapolis",
      .baseline_year = 2022,
      .leed_start_year = 2028,
      .new_sf_homes_leed_gold_pct = 0,
      .new_mf_homes_leed_gold_pct = 0,
      .retrofit_start_year = 2028,
      .retrofit_end_year = 2050,
      .existing_sf_retrofit_pct = 0,
      .existing_mf_retrofit_pct = 0,
      .heatpump_start_year = 2028,
      .heatpump_end_year = 2050,
      .sf_heat_pump_pct = 0.3,
      .mf_heat_pump_pct = 0.3,
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    # combination


    combo_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(),
      .scenario = "alt",
      .selected_ctu = "Minneapolis",
      .baseline_year = 2022,
      .leed_start_year = 2028,
      .new_sf_homes_leed_gold_pct = 0.3,
      .new_mf_homes_leed_gold_pct = 0.3,
      .retrofit_start_year = 2028,
      .retrofit_end_year = 2050,
      .existing_sf_retrofit_pct = 0.4,
      .existing_mf_retrofit_pct = 0.4,
      .heatpump_start_year = 2028,
      .heatpump_end_year = 2050,
      .sf_heat_pump_pct = 0.5,
      .mf_heat_pump_pct = 0.5,
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    purrr::map(
      list(leed_table, retro_table),
      test_energy_efficiency
    )

    purrr::map(
      list(heatpump_table, combo_table),
      test_energy_electrification
    )
  })

  testthat::test_that("Energy residential should reduce with interventions - Maple Grove", {
    ### Maple Grove ----

    ### Efficient new buildings

    leed_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .scenario = "alt",
      .selected_ctu = "Maple Grove",
      .baseline_year = 2022,
      .leed_start_year = 2028,
      .new_sf_homes_leed_gold_pct = 0.3,
      .new_mf_homes_leed_gold_pct = 0.3,
      .retrofit_start_year = 2028,
      .retrofit_end_year = 2050,
      .existing_sf_retrofit_pct = 0,
      .existing_mf_retrofit_pct = 0,
      .heatpump_start_year = 2028,
      .heatpump_end_year = 2050,
      .sf_heat_pump_pct = 0,
      .mf_heat_pump_pct = 0,
      .density_output = run_scenario_land_use(),
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    leed_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(),
      .scenario = "alt",
      .selected_ctu = "Maple Grove",
      .baseline_year = 2022,
      .leed_start_year = 2028,
      .new_sf_homes_leed_gold_pct = 0.3,
      .new_mf_homes_leed_gold_pct = 0.3,
      .retrofit_start_year = 2028,
      .retrofit_end_year = 2050,
      .existing_sf_retrofit_pct = 0,
      .existing_mf_retrofit_pct = 0,
      .heatpump_start_year = 2028,
      .heatpump_end_year = 2050,
      .sf_heat_pump_pct = 0,
      .mf_heat_pump_pct = 0,
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    ### Retrofit existing buildings

    retro_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(),
      .scenario = "alt",
      .selected_ctu = "Maple Grove",
      .baseline_year = 2022,
      .leed_start_year = 2028,
      .new_sf_homes_leed_gold_pct = 0,
      .new_mf_homes_leed_gold_pct = 0,
      .retrofit_start_year = 2028,
      .retrofit_end_year = 2050,
      .existing_sf_retrofit_pct = 0.3,
      .existing_mf_retrofit_pct = 0.3,
      .heatpump_start_year = 2028,
      .heatpump_end_year = 2050,
      .sf_heat_pump_pct = 0,
      .mf_heat_pump_pct = 0,
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    retro_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(),
      .scenario = "alt",
      .selected_ctu = "Maple Grove",
      .baseline_year = 2022,
      .leed_start_year = 2028,
      .new_sf_homes_leed_gold_pct = 0,
      .new_mf_homes_leed_gold_pct = 0,
      .retrofit_start_year = 2028,
      .retrofit_end_year = 2050,
      .existing_sf_retrofit_pct = 0.3,
      .existing_mf_retrofit_pct = 0.3,
      .heatpump_start_year = 2028,
      .heatpump_end_year = 2050,
      .sf_heat_pump_pct = 0,
      .mf_heat_pump_pct = 0,
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    heatpump_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(),
      .scenario = "alt",
      .selected_ctu = "Maple Grove",
      .baseline_year = 2022,
      .leed_start_year = 2028,
      .new_sf_homes_leed_gold_pct = 0,
      .new_mf_homes_leed_gold_pct = 0,
      .retrofit_start_year = 2028,
      .retrofit_end_year = 2050,
      .existing_sf_retrofit_pct = 0,
      .existing_mf_retrofit_pct = 0,
      .heatpump_start_year = 2028,
      .heatpump_end_year = 2050,
      .sf_heat_pump_pct = 0.3,
      .mf_heat_pump_pct = 0.3,
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    heatpump_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(),
      .scenario = "alt",
      .selected_ctu = "Maple Grove",
      .baseline_year = 2022,
      .leed_start_year = 2028,
      .new_sf_homes_leed_gold_pct = 0,
      .new_mf_homes_leed_gold_pct = 0,
      .retrofit_start_year = 2028,
      .retrofit_end_year = 2050,
      .existing_sf_retrofit_pct = 0,
      .existing_mf_retrofit_pct = 0,
      .heatpump_start_year = 2028,
      .heatpump_end_year = 2050,
      .sf_heat_pump_pct = 0.3,
      .mf_heat_pump_pct = 0.3,
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )


    combo_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(),
      .scenario = "alt",
      .selected_ctu = "Maple Grove",
      .baseline_year = 2022,
      .leed_start_year = 2028,
      .new_sf_homes_leed_gold_pct = 0.3,
      .new_mf_homes_leed_gold_pct = 0.3,
      .retrofit_start_year = 2028,
      .retrofit_end_year = 2050,
      .existing_sf_retrofit_pct = 0.4,
      .existing_mf_retrofit_pct = 0.4,
      .heatpump_start_year = 2028,
      .heatpump_end_year = 2050,
      .sf_heat_pump_pct = 0.5,
      .mf_heat_pump_pct = 0.5,
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    combo_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(),
      .scenario = "alt",
      .selected_ctu = "Maple Grove",
      .baseline_year = 2022,
      .leed_start_year = 2028,
      .new_sf_homes_leed_gold_pct = 0.3,
      .new_mf_homes_leed_gold_pct = 0.3,
      .retrofit_start_year = 2028,
      .retrofit_end_year = 2050,
      .existing_sf_retrofit_pct = 0.4,
      .existing_mf_retrofit_pct = 0.4,
      .heatpump_start_year = 2028,
      .heatpump_end_year = 2050,
      .sf_heat_pump_pct = 0.5,
      .mf_heat_pump_pct = 0.5,
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    purrr::map(
      list(heatpump_table, combo_table),
      test_energy_electrification
    )
  })



  testthat::test_that("Energy residential should reduce with interventions - New Germany", {
    ### New Germany ----

    ### Efficient new buildings

    leed_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(),
      .scenario = "alt",
      .selected_ctu = "New Germany",
      .baseline_year = 2022,
      .leed_start_year = 2028,
      .new_sf_homes_leed_gold_pct = 0.3,
      .new_mf_homes_leed_gold_pct = 0.3,
      .retrofit_start_year = 2028,
      .retrofit_end_year = 2050,
      .existing_sf_retrofit_pct = 0,
      .existing_mf_retrofit_pct = 0,
      .heatpump_start_year = 2028,
      .heatpump_end_year = 2050,
      .sf_heat_pump_pct = 0,
      .mf_heat_pump_pct = 0,
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )


    ### Retrofit existing buildings

    retro_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(),
      .scenario = "alt",
      .selected_ctu = "New Germany",
      .baseline_year = 2022,
      .leed_start_year = 2028,
      .new_sf_homes_leed_gold_pct = 0,
      .new_mf_homes_leed_gold_pct = 0,
      .retrofit_start_year = 2028,
      .retrofit_end_year = 2050,
      .existing_sf_retrofit_pct = 0.3,
      .existing_mf_retrofit_pct = 0.3,
      .heatpump_start_year = 2028,
      .heatpump_end_year = 2050,
      .sf_heat_pump_pct = 0,
      .mf_heat_pump_pct = 0,
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    # install heat pumps

    heatpump_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(),
      .scenario = "alt",
      .selected_ctu = "New Germany",
      .baseline_year = 2022,
      .leed_start_year = 2028,
      .new_sf_homes_leed_gold_pct = 0,
      .new_mf_homes_leed_gold_pct = 0,
      .retrofit_start_year = 2028,
      .retrofit_end_year = 2050,
      .existing_sf_retrofit_pct = 0,
      .existing_mf_retrofit_pct = 0,
      .heatpump_start_year = 2028,
      .heatpump_end_year = 2050,
      .sf_heat_pump_pct = 0.3,
      .mf_heat_pump_pct = 0.3,
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    # combination


    combo_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(),
      .scenario = "alt",
      .selected_ctu = "New Germany",
      .baseline_year = 2022,
      .leed_start_year = 2028,
      .new_sf_homes_leed_gold_pct = 0.3,
      .new_mf_homes_leed_gold_pct = 0.3,
      .retrofit_start_year = 2028,
      .retrofit_end_year = 2050,
      .existing_sf_retrofit_pct = 0.4,
      .existing_mf_retrofit_pct = 0.4,
      .heatpump_start_year = 2028,
      .heatpump_end_year = 2050,
      .sf_heat_pump_pct = 0.5,
      .mf_heat_pump_pct = 0.5,
      .grid_emissions = ghg.ccap::grid_emissions,
      .enviro_factors = ghg.ccap::enviro_factors
    )

    purrr::map(
      list(leed_table, retro_table),
      test_energy_efficiency
    )

    purrr::map(
      list(heatpump_table, combo_table),
      test_energy_electrification
    )
  })
})
