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


  testthat::test_that("Residential emission should reduce with interventions - Minneapolis", {
    ### Efficient new buildings

    leed_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(
        .selected_ctu = "Minneapolis"
      ),
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
      .sf_heatpump_pct = 0,
      .mf_heatpump_pct = 0,
      .grid_emissions = ghg.gert::grid_emissions,
      .enviro_factors = ghg.gert::enviro_factors
    )


    ### Retrofit existing buildings

    retro_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(
        .selected_ctu = "Minneapolis"
      ),
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
      .sf_heatpump_pct = 0,
      .mf_heatpump_pct = 0,
      .grid_emissions = ghg.gert::grid_emissions,
      .enviro_factors = ghg.gert::enviro_factors
    )

    # install heat pumps

    heatpump_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(
        .selected_ctu = "Minneapolis"
      ),
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
      .sf_heatpump_pct = 0.3,
      .mf_heatpump_pct = 0.3,
      .grid_emissions = ghg.gert::grid_emissions,
      .enviro_factors = ghg.gert::enviro_factors
    )

    # combination


    combo_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(
        .selected_ctu = "Minneapolis"
      ),
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
      .sf_heatpump_pct = 0.5,
      .mf_heatpump_pct = 0.5,
      .grid_emissions = ghg.gert::grid_emissions,
      .enviro_factors = ghg.gert::enviro_factors
    )

    purrr::map(
      list(leed_table, retro_table, heatpump_table, combo_table),
      test_emissions
    )
  })

  testthat::test_that("Energy residential should reduce with interventions - Maple Grove", {
    ### Maple Grove ----

    ### Efficient new buildings

    leed_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(
        .selected_ctu = "Maple Grove"
      ),
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
      .sf_heatpump_pct = 0,
      .mf_heatpump_pct = 0,
      .grid_emissions = ghg.gert::grid_emissions,
      .enviro_factors = ghg.gert::enviro_factors
    )


    ### Retrofit existing buildings

    retro_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(
        .selected_ctu = "Maple Grove"
      ),
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
      .sf_heatpump_pct = 0,
      .mf_heatpump_pct = 0,
      .grid_emissions = ghg.gert::grid_emissions,
      .enviro_factors = ghg.gert::enviro_factors
    )

    # install heat pumps

    heatpump_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(
        .selected_ctu = "Maple Grove"
      ),
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
      .sf_heatpump_pct = 0.3,
      .mf_heatpump_pct = 0.3,
      .grid_emissions = ghg.gert::grid_emissions,
      .enviro_factors = ghg.gert::enviro_factors
    )

    # combination


    combo_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(
        .selected_ctu = "Maple Grove"
      ),
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
      .sf_heatpump_pct = 0.5,
      .mf_heatpump_pct = 0.5,
      .grid_emissions = ghg.gert::grid_emissions,
      .enviro_factors = ghg.gert::enviro_factors
    )

    purrr::map(
      list(leed_table, retro_table, heatpump_table, combo_table),
      test_emissions
    )
  })


  testthat::test_that("Energy residential should reduce with interventions - New Germany", {
    ### New Germany ----

    ### Efficient new buildings

    leed_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(
        .selected_ctu = "New Germany"
      ),
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
      .sf_heatpump_pct = 0,
      .mf_heatpump_pct = 0,
      .grid_emissions = ghg.gert::grid_emissions,
      .enviro_factors = ghg.gert::enviro_factors
    )


    ### Retrofit existing buildings

    retro_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(
        .selected_ctu = "New Germany"
      ),
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
      .sf_heatpump_pct = 0,
      .mf_heatpump_pct = 0,
      .grid_emissions = ghg.gert::grid_emissions,
      .enviro_factors = ghg.gert::enviro_factors
    )

    # install heat pumps

    heatpump_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(
        .selected_ctu = "New Germany"
      ),
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
      .sf_heatpump_pct = 0.3,
      .mf_heatpump_pct = 0.3,
      .grid_emissions = ghg.gert::grid_emissions,
      .enviro_factors = ghg.gert::enviro_factors
    )

    # combination


    combo_table <- scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .density_output = run_scenario_land_use(
        .selected_ctu = "New Germany"
      ),
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
      .sf_heatpump_pct = 0.5,
      .mf_heatpump_pct = 0.5,
      .grid_emissions = ghg.gert::grid_emissions,
      .enviro_factors = ghg.gert::enviro_factors
    )

    purrr::map(
      list(leed_table, retro_table, heatpump_table, combo_table),
      test_emissions
    )
  })
})
