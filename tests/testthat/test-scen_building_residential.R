test_that("Residential building energy runs", {
  none <- scen_building_residential(
    res_tb = ghg.ccap::building_data$residential,
    res_tb_bau = ghg.ccap::building_data$residential,
    .selected_ctu = "Saint Paul",
    .scenario = "none",
    .density_output = run_scenario_land_use(
      tb = planned_land_use$ctu_planned_land_use_parcel,
      tb_strategy = NULL,
      .selected_ctu = "Saint Paul",
      .scenario = "none"
    ),
    .grid_emissions = ghg.ccap::grid_emissions,
    .enviro_factors = ghg.ccap::enviro_factors,
    .leed_start_year = 2028,
    .new_sf_homes_leed_gold_pct = 0,
    .new_mf_homes_leed_gold_pct = 0,
    .retrofit_start_year = 2028,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0,
    .existing_mf_retrofit_pct = 0,
    .heatpump_start_year = 2028,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0,
    .mf_heat_pump_pct = 0,
    .baseline_year = 2025
  ) %>%
    mutate(scen_run = "none")


  leed <- scen_building_residential(
    res_tb = ghg.ccap::building_data$residential,
    res_tb_bau = ghg.ccap::building_data$residential,
    .selected_ctu = "Saint Paul",
    .scenario = "leed",
    .density_output = run_scenario_land_use(
      tb = planned_land_use$ctu_planned_land_use_parcel,
      tb_strategy = NULL,
      .selected_ctu = "Saint Paul",
      .scenario = "leed"
    ),
    .grid_emissions = ghg.ccap::grid_emissions,
    .enviro_factors = ghg.ccap::enviro_factors,
    .leed_start_year = 2028,
    .new_sf_homes_leed_gold_pct = 0.10,
    .new_mf_homes_leed_gold_pct = 0.50,
    .retrofit_start_year = 2028,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0,
    .existing_mf_retrofit_pct = 0,
    .heatpump_start_year = 2028,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0,
    .mf_heat_pump_pct = 0,
    .baseline_year = 2025
  ) %>%
    mutate(scen_run = "leed")



  retrofit <- scen_building_residential(
    res_tb = ghg.ccap::building_data$residential,
    res_tb_bau = ghg.ccap::building_data$residential,
    .selected_ctu = "Saint Paul",
    .scenario = "leed",
    .density_output = run_scenario_land_use(
      tb = planned_land_use$ctu_planned_land_use_parcel,
      tb_strategy = NULL,
      .selected_ctu = "Saint Paul",
      .scenario = "retrofit"
    ),
    .grid_emissions = ghg.ccap::grid_emissions,
    .enviro_factors = ghg.ccap::enviro_factors,
    .leed_start_year = 2028,
    .new_sf_homes_leed_gold_pct = 0.0,
    .new_mf_homes_leed_gold_pct = 0.0,
    .retrofit_start_year = 2028,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0.50,
    .existing_mf_retrofit_pct = 0.60,
    .heatpump_start_year = 2028,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0,
    .mf_heat_pump_pct = 0,
    .baseline_year = 2025
  ) %>%
    mutate(scen_run = "retrofit")


  heatpump <- scen_building_residential(
    res_tb = ghg.ccap::building_data$residential,
    res_tb_bau = ghg.ccap::building_data$residential,
    .selected_ctu = "Saint Paul",
    .scenario = "heatpump",
    .density_output = run_scenario_land_use(
      tb = planned_land_use$ctu_planned_land_use_parcel,
      tb_strategy = NULL,
      .selected_ctu = "Saint Paul",
      .scenario = "heatpump"
    ),
    .grid_emissions = ghg.ccap::grid_emissions,
    .enviro_factors = ghg.ccap::enviro_factors,
    .leed_start_year = 2028,
    .new_sf_homes_leed_gold_pct = 0.0,
    .new_mf_homes_leed_gold_pct = 0.0,
    .retrofit_start_year = 2028,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0.0,
    .existing_mf_retrofit_pct = 0.00,
    .heatpump_start_year = 2028,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0.10,
    .mf_heat_pump_pct = 0.20,
    .baseline_year = 2025
  ) %>%
    mutate(scen_run = "heatpump")


  bau_none <- none %>%
    filter(scenario == "bau") %>%
    select(-scen_run)

  purrr::map(
    list(
      leed,
      heatpump,
      retrofit
    ),
    function(x) {
      x %>%
        filter(scenario == "bau") %>%
        select(-scen_run) %>%
        testthat::expect_equal(bau_none)
    }
  )



  purrr::map(
    list(
      leed,
      heatpump,
      retrofit
    ),
    function(x) {
      tb <- leed %>%
        group_by(scenario) %>%
        summarize(emissions = sum(electricity_emissions, natural_gas_emissions)) %>%
        pivot_wider(
          names_from = scenario,
          values_from = emissions
        )

      testthat::expect_gte(tb[1], tb[2])
    }
  )
})


test_that("Residential building energy runs", {
  none <- scen_building_residential(
    res_tb = ghg.ccap::building_data$residential,
    res_tb_bau = ghg.ccap::building_data$residential,
    .selected_ctu = "Afton",
    .scenario = "none",
    .density_output = run_scenario_land_use(
      tb = planned_land_use$ctu_planned_land_use_parcel,
      tb_strategy = NULL,
      .selected_ctu = "Afton",
      .scenario = "none"
    ),
    .grid_emissions = ghg.ccap::grid_emissions,
    .enviro_factors = ghg.ccap::enviro_factors,
    .leed_start_year = 2028,
    .new_sf_homes_leed_gold_pct = 0,
    .new_mf_homes_leed_gold_pct = 0,
    .retrofit_start_year = 2028,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0,
    .existing_mf_retrofit_pct = 0,
    .heatpump_start_year = 2028,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0,
    .mf_heat_pump_pct = 0,
    .baseline_year = 2025
  ) %>%
    mutate(scen_run = "none")


  leed <- scen_building_residential(
    res_tb = ghg.ccap::building_data$residential,
    res_tb_bau = ghg.ccap::building_data$residential,
    .selected_ctu = "Afton",
    .scenario = "leed",
    .density_output = run_scenario_land_use(
      tb = planned_land_use$ctu_planned_land_use_parcel,
      tb_strategy = NULL,
      .selected_ctu = "Afton",
      .scenario = "leed"
    ),
    .grid_emissions = ghg.ccap::grid_emissions,
    .enviro_factors = ghg.ccap::enviro_factors,
    .leed_start_year = 2028,
    .new_sf_homes_leed_gold_pct = 0.10,
    .new_mf_homes_leed_gold_pct = 0.50,
    .retrofit_start_year = 2028,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0,
    .existing_mf_retrofit_pct = 0,
    .heatpump_start_year = 2028,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0,
    .mf_heat_pump_pct = 0,
    .baseline_year = 2025
  ) %>%
    mutate(scen_run = "leed")



  retrofit <- scen_building_residential(
    res_tb = ghg.ccap::building_data$residential,
    res_tb_bau = ghg.ccap::building_data$residential,
    .selected_ctu = "Afton",
    .scenario = "leed",
    .density_output = run_scenario_land_use(
      tb = planned_land_use$ctu_planned_land_use_parcel,
      tb_strategy = NULL,
      .selected_ctu = "Afton",
      .scenario = "retrofit"
    ),
    .grid_emissions = ghg.ccap::grid_emissions,
    .enviro_factors = ghg.ccap::enviro_factors,
    .leed_start_year = 2028,
    .new_sf_homes_leed_gold_pct = 0.0,
    .new_mf_homes_leed_gold_pct = 0.0,
    .retrofit_start_year = 2028,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0.50,
    .existing_mf_retrofit_pct = 0.60,
    .heatpump_start_year = 2028,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0,
    .mf_heat_pump_pct = 0,
    .baseline_year = 2025
  ) %>%
    mutate(scen_run = "retrofit")


  heatpump <- scen_building_residential(
    res_tb = ghg.ccap::building_data$residential,
    res_tb_bau = ghg.ccap::building_data$residential,
    .selected_ctu = "Afton",
    .scenario = "heatpump",
    .density_output = run_scenario_land_use(
      tb = planned_land_use$ctu_planned_land_use_parcel,
      tb_strategy = NULL,
      .selected_ctu = "Afton",
      .scenario = "heatpump"
    ),
    .grid_emissions = ghg.ccap::grid_emissions,
    .enviro_factors = ghg.ccap::enviro_factors,
    .leed_start_year = 2028,
    .new_sf_homes_leed_gold_pct = 0.0,
    .new_mf_homes_leed_gold_pct = 0.0,
    .retrofit_start_year = 2028,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0.0,
    .existing_mf_retrofit_pct = 0.00,
    .heatpump_start_year = 2028,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0.10,
    .mf_heat_pump_pct = 0.20,
    .baseline_year = 2025
  ) %>%
    mutate(scen_run = "heatpump")


  bau_none <- none %>%
    filter(scenario == "bau") %>%
    select(-scen_run)

  purrr::map(
    list(
      leed,
      heatpump,
      retrofit
    ),
    function(x) {
      x %>%
        filter(scenario == "bau") %>%
        select(-scen_run) %>%
        testthat::expect_equal(bau_none)
    }
  )



  purrr::map(
    list(
      leed,
      heatpump,
      retrofit
    ),
    function(x) {
      tb <- leed %>%
        group_by(scenario) %>%
        summarize(emissions = sum(electricity_emissions, natural_gas_emissions)) %>%
        pivot_wider(
          names_from = scenario,
          values_from = emissions
        )

      testthat::expect_gte(tb[1], tb[2])
    }
  )
})



test_that("Residential building energy runs", {
  none <- scen_building_residential(
    res_tb = ghg.ccap::building_data$residential,
    res_tb_bau = ghg.ccap::building_data$residential,
    .selected_ctu = "Eagan",
    .scenario = "none",
    .density_output = run_scenario_land_use(
      tb = planned_land_use$ctu_planned_land_use_parcel,
      tb_strategy = NULL,
      .selected_ctu = "Eagan",
      .scenario = "none"
    ),
    .grid_emissions = ghg.ccap::grid_emissions,
    .enviro_factors = ghg.ccap::enviro_factors,
    .leed_start_year = 2028,
    .new_sf_homes_leed_gold_pct = 0,
    .new_mf_homes_leed_gold_pct = 0,
    .retrofit_start_year = 2028,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0,
    .existing_mf_retrofit_pct = 0,
    .heatpump_start_year = 2028,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0,
    .mf_heat_pump_pct = 0,
    .baseline_year = 2025
  ) %>%
    mutate(scen_run = "none")


  leed <- scen_building_residential(
    res_tb = ghg.ccap::building_data$residential,
    res_tb_bau = ghg.ccap::building_data$residential,
    .selected_ctu = "Eagan",
    .scenario = "leed",
    .density_output = run_scenario_land_use(
      tb = planned_land_use$ctu_planned_land_use_parcel,
      tb_strategy = NULL,
      .selected_ctu = "Eagan",
      .scenario = "leed"
    ),
    .grid_emissions = ghg.ccap::grid_emissions,
    .enviro_factors = ghg.ccap::enviro_factors,
    .leed_start_year = 2028,
    .new_sf_homes_leed_gold_pct = 0.10,
    .new_mf_homes_leed_gold_pct = 0.50,
    .retrofit_start_year = 2028,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0,
    .existing_mf_retrofit_pct = 0,
    .heatpump_start_year = 2028,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0,
    .mf_heat_pump_pct = 0,
    .baseline_year = 2025
  ) %>%
    mutate(scen_run = "leed")



  retrofit <- scen_building_residential(
    res_tb = ghg.ccap::building_data$residential,
    res_tb_bau = ghg.ccap::building_data$residential,
    .selected_ctu = "Eagan",
    .scenario = "leed",
    .density_output = run_scenario_land_use(
      tb = planned_land_use$ctu_planned_land_use_parcel,
      tb_strategy = NULL,
      .selected_ctu = "Eagan",
      .scenario = "retrofit"
    ),
    .grid_emissions = ghg.ccap::grid_emissions,
    .enviro_factors = ghg.ccap::enviro_factors,
    .leed_start_year = 2028,
    .new_sf_homes_leed_gold_pct = 0.0,
    .new_mf_homes_leed_gold_pct = 0.0,
    .retrofit_start_year = 2028,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0.50,
    .existing_mf_retrofit_pct = 0.60,
    .heatpump_start_year = 2028,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0,
    .mf_heat_pump_pct = 0,
    .baseline_year = 2025
  ) %>%
    mutate(scen_run = "retrofit")


  heatpump <- scen_building_residential(
    res_tb = ghg.ccap::building_data$residential,
    res_tb_bau = ghg.ccap::building_data$residential,
    .selected_ctu = "Eagan",
    .scenario = "heatpump",
    .density_output = run_scenario_land_use(
      tb = planned_land_use$ctu_planned_land_use_parcel,
      tb_strategy = NULL,
      .selected_ctu = "Eagan",
      .scenario = "heatpump"
    ),
    .grid_emissions = ghg.ccap::grid_emissions,
    .enviro_factors = ghg.ccap::enviro_factors,
    .leed_start_year = 2028,
    .new_sf_homes_leed_gold_pct = 0.0,
    .new_mf_homes_leed_gold_pct = 0.0,
    .retrofit_start_year = 2028,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0.0,
    .existing_mf_retrofit_pct = 0.00,
    .heatpump_start_year = 2028,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0.10,
    .mf_heat_pump_pct = 0.20,
    .baseline_year = 2025
  ) %>%
    mutate(scen_run = "heatpump")


  bau_none <- none %>%
    filter(scenario == "bau") %>%
    select(-scen_run)

  purrr::map(
    list(
      leed,
      heatpump,
      retrofit
    ),
    function(x) {
      x %>%
        filter(scenario == "bau") %>%
        select(-scen_run) %>%
        testthat::expect_equal(bau_none)
    }
  )



  purrr::map(
    list(
      leed,
      heatpump,
      retrofit
    ),
    function(x) {
      tb <- leed %>%
        group_by(scenario) %>%
        summarize(emissions = sum(electricity_emissions, natural_gas_emissions)) %>%
        pivot_wider(
          names_from = scenario,
          values_from = emissions
        )

      testthat::expect_gte(tb[1], tb[2])
    }
  )
})


test_that("Residential building energy runs", {
  none <- scen_building_residential(
    res_tb = ghg.ccap::building_data$residential,
    res_tb_bau = ghg.ccap::building_data$residential,
    .selected_ctu = "Shorewood",
    .scenario = "none",
    .density_output = run_scenario_land_use(
      tb = planned_land_use$ctu_planned_land_use_parcel,
      tb_strategy = NULL,
      .selected_ctu = "Shorewood",
      .scenario = "none"
    ),
    .grid_emissions = ghg.ccap::grid_emissions,
    .enviro_factors = ghg.ccap::enviro_factors,
    .leed_start_year = 2028,
    .new_sf_homes_leed_gold_pct = 0,
    .new_mf_homes_leed_gold_pct = 0,
    .retrofit_start_year = 2028,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0,
    .existing_mf_retrofit_pct = 0,
    .heatpump_start_year = 2028,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0,
    .mf_heat_pump_pct = 0,
    .baseline_year = 2025
  ) %>%
    mutate(scen_run = "none")


  leed <- scen_building_residential(
    res_tb = ghg.ccap::building_data$residential,
    res_tb_bau = ghg.ccap::building_data$residential,
    .selected_ctu = "Shorewood",
    .scenario = "leed",
    .density_output = run_scenario_land_use(
      tb = planned_land_use$ctu_planned_land_use_parcel,
      tb_strategy = NULL,
      .selected_ctu = "Shorewood",
      .scenario = "leed"
    ),
    .grid_emissions = ghg.ccap::grid_emissions,
    .enviro_factors = ghg.ccap::enviro_factors,
    .leed_start_year = 2028,
    .new_sf_homes_leed_gold_pct = 0.10,
    .new_mf_homes_leed_gold_pct = 0.50,
    .retrofit_start_year = 2028,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0,
    .existing_mf_retrofit_pct = 0,
    .heatpump_start_year = 2028,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0,
    .mf_heat_pump_pct = 0,
    .baseline_year = 2025
  ) %>%
    mutate(scen_run = "leed")



  retrofit <- scen_building_residential(
    res_tb = ghg.ccap::building_data$residential,
    res_tb_bau = ghg.ccap::building_data$residential,
    .selected_ctu = "Shorewood",
    .scenario = "leed",
    .density_output = run_scenario_land_use(
      tb = planned_land_use$ctu_planned_land_use_parcel,
      tb_strategy = NULL,
      .selected_ctu = "Shorewood",
      .scenario = "retrofit"
    ),
    .grid_emissions = ghg.ccap::grid_emissions,
    .enviro_factors = ghg.ccap::enviro_factors,
    .leed_start_year = 2028,
    .new_sf_homes_leed_gold_pct = 0.0,
    .new_mf_homes_leed_gold_pct = 0.0,
    .retrofit_start_year = 2028,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0.50,
    .existing_mf_retrofit_pct = 0.60,
    .heatpump_start_year = 2028,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0,
    .mf_heat_pump_pct = 0,
    .baseline_year = 2025
  ) %>%
    mutate(scen_run = "retrofit")


  heatpump <- scen_building_residential(
    res_tb = ghg.ccap::building_data$residential,
    res_tb_bau = ghg.ccap::building_data$residential,
    .selected_ctu = "Shorewood",
    .scenario = "heatpump",
    .density_output = run_scenario_land_use(
      tb = planned_land_use$ctu_planned_land_use_parcel,
      tb_strategy = NULL,
      .selected_ctu = "Shorewood",
      .scenario = "heatpump"
    ),
    .grid_emissions = ghg.ccap::grid_emissions,
    .enviro_factors = ghg.ccap::enviro_factors,
    .leed_start_year = 2028,
    .new_sf_homes_leed_gold_pct = 0.0,
    .new_mf_homes_leed_gold_pct = 0.0,
    .retrofit_start_year = 2028,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0.0,
    .existing_mf_retrofit_pct = 0.00,
    .heatpump_start_year = 2028,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0.10,
    .mf_heat_pump_pct = 0.20,
    .baseline_year = 2025
  ) %>%
    mutate(scen_run = "heatpump")


  bau_none <- none %>%
    filter(scenario == "bau") %>%
    select(-scen_run)

  purrr::map(
    list(
      leed,
      heatpump,
      retrofit
    ),
    function(x) {
      x %>%
        filter(scenario == "bau") %>%
        select(-scen_run) %>%
        testthat::expect_equal(bau_none)
    }
  )

  purrr::map(
    list(
      leed,
      heatpump,
      retrofit
    ),
    function(x) {
      tb <- leed %>%
        group_by(scenario) %>%
        summarize(emissions = sum(electricity_emissions, natural_gas_emissions)) %>%
        pivot_wider(
          names_from = scenario,
          values_from = emissions
        )

      testthat::expect_gte(tb[1], tb[2])
    }
  )
})


test_that("Residential building energy runs", {
  none <- scen_building_residential(
    res_tb = ghg.ccap::building_data$residential,
    res_tb_bau = ghg.ccap::building_data$residential,
    .selected_ctu = "Belle Plaine",
    .scenario = "none",
    .density_output = run_scenario_land_use(
      tb = planned_land_use$ctu_planned_land_use_parcel,
      tb_strategy = NULL,
      .selected_ctu = "Belle Plaine",
      .scenario = "none"
    ),
    .grid_emissions = ghg.ccap::grid_emissions,
    .enviro_factors = ghg.ccap::enviro_factors,
    .leed_start_year = 2028,
    .new_sf_homes_leed_gold_pct = 0,
    .new_mf_homes_leed_gold_pct = 0,
    .retrofit_start_year = 2028,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0,
    .existing_mf_retrofit_pct = 0,
    .heatpump_start_year = 2028,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0,
    .mf_heat_pump_pct = 0,
    .baseline_year = 2025
  ) %>%
    mutate(scen_run = "none")


  leed <- scen_building_residential(
    res_tb = ghg.ccap::building_data$residential,
    res_tb_bau = ghg.ccap::building_data$residential,
    .selected_ctu = "Belle Plaine",
    .scenario = "leed",
    .density_output = run_scenario_land_use(
      tb = planned_land_use$ctu_planned_land_use_parcel,
      tb_strategy = NULL,
      .selected_ctu = "Belle Plaine",
      .scenario = "leed"
    ),
    .grid_emissions = ghg.ccap::grid_emissions,
    .enviro_factors = ghg.ccap::enviro_factors,
    .leed_start_year = 2028,
    .new_sf_homes_leed_gold_pct = 0.10,
    .new_mf_homes_leed_gold_pct = 0.50,
    .retrofit_start_year = 2028,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0,
    .existing_mf_retrofit_pct = 0,
    .heatpump_start_year = 2028,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0,
    .mf_heat_pump_pct = 0,
    .baseline_year = 2025
  ) %>%
    mutate(scen_run = "leed")



  retrofit <- scen_building_residential(
    res_tb = ghg.ccap::building_data$residential,
    res_tb_bau = ghg.ccap::building_data$residential,
    .selected_ctu = "Belle Plaine",
    .scenario = "leed",
    .density_output = run_scenario_land_use(
      tb = planned_land_use$ctu_planned_land_use_parcel,
      tb_strategy = NULL,
      .selected_ctu = "Belle Plaine",
      .scenario = "retrofit"
    ),
    .grid_emissions = ghg.ccap::grid_emissions,
    .enviro_factors = ghg.ccap::enviro_factors,
    .leed_start_year = 2028,
    .new_sf_homes_leed_gold_pct = 0.0,
    .new_mf_homes_leed_gold_pct = 0.0,
    .retrofit_start_year = 2028,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0.50,
    .existing_mf_retrofit_pct = 0.60,
    .heatpump_start_year = 2028,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0,
    .mf_heat_pump_pct = 0,
    .baseline_year = 2025
  ) %>%
    mutate(scen_run = "retrofit")


  heatpump <- scen_building_residential(
    res_tb = ghg.ccap::building_data$residential,
    res_tb_bau = ghg.ccap::building_data$residential,
    .selected_ctu = "Belle Plaine",
    .scenario = "heatpump",
    .density_output = run_scenario_land_use(
      tb = planned_land_use$ctu_planned_land_use_parcel,
      tb_strategy = NULL,
      .selected_ctu = "Belle Plaine",
      .scenario = "heatpump"
    ),
    .grid_emissions = ghg.ccap::grid_emissions,
    .enviro_factors = ghg.ccap::enviro_factors,
    .leed_start_year = 2028,
    .new_sf_homes_leed_gold_pct = 0.0,
    .new_mf_homes_leed_gold_pct = 0.0,
    .retrofit_start_year = 2028,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0.0,
    .existing_mf_retrofit_pct = 0.00,
    .heatpump_start_year = 2028,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0.10,
    .mf_heat_pump_pct = 0.20,
    .baseline_year = 2025
  ) %>%
    mutate(scen_run = "heatpump")


  bau_none <- none %>%
    filter(scenario == "bau") %>%
    select(-scen_run)

  purrr::map(
    list(
      leed,
      heatpump,
      retrofit
    ),
    function(x) {
      x %>%
        filter(scenario == "bau") %>%
        select(-scen_run) %>%
        testthat::expect_equal(bau_none)
    }
  )

  purrr::map(
    list(
      leed,
      heatpump,
      retrofit
    ),
    function(x) {
      tb <- leed %>%
        group_by(scenario) %>%
        summarize(emissions = sum(electricity_emissions, natural_gas_emissions)) %>%
        pivot_wider(
          names_from = scenario,
          values_from = emissions
        )

      testthat::expect_gte(tb[1], tb[2])
    }
  )
})

