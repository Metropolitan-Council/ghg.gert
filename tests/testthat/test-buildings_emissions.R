# business as usual

testthat::test_that("Minneapolis strategies reduce emissions", {
  mpls_decarb_grid <- calc_ghg_residential(
    res_tb = building_data$residential,
    res_tb_bau = building_data$residential,
    .selected_ctu = "Minneapolis",
    .grid_decarbonization_pct = 1,
    .enviro_factors = enviro_factors
  )


  mpls_electric_heat <- calc_electrify_residential_heating(
    res_tb = calc_ghg_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .selected_ctu = "Minneapolis",
      .grid_decarbonization_pct = 0.80,
      .enviro_factors = enviro_factors
    ),
    .selected_ctu = "Minneapolis",
    .additional_electrified_residential_buildings_pct = 0.45,
    .grid_decarbonization_pct = 0.80,
    .enviro_factors = enviro_factors
  )

  mpls_residential_ng <- calc_residential_renewable_ng(
    res_tb = calc_ghg_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .selected_ctu = "Minneapolis",
      .grid_decarbonization_pct = 1,
      .enviro_factors = enviro_factors
    ),
    .selected_ctu = "Minneapolis",
    .renewable_ng_res = TRUE,
    .enviro_factors = enviro_factors
  )



  # non_residential

  check <- calc_ghg_non_residential(
    non_res_tb = calc_existing_comm_building_efficiency(
      non_res_tb = building_data$non_residential,
      .existing_high_efficiency_buildings_pct = 0.80,
      .selected_ctu = "Minneapolis"
    ),
    non_res_tb_bau = building_data$non_residential,
    .selected_ctu = "Minneapolis",
    .grid_decarbonization_pct = 0.8,
    .smart_grid_energy_reduction_pct = 1,
    .enviro_factors = enviro_factors,
    .existing_high_efficiency_buildings_pct = 0.8
  )



  mpls_nonres_heating <- calc_electrify_commercial_heating(
    non_res_tb = calc_ghg_non_residential(
      non_res_tb = building_data$non_residential,
      non_res_tb_bau = building_data$non_residential,
      .selected_ctu = "Minneapolis",
      .grid_decarbonization_pct = 1,
      .smart_grid_energy_reduction_pct = 1,
      .enviro_factors = enviro_factors,
      .existing_high_efficiency_buildings_pct = 0.8
    ),
    .selected_ctu = "Minneapolis",
    .grid_decarbonization_pct = 0.8,
    .electrified_buildings_pct = 0.40,
    .enviro_factors = enviro_factors
  )

  mpls_nonres_reweable <- calc_non_res_renewable_ng(
    .renewable_ng_nonres = TRUE,
    non_res_tb = calc_ghg_non_residential(
      non_res_tb = building_data$non_residential,
      non_res_tb_bau = building_data$non_residential,
      .selected_ctu = "Minneapolis",
      .grid_decarbonization_pct = 0.8,
      .smart_grid_energy_reduction_pct = 1,
      .enviro_factors = enviro_factors,
      .existing_high_efficiency_buildings_pct = 0.8
    ),
    .selected_ctu = "Minneapolis",
    .enviro_factors = enviro_factors
  )



  mpls_mix <- scen_building_non_residential(
    .renewable_ng_nonres = TRUE,
    non_res_tb = building_data$non_residential,
    non_res_tb_bau = building_data$non_residential,
    .selected_ctu = "Minneapolis",
    .electrified_buildings_pct = 0.40,
    .smart_grid_energy_reduction_pct = 1.00,
    .grid_decarbonization_pct = 0.80,
    .existing_high_efficiency_buildings_pct = 0.80,
    .enviro_factors = enviro_factors
  )




  purrr::map(
    list(
      mpls_decarb_grid,
      check,
      mpls_nonres_reweable,
      mpls_nonres_heating,
      mpls_residential_ng,
      mpls_mix
    ),
    function(x) {
      test_df <- x %>%
        group_by(scen) %>%
        pivot_wider(
          names_from = scen,
          values_from = value
        ) %>%
        filter(
          year == 2040,
          var %in% c(
            "total_residential_emissions",
            "total_industrial_commercial_emissions"
          )
        )
      testthat::expect_lt(test_df$scen, test_df$bau)
    }
  )


  testthat::expect_warning(
    scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .selected_ctu = "Minneapolis",
      .renewable_ng_res = FALSE,
      .new_homes_to_multifamily_pct = 0.50,
      .single_family_floor_area_growth_pct = 0.05,
      .new_homes_affected_pct = 0.50,
      .new_homes_leed_gold_pct = 0.50,
      .existing_home_retrofit_pct = 0.80,
      .existing_home_ultra_retrofit_pct = 0.20,
      .home_behavior_change_pct = 1.00,
      .grid_decarbonization_pct = 1,
      .additional_electrified_residential_buildings_pct = 0.45,
      .enviro_factors = enviro_factors
    )
  )
})



testthat::test_that("Maplewood strategies reduce emissions", {
  mpls_decarb_grid <- calc_ghg_residential(
    res_tb = building_data$residential,
    res_tb_bau = building_data$residential,
    .selected_ctu = "Maplewood",
    .grid_decarbonization_pct = 1,
    .enviro_factors = enviro_factors
  )


  mpls_electric_heat <- calc_electrify_residential_heating(
    res_tb = calc_ghg_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .selected_ctu = "Maplewood",
      .grid_decarbonization_pct = 0.80,
      .enviro_factors = enviro_factors
    ),
    .selected_ctu = "Maplewood",
    .additional_electrified_residential_buildings_pct = 0.45,
    .grid_decarbonization_pct = 0.80,
    .enviro_factors = enviro_factors
  )

  mpls_residential_ng <- calc_residential_renewable_ng(
    res_tb = calc_ghg_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .selected_ctu = "Maplewood",
      .grid_decarbonization_pct = 1,
      .enviro_factors = enviro_factors
    ),
    .selected_ctu = "Maplewood",
    .renewable_ng_res = TRUE,
    .enviro_factors = enviro_factors
  )



  # non_residential

  check <- calc_ghg_non_residential(
    non_res_tb = calc_existing_comm_building_efficiency(
      non_res_tb = building_data$non_residential,
      .existing_high_efficiency_buildings_pct = 0.80,
      .selected_ctu = "Maplewood"
    ),
    non_res_tb_bau = building_data$non_residential,
    .selected_ctu = "Maplewood",
    .grid_decarbonization_pct = 0.8,
    .smart_grid_energy_reduction_pct = 1,
    .enviro_factors = enviro_factors,
    .existing_high_efficiency_buildings_pct = 0.8
  )



  mpls_nonres_heating <- calc_electrify_commercial_heating(
    non_res_tb = calc_ghg_non_residential(
      non_res_tb = building_data$non_residential,
      non_res_tb_bau = building_data$non_residential,
      .selected_ctu = "Maplewood",
      .grid_decarbonization_pct = 1,
      .smart_grid_energy_reduction_pct = 1,
      .enviro_factors = enviro_factors,
      .existing_high_efficiency_buildings_pct = 0.8
    ),
    .selected_ctu = "Maplewood",
    .grid_decarbonization_pct = 0.8,
    .electrified_buildings_pct = 0.40,
    .enviro_factors = enviro_factors
  )

  mpls_nonres_reweable <- calc_non_res_renewable_ng(
    .renewable_ng_nonres = TRUE,
    non_res_tb = calc_ghg_non_residential(
      non_res_tb = building_data$non_residential,
      non_res_tb_bau = building_data$non_residential,
      .selected_ctu = "Maplewood",
      .grid_decarbonization_pct = 0.8,
      .smart_grid_energy_reduction_pct = 1,
      .enviro_factors = enviro_factors,
      .existing_high_efficiency_buildings_pct = 0.8
    ),
    .selected_ctu = "Maplewood",
    .enviro_factors = enviro_factors
  )



  mpls_mix <- scen_building_non_residential(
    .renewable_ng_nonres = TRUE,
    non_res_tb = building_data$non_residential,
    non_res_tb_bau = building_data$non_residential,
    .selected_ctu = "Maplewood",
    .electrified_buildings_pct = 0.40,
    .smart_grid_energy_reduction_pct = 1.00,
    .grid_decarbonization_pct = 0.80,
    .existing_high_efficiency_buildings_pct = 0.80,
    .enviro_factors = enviro_factors
  )




  purrr::map(
    list(
      mpls_decarb_grid,
      check,
      mpls_nonres_reweable,
      mpls_nonres_heating,
      mpls_residential_ng,
      mpls_mix
    ),
    function(x) {
      test_df <- x %>%
        group_by(scen) %>%
        pivot_wider(
          names_from = scen,
          values_from = value
        ) %>%
        filter(
          year == 2040,
          var %in% c(
            "total_residential_emissions",
            "total_industrial_commercial_emissions"
          )
        )
      testthat::expect_lt(test_df$scen, test_df$bau)
    }
  )


  testthat::expect_no_warning(
    scen_building_residential(
      res_tb = building_data$residential,
      res_tb_bau = building_data$residential,
      .selected_ctu = "Maplewood",
      .renewable_ng_res = FALSE,
      .new_homes_to_multifamily_pct = 0.50,
      .single_family_floor_area_growth_pct = 0.05,
      .new_homes_affected_pct = 0.50,
      .new_homes_leed_gold_pct = 0.50,
      .existing_home_retrofit_pct = 0.80,
      .existing_home_ultra_retrofit_pct = 0.20,
      .home_behavior_change_pct = 1.00,
      .grid_decarbonization_pct = 1,
      .additional_electrified_residential_buildings_pct = 0.45,
      .enviro_factors = enviro_factors
    )
  )
})
