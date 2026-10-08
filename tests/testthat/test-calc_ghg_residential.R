# test-calc_ghg_residential.R


# helper: run a single scen_building_residential call
run_res <- function(ctu, sf_leed = 0, mf_leed = 0,
                    sf_retro = 0, mf_retro = 0,
                    sf_hp = 0, mf_hp = 0) {
  scen_building_residential(
    res_tb       = building_energy_data$residential,
    res_tb_bau   = building_energy_data$residential,
    .density_output = run_scenario_land_use(.selected_ctu = ctu),
    .scenario    = "alt",
    .selected_ctu = ctu,
    .baseline_year = 2022,
    .leed_start_year     = 2028,
    .new_sf_homes_leed_gold_pct = sf_leed,
    .new_mf_homes_leed_gold_pct = mf_leed,
    .retrofit_start_year = 2028,
    .retrofit_end_year   = 2050,
    .existing_sf_retrofit_pct = sf_retro,
    .existing_mf_retrofit_pct = mf_retro,
    .heatpump_start_year = 2028,
    .heatpump_end_year   = 2050,
    .sf_heatpump_pct = sf_hp,
    .mf_heatpump_pct = mf_hp
  )
}

test_emissions <- function(ghg_table) {
  ghg_table %>%
    dplyr::filter(emissions_year == 2050) %>%
    dplyr::group_by(geog_name, scenario) %>%
    dplyr::summarize(
      emissions = sum(
        electricity_emissions +
          natural_gas_emissions +
          liquid_fuel_emissions,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(names_from = scenario, values_from = emissions) %>%
    dplyr::filter(bau >= alt) %>%
    nrow() %>%
    testthat::expect_equal(1)
}


test_ghg_residential <- function(ctu) {
  test_that(paste("Emissions reduce with interventions -", ctu), {
    leed_table     <- run_res(ctu, sf_leed = 0.3, mf_leed = 0.3)
    retro_table    <- run_res(ctu, sf_retro = 0.3, mf_retro = 0.3)
    heatpump_table <- run_res(ctu, sf_hp = 0.3, mf_hp = 0.3)
    combo_table    <- run_res(ctu,
                              sf_leed = 0.3, mf_leed = 0.3,
                              sf_retro = 0.4, mf_retro = 0.4,
                              sf_hp = 0.5, mf_hp = 0.5
    )

    purrr::walk(
      list(leed_table, retro_table, heatpump_table, combo_table),
      test_emissions
    )
  })
}

purrr::walk(geography_test_list, test_ghg_residential)
