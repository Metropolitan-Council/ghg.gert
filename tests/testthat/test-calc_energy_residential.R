# test-calc_energy_residential.R


# helper: run a single scenario
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


# efficiency: both mwh and mcf should decrease
test_energy_efficiency <- function(en_table) {
  at_2050 <- en_table %>%
    dplyr::filter(emissions_year == 2050)
browser()
  at_2050 %>%
    dplyr::select(geog_name, scenario, residential_mwh) %>%
    tidyr::pivot_wider(names_from = scenario, values_from = residential_mwh) %>%
    dplyr::filter(bau > alt) %>%
    nrow() %>%
    expect_equal(1)

  at_2050 %>%
    dplyr::select(geog_name, scenario, residential_mcf) %>%
    tidyr::pivot_wider(names_from = scenario, values_from = residential_mcf) %>%
    dplyr::filter(bau > alt) %>%
    nrow() %>%
    expect_equal(1)
}


# electrification: mwh increases (heat pump load), mcf decreases
test_energy_electrification <- function(en_table) {
  at_2050 <- en_table %>%
    dplyr::filter(emissions_year == 2050)

  at_2050 %>%
    dplyr::select(geog_name, scenario, residential_mwh) %>%
    tidyr::pivot_wider(names_from = scenario, values_from = residential_mwh) %>%
    dplyr::filter(bau < alt) %>%
    nrow() %>%
    expect_equal(1)

  at_2050 %>%
    dplyr::select(geog_name, scenario, residential_mcf) %>%
    tidyr::pivot_wider(names_from = scenario, values_from = residential_mcf) %>%
    dplyr::filter(bau > alt) %>%
    nrow() %>%
    expect_equal(1)
}

# propane should track mcf direction (same strategy reductions apply)
test_propane_tracks_mcf <- function(en_table) {
  at_2050 <- en_table %>%
    dplyr::filter(emissions_year == 2050)

  mcf_wide <- at_2050 %>%
    dplyr::select(geog_name, scenario, residential_mcf) %>%
    tidyr::pivot_wider(names_from = scenario, values_from = residential_mcf)

  prop_wide <- at_2050 %>%
    dplyr::select(geog_name, scenario, residential_propane_mmbtu) %>%
    tidyr::pivot_wider(names_from = scenario, values_from = residential_propane_mmbtu)

  # if mcf decreased, propane should not increase
  if (nrow(mcf_wide) > 0 && mcf_wide$bau[1] > mcf_wide$alt[1]) {
    expect_true(prop_wide$bau[1] >= prop_wide$alt[1])
  }
}


test_energy_residential <- function(ctu) {
  test_that(paste("Energy reduces with efficiency strategies -", ctu), {
    retro_table <- run_res(ctu, sf_retro = 0.3, mf_retro = 0.3)
    test_energy_efficiency(retro_table)
  })

  test_that(paste("Electrification shifts fuel mix -", ctu), {
    heatpump_table <- run_res(ctu, sf_hp = 0.3, mf_hp = 0.3)
    combo_table    <- run_res(ctu,
                              sf_leed = 0.3, mf_leed = 0.3,
                              sf_retro = 0.4, mf_retro = 0.4,
                              sf_hp = 0.5, mf_hp = 0.5
    )
    purrr::walk(list(heatpump_table, combo_table), test_energy_electrification)
  })

  test_that(paste("Propane tracks natgas direction -", ctu), {
    heatpump_table <- run_res(ctu, sf_hp = 0.3, mf_hp = 0.3)
    test_propane_tracks_mcf(heatpump_table)
  })
}

purrr::walk(geography_test_list, test_energy_residential)
