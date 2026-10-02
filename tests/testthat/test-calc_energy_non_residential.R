# test-calc_energy_non_residential.R


# helper: run a single non-residential scenario
run_nonres <- function(ctu, leed = 0, retro = 0, hp = 0) {
  scen_building_non_residential(
    non_res_tb     = building_energy_data$non_residential,
    non_res_tb_bau = building_energy_data$non_residential,
    .scenario            = "alt",
    .selected_ctu        = ctu,
    .baseline_year       = 2022,
    .new_jobs_leed_gold_pct     = leed,
    .leed_start_year            = 2028,
    .existing_jobs_retrofit_pct = retro,
    .retrofit_start_year        = 2028,
    .retrofit_end_year          = 2050,
    .jobs_heatpump_pct          = hp,
    .heatpump_start_year        = 2028,
    .heatpump_end_year          = 2050
  )
}


# efficiency: both mwh and mcf should decrease
test_energy_efficiency_nonres <- function(en_table) {
  at_2050 <- en_table %>%
    dplyr::filter(emissions_year == 2050)

  at_2050 %>%
    dplyr::select(geog_name, scenario, non_residential_mwh) %>%
    tidyr::pivot_wider(names_from = scenario, values_from = non_residential_mwh) %>%
    dplyr::filter(bau > alt) %>%
    nrow() %>%
    expect_equal(1)

  at_2050 %>%
    dplyr::select(geog_name, scenario, non_residential_mcf) %>%
    tidyr::pivot_wider(names_from = scenario, values_from = non_residential_mcf) %>%
    dplyr::filter(bau > alt) %>%
    nrow() %>%
    expect_equal(1)
}


# electrification: mwh increases (heat pump load), mcf decreases
test_energy_electrification_nonres <- function(en_table) {
  at_2050 <- en_table %>%
    dplyr::filter(emissions_year == 2050)

  at_2050 %>%
    dplyr::select(geog_name, scenario, non_residential_mwh) %>%
    tidyr::pivot_wider(names_from = scenario, values_from = non_residential_mwh) %>%
    dplyr::filter(bau < alt) %>%
    nrow() %>%
    expect_equal(1)

  at_2050 %>%
    dplyr::select(geog_name, scenario, non_residential_mcf) %>%
    tidyr::pivot_wider(names_from = scenario, values_from = non_residential_mcf) %>%
    dplyr::filter(bau > alt) %>%
    nrow() %>%
    expect_equal(1)
}


test_energy_non_residential <- function(ctu) {
  test_that(paste("Non-res energy reduces with retrofit -", ctu), {
    retro_table <- run_nonres(ctu, retro = 0.3)
    test_energy_efficiency_nonres(retro_table)
  })

  test_that(paste("Non-res electrification shifts fuel mix -", ctu), {
    heatpump_table <- run_nonres(ctu, hp = 0.3)
    combo_table    <- run_nonres(ctu, leed = 0.3, retro = 0.4, hp = 0.5)
    purrr::walk(
      list(heatpump_table, combo_table),
      test_energy_electrification_nonres
    )
  })
}

purrr::walk(geography_test_list, test_energy_non_residential)
