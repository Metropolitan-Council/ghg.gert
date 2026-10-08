# test-calc_ghg_non_residential.R


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

test_emissions_nonres <- function(ghg_table) {
  ghg_table %>%
    dplyr::filter(emissions_year == 2050) %>%
    dplyr::group_by(geog_name, scenario) %>%
    dplyr::summarize(
      emissions = sum(
        electricity_emissions + natural_gas_emissions,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(names_from = scenario, values_from = emissions) %>%
    dplyr::filter(bau >= alt) %>%
    nrow() %>%
    testthat::expect_equal(1)
}


test_ghg_non_residential <- function(ctu) {
  test_that(paste("Non-res emissions reduce with interventions -", ctu), {
    leed_table     <- run_nonres(ctu, leed = 0.3)
    retro_table    <- run_nonres(ctu, retro = 0.3)
    heatpump_table <- run_nonres(ctu, hp = 0.3)
    combo_table    <- run_nonres(ctu, leed = 0.3, retro = 0.4, hp = 0.5)

    purrr::walk(
      list(leed_table, retro_table, heatpump_table, combo_table),
      test_emissions_nonres
    )
  })
}

purrr::walk(geography_test_list, test_ghg_non_residential)
