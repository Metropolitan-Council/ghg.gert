# test-scen_building_residential.R

# CTU-level tests only — filter out county entries

# helper: run the four strategy scenarios for a given CTU
run_res_scenarios <- function(ctu) {
  density <- run_scenario_land_use(
    tb = planned_land_use$ctu_planned_land_use_parcel,
    tb_strategy = NULL,
    .selected_ctu = ctu,
    .scenario = "none"
  )

  shared_args <- list(
    res_tb       = building_energy_data$residential,
    res_tb_bau   = building_energy_data$residential,
    .selected_ctu      = ctu,
    .density_output    = density,
    .baseline_year     = 2022,
    .leed_start_year   = 2028,
    .retrofit_start_year = 2028,
    .retrofit_end_year   = 2050,
    .heatpump_start_year = 2028,
    .heatpump_end_year   = 2050
  )

  zero_args <- list(
    .new_sf_homes_leed_gold_pct = 0,
    .new_mf_homes_leed_gold_pct = 0,
    .existing_sf_retrofit_pct   = 0,
    .existing_mf_retrofit_pct   = 0,
    .sf_heatpump_pct = 0,
    .mf_heatpump_pct = 0
  )

  run_one <- function(scenario_name, overrides = list()) {
    args <- modifyList(c(shared_args, zero_args, list(.scenario = scenario_name)), overrides)
    do.call(scen_building_residential, args)
  }

  list(
    none     = run_one("none"),
    leed     = run_one("leed", list(
      .new_sf_homes_leed_gold_pct = 0.10,
      .new_mf_homes_leed_gold_pct = 0.50
    )),
    retrofit = run_one("retrofit", list(
      .existing_sf_retrofit_pct = 0.50,
      .existing_mf_retrofit_pct = 0.60
    )),
    heatpump = run_one("heatpump", list(
      .sf_heatpump_pct = 0.10,
      .mf_heatpump_pct = 0.20
    ))
  )
}


# helper: total emissions across all three fuel types
total_emissions <- function(tb) {
  tb %>%
    dplyr::group_by(scenario) %>%
    dplyr::summarize(
      emissions = sum(
        electricity_emissions +
          natural_gas_emissions +
          liquid_fuel_emissions,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}


# parameterized tests across CTUs ----

test_scen_residential <- function(ctu) {
  test_that(paste("Residential scenarios run for", ctu), {
    results <- run_res_scenarios(ctu)

    # BAU should be identical regardless of which strategy scenario produced it
    bau_none <- results$none %>%
      dplyr::filter(scenario == "bau")

    purrr::walk(
      list(results$leed, results$retrofit, results$heatpump),
      function(x) {
        expect_equal(
          x %>% dplyr::filter(scenario == "bau"),
          bau_none
        )
      }
    )

    # strategy scenario should have <= BAU total emissions
    purrr::walk(
      list(results$leed, results$retrofit, results$heatpump),
      function(x) {
        em <- total_emissions(x) %>%
          tidyr::pivot_wider(names_from = scenario, values_from = emissions)
        expect_gte(em$bau, em[[2]])
      }
    )
  })

  test_that(paste("Propane/kerosene columns present and >= 0 for", ctu), {
    results <- run_res_scenarios(ctu)
    out <- results$heatpump

    expect_true("residential_propane_mmbtu" %in% names(out))
    expect_true("residential_kerosene_mmbtu" %in% names(out))
    expect_true("liquid_fuel_emissions" %in% names(out))
    expect_true(all(out$residential_propane_mmbtu >= 0, na.rm = TRUE))
    expect_true(all(out$residential_kerosene_mmbtu >= 0, na.rm = TRUE))
    expect_true(all(out$liquid_fuel_emissions >= 0, na.rm = TRUE))
  })
}

purrr::walk(geography_test_list, test_scen_residential)
