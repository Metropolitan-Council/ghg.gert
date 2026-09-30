# test-scen_building_non_residential.R

# helper: run four strategy scenarios for a given CTU
run_nonres_scenarios <- function(ctu) {
  shared_args <- list(
    non_res_tb     = building_energy_data$non_residential,
    non_res_tb_bau = building_energy_data$non_residential,
    .selected_ctu        = ctu,
    .baseline_year       = 2022,
    .leed_start_year     = 2028,
    .retrofit_start_year = 2028,
    .retrofit_end_year   = 2050,
    .heatpump_start_year = 2028,
    .heatpump_end_year   = 2050
  )

  zero_args <- list(
    .new_jobs_leed_gold_pct    = 0,
    .existing_jobs_retrofit_pct = 0,
    .jobs_heatpump_pct         = 0
  )

  run_one <- function(scenario_name, overrides = list()) {
    args <- modifyList(c(shared_args, zero_args, list(.scenario = scenario_name)), overrides)
    do.call(scen_building_non_residential, args)
  }

  list(
    none     = run_one("none"),
    leed     = run_one("leed", list(.new_jobs_leed_gold_pct = 0.20)),
    retrofit = run_one("retrofit", list(.existing_jobs_retrofit_pct = 0.50)),
    heatpump = run_one("heatpump", list(.jobs_heatpump_pct = 0.30))
  )
}


# helper: total emissions across fuel types
total_emissions <- function(tb) {
  tb %>%
    dplyr::group_by(scenario) %>%
    dplyr::summarize(
      emissions = sum(
        electricity_emissions + natural_gas_emissions,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}


# parameterized tests across geographies ----

test_scen_non_residential <- function(ctu) {
  test_that(paste("Non-residential scenarios run for", ctu), {
    results <- run_nonres_scenarios(ctu)

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

  test_that(paste("Non-residential output has expected columns for", ctu), {
    results <- run_nonres_scenarios(ctu)
    out <- results$heatpump

    expect_true("non_residential_mwh" %in% names(out))
    expect_true("non_residential_mcf" %in% names(out))
    expect_true("electricity_emissions" %in% names(out))
    expect_true("natural_gas_emissions" %in% names(out))

    # no NA energy or emissions values
    expect_false(any(is.na(out$non_residential_mwh)))
    expect_false(any(is.na(out$non_residential_mcf)))
    expect_false(any(is.na(out$electricity_emissions)))
    expect_false(any(is.na(out$natural_gas_emissions)))
  })
}

purrr::walk(geography_test_list, test_scen_non_residential)
