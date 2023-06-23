testthat::test_that("Minneapolis baseline buildings data returned is correct", {
  # demographic baseline -----

  mpls_demo_baseline <- get_demographic_baseline(
    tb = building_energy_data,
    .selected_ctu = "Minneapolis"
  )

  # expected vars
  testthat::expect_equal(
    sort(unique(mpls_demo_baseline$county$var)),
    sort(c(
      "single_family_average_floor_area_sqft_county",
      "multifamily_average_floor_area_sqft_county",
      "commercial_workers_county", "industrial_workers_county"
    ))
  )

  testthat::expect_equal(
    sort(unique(mpls_demo_baseline$ctu$var)),
    sort(c(
      "households", "population", "jobs",
      "commercial_jobs",
      "industrial_jobs",
      "single_family_units",
      "multifamily_units",
      "single_family_average_floor_area_sqft_ctu",
      "multifamily_average_floor_area_sqft_ctu",
      "multifamily_average_floor_area_sqft_county"
    ))
  )


  testthat::expect_equal(nrow(mpls_demo_baseline$county), 28)
  testthat::expect_equal(nrow(mpls_demo_baseline$ctu), 10)


  # mpls_res_energy -----
  mpls_res_energy <- get_residential_energy_baseline(
    tb = building_energy_data,
    .selected_ctu = "Minneapolis"
  )

  testthat::expect_equal(
    sort(unique(mpls_res_energy$var)),
    sort(c(
      "residential_mwh", "residential_elec_emis_t_co2e", "residential_ng_therms",
      "residential_ng_emis_t_co2e", "residential_kwh_per_floor_area",
      "residential_therms_per_floor_area", "residential_mwh_per_households",
      "residential_therms_per_households"
    ))
  )

  testthat::expect_equal(nrow(mpls_res_energy), 8)


  # county_non_res_energy ----
  county_non_res_energy <- get_by_county_non_residential_energy_baseline(
    tb = building_energy_data,
    .selected_ctu = "Minneapolis"
  )

  testthat::expect_equal(
    sort(unique(county_non_res_energy$var)),
    sort(c(
      "commercial_mwh_per_worker_county",
      "industrial_mwh_per_worker_county"
    ))
  )
  testthat::expect_equal(nrow(county_non_res_energy), 14)

  # state_non_res_energy -----
  state_non_res_energy <- get_statewide_non_residential_energy(
    tb = building_energy_data,
    .selected_ctu = "Minneapolis"
  )

  testthat::expect_equal(
    sort(unique(state_non_res_energy$var)),
    sort(c(
      "electricity_residential_consumption_mwh_state",
      "electricity_commercial_consumption_mwh_state",
      "electricity_industrial_consumption_mwh_state",
      "commercial_employees_state",
      "industrial_employees_state",
      "commercial_mwh_per_worker_state",
      "industrial_mwh_per_worker_state",
      "commercial_therms_per_worker_state",
      "industrial_therms_per_worker_state"
    ))
  )
  testthat::expect_equal(nrow(state_non_res_energy), 9)


  # mpls_non_res_energy -----
  mpls_non_res_energy <- get_non_residential_energy_baseline(
    tb = building_energy_data,
    .selected_ctu = "Minneapolis"
  )
  testthat::expect_equal(
    sort(unique(mpls_non_res_energy$var)),
    sort(c(
      "commercial_therms", "industrial_therms",
      "commercial_mwh",
      "industrial_mwh",
      "commercial_therm_per_worker",
      "industrial_therm_per_worker",
      "commercial_mwh_per_worker",
      "industrial_mwh_per_worker"
    ))
  )
  testthat::expect_equal(nrow(mpls_non_res_energy), 8)

  # mpls_non_res_xcel
  mpls_non_res_xcel <- get_by_ctu_non_residential_xcel_energy_baseline(
    tb = building_energy_data,
    .selected_ctu = "Minneapolis"
  )

  testthat::expect_equal(
    sort(unique(mpls_non_res_xcel$var)),
    sort(c(
      "commercial_mwh_xcel",
      "industrial_mwh_xcel"
    ))
  )

  testthat::expect_equal(nrow(mpls_non_res_xcel), 2)


  # test for county and ctu  ------
  # expected counties
  purrr::map(
    list(
      county_non_res_energy,
      mpls_demo_baseline$county
    ),
    function(x) {
      testthat::expect_equal(
        sort(unique(x$co_name)),
        c(
          "Anoka", "Carver",
          "Dakota", "Hennepin",
          "Ramsey", "Scott",
          "Washington"
        )
      )
    }
  )


  # expected ctu
  purrr::map(
    list(
      mpls_demo_baseline[[2]],
      mpls_res_energy,
      mpls_non_res_energy,
      mpls_non_res_xcel
    ),
    function(x) {
      testthat::expect_equal(unique(x$ctu_name), "Minneapolis")
    }
  )

  # expected year
  purrr::map(
    list(
      mpls_demo_baseline$county,
      mpls_demo_baseline$ctu,
      mpls_res_energy,
      mpls_non_res_energy,
      mpls_non_res_xcel,
      county_non_res_energy,
      state_non_res_energy
    ),
    function(x) {
      testthat::expect_equal(unique(x$year), 2018)
    }
  )
})


testthat::test_that("Woodbury baseline buildings data returned is correct", {
  # demographic baseline -----

  wdbry_demo_baseline <- get_demographic_baseline(
    tb = building_energy_data,
    .selected_ctu = "Woodbury"
  )

  # expected vars
  testthat::expect_equal(
    unique(wdbry_demo_baseline$county$var),
    c(
      "single_family_average_floor_area_sqft_county",
      "multifamily_average_floor_area_sqft_county",
      "commercial_workers_county", "industrial_workers_county"
    )
  )

  testthat::expect_equal(
    sort(unique(wdbry_demo_baseline$ctu$var)),
    sort(c(
      "households", "population", "jobs",
      "commercial_jobs",
      "industrial_jobs",
      "single_family_units",
      "multifamily_units",
      "single_family_average_floor_area_sqft_ctu",
      "multifamily_average_floor_area_sqft_ctu",
      "multifamily_average_floor_area_sqft_county"
    ))
  )

  wdbry_demo_baseline$ctu

  testthat::expect_equal(nrow(wdbry_demo_baseline$county), 28)
  testthat::expect_equal(nrow(wdbry_demo_baseline$ctu), 10)


  # wdbry_res_energy -----
  wdbry_res_energy <- get_residential_energy_baseline(
    tb = building_energy_data,
    .selected_ctu = "Woodbury"
  )

  testthat::expect_equal(
    unique(wdbry_res_energy$var),
    c(
      "residential_mwh", "residential_elec_emis_t_co2e", "residential_ng_therms",
      "residential_ng_emis_t_co2e", "residential_kwh_per_floor_area",
      "residential_therms_per_floor_area", "residential_mwh_per_households",
      "residential_therms_per_households"
    )
  )

  testthat::expect_equal(nrow(wdbry_res_energy), 8)


  # county_non_res_energy ----
  county_non_res_energy <- get_by_county_non_residential_energy_baseline(
    tb = building_energy_data,
    .selected_ctu = "Woodbury"
  )

  testthat::expect_equal(
    unique(county_non_res_energy$var),
    c(
      "commercial_mwh_per_worker_county",
      "industrial_mwh_per_worker_county"
    )
  )
  testthat::expect_equal(nrow(county_non_res_energy), 14)

  # state_non_res_energy -----
  state_non_res_energy <- get_statewide_non_residential_energy(
    tb = building_energy_data,
    .selected_ctu = "Woodbury"
  )

  testthat::expect_equal(
    unique(state_non_res_energy$var),
    c(
      "electricity_residential_consumption_mwh_state",
      "electricity_commercial_consumption_mwh_state",
      "electricity_industrial_consumption_mwh_state",
      "commercial_employees_state",
      "industrial_employees_state",
      "commercial_mwh_per_worker_state",
      "industrial_mwh_per_worker_state",
      "commercial_therms_per_worker_state",
      "industrial_therms_per_worker_state"
    )
  )
  testthat::expect_equal(nrow(state_non_res_energy), 9)


  # wdbry_non_res_energy -----
  wdbry_non_res_energy <- get_non_residential_energy_baseline(
    tb = building_energy_data,
    .selected_ctu = "Woodbury"
  )

  testthat::expect_equal(
    unique(wdbry_non_res_energy$var),
    c(
      "commercial_therms", "industrial_therms",
      "commercial_mwh",
      "industrial_mwh",
      "commercial_therm_per_worker",
      "industrial_therm_per_worker",
      "commercial_mwh_per_worker",
      "industrial_mwh_per_worker"
    )
  )
  testthat::expect_equal(nrow(wdbry_non_res_energy), 8)

  # wdbry_non_res_xcel
  wdbry_non_res_xcel <- get_by_ctu_non_residential_xcel_energy_baseline(
    tb = building_energy_data,
    .selected_ctu = "Woodbury"
  )

  testthat::expect_equal(nrow(wdbry_non_res_xcel), 0)


  # test for county and ctu  ------
  # expected counties
  purrr::map(
    list(
      county_non_res_energy,
      wdbry_demo_baseline$county
    ),
    function(x) {
      testthat::expect_equal(
        unique(x$co_name),
        c(
          "Anoka", "Carver",
          "Dakota", "Hennepin",
          "Ramsey", "Scott",
          "Washington"
        )
      )
    }
  )


  # expected ctu
  purrr::map(
    list(
      wdbry_demo_baseline[[2]],
      wdbry_res_energy,
      wdbry_non_res_energy
      # wdbry_non_res_xcel
    ),
    function(x) {
      testthat::expect_equal(unique(x$ctu_name), "Woodbury")
    }
  )

  # expected year
  purrr::map(
    list(
      wdbry_demo_baseline$county,
      wdbry_demo_baseline$ctu,
      wdbry_res_energy,
      wdbry_non_res_energy,
      # wdbry_non_res_xcel,
      county_non_res_energy,
      state_non_res_energy
    ),
    function(x) {
      testthat::expect_equal(unique(x$year), 2018)
    }
  )
})
