testthat::test_that("Expected changes, Saint Paul", {
  # Stock adjustments only adjust the proportion of the total vehicles
  # The total number of vehicles should NOT change

  testthat::expect_error(
    adj_fleet_shares(
      .bev_pct_sales = .20,
      .hev_pct_sales = .40,
      .pass_tb = transportation_data$passenger,
      .freight_tb = transportation_data$freight,
      .selected_ctu = "Saint Paul",
      .vmt_fee = 100,
      .enviro_factors = enviro_factors
    )
  )


  testthat::expect_error(
    adj_fleet_shares(
      .bev_pct_sales = .20,
      .hev_pct_sales = .40,
      .pass_tb = transportation_data$passenger,
      .freight_tb = transportation_data$freight,
      .selected_ctu = "Saint Paul",
      .vmt_fee = 0.1,
      .payd_fee = 0.1,
      .enviro_factors = enviro_factors
    )
  )


  testthat::expect_warning(
    adj_fleet_shares(
      .bev_pct_sales = 0.35,
      .hev_pct_sales = 0.60,
      .pass_tb = transportation_data$passenger,
      .freight_tb = transportation_data$freight,
      .selected_ctu = "Saint Paul",
      .enviro_factors = enviro_factors
    )
  )


  t_hev_bev <- adj_fleet_shares(
    .bev_pct_sales = .20,
    .hev_pct_sales = .40,
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "Saint Paul",
    .vmt_fee = 0,
    .payd_fee = 0,
    .gas_tax = 0,
    .enviro_factors = enviro_factors
  )


  test_total_table <- left_join(
    transportation_data$passenger %>%
      filter(
        mode == "PLDV",
        str_detect(var, "Tot")
      ),
    t_hev_bev$pass %>%
      filter(
        mode == "PLDV",
        str_detect(var, "Tot")
      ),
    by = c("mode", "var", "geog_name", "year", "aeo_mode", "type"),
    suffix = c(".orig", ".adj")
  ) %>%
    mutate(diff = round(value.orig - value.adj)) %>%
    filter(diff != 0)

  testthat::expect_equal(nrow(test_total_table), 0)



  testthat::expect_warning(
    suppressMessages(
      adj_fleet_shares(
        .bev_pct_sales = 1,
        .pass_tb = transportation_data$passenger,
        .freight_tb = transportation_data$freight %>%
          filter(geog_name == "Saint Paul"),
        .selected_ctu = "Saint Paul",
        .vmt_fee = 0,
        .payd_fee = 0,
        .gas_tax = 0,
        .enviro_factors = enviro_factors
      )
    )
  )
  # t_hev_bev$freight


  freight_test_total_table <- left_join(
    transportation_data$freight %>%
      filter(
        mode %in% c("SUT", "CUT"),
        str_detect(var, "Tot")
      ),
    t_hev_bev$freight %>%
      filter(
        mode %in% c("SUT", "CUT"),
        str_detect(var, "Tot")
      ),
    by = c("mode", "var", "geog_name", "year", "aeo_mode", "type"),
    suffix = c(".orig", ".adj")
  ) %>%
    mutate(diff = round(value.orig - value.adj)) %>%
    filter(diff != 0)

  testthat::expect_equal(nrow(freight_test_total_table), 0)
})

testthat::test_that("Expected changes, Centerville", {
  # Stock adjustments only adjust the proportion of the total vehicles
  # The total number of vehicles should NOT change

  testthat::expect_error(
    adj_fleet_shares(
      .bev_pct_sales = .20,
      .hev_pct_sales = .40,
      .pass_tb = transportation_data$passenger,
      .freight_tb = transportation_data$freight,
      .selected_ctu = "Centerville",
      .vmt_fee = 100,
      .enviro_factors = enviro_factors
    )
  )

  testthat::expect_warning(
    adj_fleet_shares(
      .bev_pct_sales = 0.35,
      .hev_pct_sales = 0.60,
      .pass_tb = transportation_data$passenger,
      .freight_tb = transportation_data$freight,
      .selected_ctu = "Centerville",
      .enviro_factors = enviro_factors
    )
  )


  t_hev_bev <- adj_fleet_shares(
    .bev_pct_sales = .20,
    .hev_pct_sales = .40,
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "Centerville",
    .vmt_fee = 0,
    .payd_fee = 0,
    .gas_tax = 0,
    .enviro_factors = enviro_factors
  )


  test_total_table <- left_join(
    transportation_data$passenger %>%
      filter(
        mode == "PLDV",
        str_detect(var, "Tot")
      ),
    t_hev_bev$pass %>%
      filter(
        mode == "PLDV",
        str_detect(var, "Tot")
      ),
    by = c("mode", "var", "geog_name", "year", "aeo_mode", "type"),
    suffix = c(".orig", ".adj")
  ) %>%
    mutate(diff = round(value.orig - value.adj)) %>%
    filter(diff != 0)

  testthat::expect_equal(nrow(test_total_table), 0)



  testthat::expect_warning(
    suppressMessages(
      adj_fleet_shares(
        .bev_pct_sales = 1,
        .pass_tb = transportation_data$passenger,
        .freight_tb = transportation_data$freight %>%
          filter(geog_name == "Centerville"),
        .selected_ctu = "Centerville",
        .vmt_fee = 0,
        .payd_fee = 0,
        .gas_tax = 0,
        .enviro_factors = enviro_factors
      )
    )
  )
  # t_hev_bev$freight


  freight_test_total_table <- left_join(
    transportation_data$freight %>%
      filter(
        mode %in% c("SUT", "CUT"),
        str_detect(var, "Tot")
      ),
    t_hev_bev$freight %>%
      filter(
        mode %in% c("SUT", "CUT"),
        str_detect(var, "Tot")
      ),
    by = c("mode", "var", "geog_name", "year", "aeo_mode", "type"),
    suffix = c(".orig", ".adj")
  ) %>%
    mutate(diff = round(value.orig - value.adj)) %>%
    filter(diff != 0)

  testthat::expect_equal(nrow(freight_test_total_table), 0)
})

test_that("fleet shares adjust to pricing inputs", {
  ref_fleet <- transportation_data$passenger %>%
    filter_ctu("Saint Paul")

  t_fleet <- adj_fleet_shares(
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "Saint Paul",
    .vmt_fee = 0.01,
    .enviro_factors = enviro_factors
  )

  t_fleet2 <- adj_fleet_shares(
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "Saint Paul",
    .vmt_fee = 0.05,
    .enviro_factors = enviro_factors
  )


  t_fleet3 <- adj_fleet_shares(
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "Saint Paul",
    .vmt_fee = 0.02,
    .enviro_factors = enviro_factors
  )


  compare_fleet <- function(comp_fleet) {
    expect_equal(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "PMT"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "PMT"
        ) %>%
        pull(value)
    )


    expect_equal(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "PMT"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "PMT"
        ) %>%
        pull(value)
    )

    # bev decrease
    expect_gt(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "BEVExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "BEVExist"
        ) %>%
        pull(value)
    )

    # si decrease
    expect_gt(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "SIExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "SIExist"
        ) %>%
        pull(value)
    )

    # ci decrease
    expect_gt(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "CIExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "CIExist"
        ) %>%
        pull(value)
    )
  }

  purrr::map(
    list(
      t_fleet,
      t_fleet2,
      t_fleet3
    ),
    compare_fleet
  )
})


test_that("fleet shares adjust to pricing inputs, Orono", {
  ref_fleet <- transportation_data$passenger %>%
    filter_ctu("Orono")

  t_fleet <- adj_fleet_shares(
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "Orono",
    .vmt_fee = 0.01,
    .enviro_factors = enviro_factors
  )

  t_fleet2 <- adj_fleet_shares(
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "Orono",
    .vmt_fee = 0.05,
    .enviro_factors = enviro_factors
  )


  t_fleet3 <- adj_fleet_shares(
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "Orono",
    .vmt_fee = 0.02,
    .enviro_factors = enviro_factors
  )


  compare_fleet <- function(comp_fleet) {
    expect_equal(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "PMT"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "PMT"
        ) %>%
        pull(value)
    )

    # bev decrease
    expect_gt(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "BEVExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "BEVExist"
        ) %>%
        pull(value)
    )

    # si decrease
    expect_gt(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "SIExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "SIExist"
        ) %>%
        pull(value)
    )

    # ci decrease
    expect_gt(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "CIExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "CIExist"
        ) %>%
        pull(value)
    )
  }

  purrr::map(
    list(
      t_fleet,
      t_fleet2,
      t_fleet3
    ),
    compare_fleet
  )
})


test_that("fleet shares adjust to pricing inputs, Marshan Twp.", {
  ref_fleet <- transportation_data$passenger %>%
    filter_ctu("Marshan Twp.")

  t_fleet <- adj_fleet_shares(
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "Marshan Twp.",
    .gas_tax = 0.01,
    .enviro_factors = enviro_factors
  )

  t_fleet2 <- adj_fleet_shares(
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "Marshan Twp.",
    .gas_tax = 0.03,
    .enviro_factors = enviro_factors
  )


  t_fleet3 <- adj_fleet_shares(
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "Marshan Twp.",
    .gas_tax = 0.05,
    .enviro_factors = enviro_factors
  )


  compare_fleet <- function(comp_fleet) {
    expect_equal(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "PMT"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "PMT"
        ) %>%
        pull(value)
    )

    # bev increase
    expect_lte(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "BEVExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "BEVExist"
        ) %>%
        pull(value)
    )

    # si decrease
    expect_gt(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "SIExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "SIExist"
        ) %>%
        pull(value)
    )

    # ci decrease
    expect_gt(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "CIExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "CIExist"
        ) %>%
        pull(value)
    )
  }

  purrr::map(
    list(
      t_fleet,
      t_fleet2,
      t_fleet3
    ),
    compare_fleet
  )
})


test_that("fleet shares adjust to pricing inputs, Hanover", {
  ref_fleet <- transportation_data$passenger %>%
    filter_ctu("Hanover")

  t_fleet <- adj_fleet_shares(
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "Hanover",
    .gas_tax = 0.03,
    .enviro_factors = enviro_factors
  )

  t_fleet2 <- adj_fleet_shares(
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "Hanover",
    .gas_tax = 0.02,
    .enviro_factors = enviro_factors
  )


  t_fleet3 <- adj_fleet_shares(
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "Hanover",
    .gas_tax = 0.01,
    .enviro_factors = enviro_factors
  )


  compare_fleet <- function(comp_fleet) {
    expect_equal(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "PMT"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "PMT"
        ) %>%
        pull(value)
    )

    # bev increase
    expect_lte(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "BEVExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "BEVExist"
        ) %>%
        pull(value)
    )

    # si decrease
    expect_gt(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "SIExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "SIExist"
        ) %>%
        pull(value)
    )

    # ci decrease
    expect_gt(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "CIExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "CIExist"
        ) %>%
        pull(value)
    )
  }

  purrr::map(
    list(
      t_fleet,
      t_fleet2,
      t_fleet3
    ),
    compare_fleet
  )
})


test_that("fleet shares adjust to sales inputs, Hanover", {
  ref_fleet <- transportation_data$passenger %>%
    filter_ctu("Hanover")

  t_fleet <- adj_fleet_shares(
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "Hanover",
    .bev_pct_sales = 0.1,
    .enviro_factors = enviro_factors
  )

  t_fleet2 <- adj_fleet_shares(
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "Hanover",
    .bev_pct_sales = 0.05,
    .enviro_factors = enviro_factors
  )


  t_fleet3 <- adj_fleet_shares(
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "Hanover",
    .bev_pct_sales = 0.5,
    .enviro_factors = enviro_factors
  )


  compare_fleet <- function(comp_fleet) {
    expect_equal(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "PMT"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "PMT"
        ) %>%
        pull(value)
    )

    # bev increase
    expect_gte(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "BEVExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "BEVExist"
        ) %>%
        pull(value)
    )

    # si decrease
    expect_lte(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "SIExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "SIExist"
        ) %>%
        pull(value)
    )

    # ci decrease
    expect_lte(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "CIExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "CIExist"
        ) %>%
        pull(value)
    )
  }

  purrr::map(
    list(
      t_fleet,
      t_fleet2,
      t_fleet3
    ),
    compare_fleet
  )
})

test_that("fleet shares adjust to sales inputs, Birchwood Village", {
  ref_fleet <- transportation_data$passenger %>%
    filter_ctu("Birchwood Village")

  t_fleet <- adj_fleet_shares(
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "Birchwood Village",
    .bev_pct_sales = 0.1,
    .enviro_factors = enviro_factors
  )

  t_fleet2 <- adj_fleet_shares(
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "Birchwood Village",
    .bev_pct_sales = 0.05,
    .enviro_factors = enviro_factors
  )


  t_fleet3 <- adj_fleet_shares(
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "Birchwood Village",
    .bev_pct_sales = 0.5,
    .enviro_factors = enviro_factors
  )


  compare_fleet <- function(comp_fleet) {
    expect_equal(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "PMT"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "PMT"
        ) %>%
        pull(value)
    )

    # bev increase
    expect_gte(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "BEVExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "BEVExist"
        ) %>%
        pull(value)
    )


    expect_gte(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "HEVExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "HEVExist"
        ) %>%
        pull(value)
    )

    # si decrease
    expect_lte(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "SIExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "SIExist"
        ) %>%
        pull(value)
    )

    # ci decrease
    expect_lte(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "CIExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "CIExist"
        ) %>%
        pull(value)
    )
  }

  purrr::map(
    list(
      t_fleet,
      t_fleet2,
      t_fleet3
    ),
    compare_fleet
  )
})

test_that("fleet shares adjust to sales inputs, New Trier", {
  ref_fleet <- transportation_data$passenger %>%
    filter_ctu("New Trier")

  t_fleet <- adj_fleet_shares(
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "New Trier",
    .bev_pct_sales = 0.1,
    .hev_pct_sales = 0.1,
    .enviro_factors = enviro_factors
  )

  t_fleet2 <- adj_fleet_shares(
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "New Trier",
    .bev_pct_sales = 0.05,
    .hev_pct_sales = 0.5,
    .enviro_factors = enviro_factors
  )


  t_fleet3 <- adj_fleet_shares(
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "New Trier",
    .bev_pct_sales = 0.5,
    .hev_pct_sales = 0.15,
    .enviro_factors = enviro_factors
  )


  compare_fleet <- function(comp_fleet) {
    expect_equal(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "PMT"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "PMT"
        ) %>%
        pull(value)
    )

    # bev increase
    expect_gte(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "BEVExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "BEVExist"
        ) %>%
        pull(value)
    )


    expect_gte(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "HEVExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "HEVExist"
        ) %>%
        pull(value)
    )

    # si decrease
    expect_lte(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "SIExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "SIExist"
        ) %>%
        pull(value)
    )

    # ci decrease
    expect_lte(
      ref_fleet %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "CIExist"
        ) %>%
        pull(value),
      comp_fleet$pass %>%
        filter(
          year == "2050",
          mode == "PLDV",
          var == "CIExist"
        ) %>%
        pull(value)
    )
  }

  purrr::map(
    list(
      t_fleet,
      t_fleet2,
      t_fleet3
    ),
    compare_fleet
  )
})
