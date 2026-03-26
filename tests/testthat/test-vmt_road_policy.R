test_road_policy <- function(x) {
  testthat::test_that(paste0(x, " road policy reduces VMT"), {
    pass_tb_filtered <- transportation_data$passenger %>%
      filter(geog_name == x | geog_name == "All")

    fcm_test <- calc_fuel_cost_mile(
      pass_tb_filtered,
      .mode = "PLDV",
      .aeo_scenario = "REF",
      .miles_per_gallon = "SIMPG",
      .fuel_cost_gallon = 239.8 / 100
    )

    testthat::expect_error(
      vmt_road_policy(
        .pass_tb = pass_tb_filtered,
        .tb_vmt = tibble(
          year = unique(pass_tb_filtered$year),
          geog_name = x,
          mode = "PLDV"
        ),
        .mode = "PLDV",
        .tb_fuel_cost_mile = fcm_test,
        .vmt_fee = 0.05,
        .payd_fee = 0.05,
        .stock = "SIStock",
        .enviro_factors = enviro_factors
      )
    )

    vmt_tb <- tibble(
      year = unique(pass_tb_filtered$year),
      geog_name = x,
      mode = "PLDV"
    ) %>% left_join(geog_index %>% select(geog_id, geog_name), by = "geog_name")

    road_bau <- vmt_road_policy(
      .pass_tb = pass_tb_filtered,
      .tb_vmt = vmt_tb,
      .mode = "PLDV",
      .tb_fuel_cost_mile = fcm_test,
      .vmt_fee = 0,
      .cong_price = 0,
      .gas_tax = 0,
      .payd_fee = 0,
      .freight_vmt_fee = 0,
      .stock = "SIStock",
      .enviro_factors = enviro_factors
    )

    testthat::expect_equal(nrow(road_bau), length(unique(pass_tb_filtered$year)))

    testthat::expect_named(road_bau,
      expected = c(
        "year",
        "geog_id",
        "geog_name",
        "fuel_time_cost_mile",
        "payd_ins_adj",
        "vmt_fee_adj",
        "cong_adjust",
        "cross_vmt",
        "gas_adj"
      ),
      ignore.order = TRUE
    )

    road_bau_final <- road_bau %>%
      filter(year == max(year)) %>%
      pull(vmt_fee_adj)


    road_vmt_fee_low <- vmt_road_policy(
      .pass_tb = pass_tb_filtered,
      .tb_vmt = vmt_tb,
      .mode = "PLDV",
      .tb_fuel_cost_mile = fcm_test,
      .vmt_fee = 0.01,
      .cong_price = 0,
      .gas_tax = 0,
      .payd_fee = 0,
      .freight_vmt_fee = 0,
      .stock = "SIStock",
      .enviro_factors = enviro_factors
    )

    road_vmt_fee_med <- vmt_road_policy(
      .pass_tb = pass_tb_filtered,
      .tb_vmt = vmt_tb,
      .mode = "PLDV",
      .tb_fuel_cost_mile = fcm_test,
      .vmt_fee = 0.05,
      .cong_price = 0,
      .gas_tax = 0,
      .payd_fee = 0,
      .freight_vmt_fee = 0,
      .stock = "SIStock",
      .enviro_factors = enviro_factors
    )

    road_vmt_fee_high <- vmt_road_policy(
      .pass_tb = pass_tb_filtered,
      .tb_vmt = vmt_tb,
      .mode = "PLDV",
      .tb_fuel_cost_mile = fcm_test,
      .vmt_fee = 0.10,
      .cong_price = 0,
      .gas_tax = 0,
      .payd_fee = 0,
      .freight_vmt_fee = 0,
      .stock = "SIStock",
      .enviro_factors = enviro_factors
    )

    purrr::map(
      list(
        road_vmt_fee_low,
        road_vmt_fee_med,
        road_vmt_fee_high
      ),
      function(x) {
        test_adj <- x %>%
          filter(year == max(year)) %>%
          pull(vmt_fee_adj)

        # Higher VMT fees reduce the adjustment factor (more VMT reduction)
        testthat::expect_lt(test_adj, road_bau_final)
      }
    )
  })
}

purrr::map(
  c(
    "Arden Hills",
    "Bloomington",
    "Saint Paul",
    "Lake Elmo",
    "Minneapolis",
    "Crystal",
    "Bethel",
    "Rosemount",
    "White Bear Twp.",
    "Hennepin County",
    "Ramsey County",
    "Washington County",
    "Dakota County",
    "Anoka County",
    "Carver County",
    "Scott County"
  ),
  test_road_policy
)
