test_transit_service <- function(x) {
  testthat::test_that(paste0(x, " transit service reduces PLDV VMT"), {
    pass_tb_filtered <- transportation_data$passenger %>%
      filter(geog_name == x | geog_name == "All")

    transit_bau <- vmt_transit_service(
      tb = pass_tb_filtered,
      .mode = "PLDV",
      .transit_service_pct = 0,
      .enviro_factors = enviro_factors
    )

    testthat::expect_equal(nrow(transit_bau), length(unique(pass_tb_filtered$year)))

    testthat::expect_named(transit_bau,
      expected = c(
        "year",
        "geog_id",
        "geog_name",
        "transit_adj"
      ),
      ignore.order = TRUE
    )

    transit_bau_final <- transit_bau %>%
      filter(year == max(year)) %>%
      pull(transit_adj)


    transit_10pct <- vmt_transit_service(
      tb = pass_tb_filtered,
      .mode = "PLDV",
      .transit_service_pct = 0.10,
      .enviro_factors = enviro_factors
    )

    transit_20pct <- vmt_transit_service(
      tb = pass_tb_filtered,
      .mode = "PLDV",
      .transit_service_pct = 0.20,
      .enviro_factors = enviro_factors
    )

    transit_30pct <- vmt_transit_service(
      tb = pass_tb_filtered,
      .mode = "PLDV",
      .transit_service_pct = 0.30,
      .enviro_factors = enviro_factors
    )

    purrr::map(
      list(
        transit_10pct,
        transit_20pct,
        transit_30pct
      ),
      function(x) {
        test_adj <- x %>%
          filter(year == max(year)) %>%
          pull(transit_adj)

        # For PLDV, transit_adj is absolute VMT reduction
        # Higher service produces greater reduction (larger absolute value)
        testthat::expect_gt(test_adj, transit_bau_final)
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
  test_transit_service
)
