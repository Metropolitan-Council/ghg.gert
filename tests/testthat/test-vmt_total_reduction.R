test_vmt_total <- function(x) {
  testthat::test_that(paste0(x, " VMT reduces with interventions"), {
    pass_tb_filtered <- transportation_data$passenger %>%
      filter(geog_name == x)

    vmt_bau <- vmt_total_reduction(
      .pass_tb = pass_tb_filtered,
      .mode = "PLDV",
      .vmt_reduction_pct = 0,
      .enviro_factors = enviro_factors
    )

    testthat::expect_equal(nrow(vmt_bau), length(unique(pass_tb_filtered$year)))

    testthat::expect_named(vmt_bau,
      expected = c(
        "year",
        "vmt_reduction_adj"
      ),
      ignore.order = TRUE
    )

    vmt_bau_final <- vmt_bau %>%
      filter(year == max(year)) %>%
      pull(vmt_reduction_adj)


    vmt_5pct <- vmt_total_reduction(
      .pass_tb = pass_tb_filtered,
      .mode = "PLDV",
      .vmt_reduction_pct = 0.05,
      .enviro_factors = enviro_factors
    )

    vmt_10pct <- vmt_total_reduction(
      .pass_tb = pass_tb_filtered,
      .mode = "PLDV",
      .vmt_reduction_pct = 0.10,
      .enviro_factors = enviro_factors
    )

    vmt_15pct <- vmt_total_reduction(
      .pass_tb = pass_tb_filtered,
      .mode = "PLDV",
      .vmt_reduction_pct = 0.15,
      .enviro_factors = enviro_factors
    )

    purrr::map(
      list(
        vmt_5pct,
        vmt_10pct,
        vmt_15pct
      ),
      function(x) {
        test_vmt <- x %>%
          filter(year == max(year)) %>%
          pull(vmt_reduction_adj)

        testthat::expect_lt(test_vmt, vmt_bau_final)
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
  test_vmt_total
)
