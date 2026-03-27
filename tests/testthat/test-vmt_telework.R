test_telework <- function(x) {
  testthat::test_that(paste0(x, " telework reduces VMT"), {
    pass_tb_filtered <- transportation_data$passenger %>%
      filter(geog_name == x | geog_name == "All")

    telework_bau <- vmt_telework(
      .pass_tb = pass_tb_filtered,
      .mode = "PLDV",
      .telework_pct = 0,
      .enviro_factors = enviro_factors
    )

    testthat::expect_equal(nrow(telework_bau), length(unique(pass_tb_filtered$year)))

    testthat::expect_named(telework_bau,
      expected = c(
        "year",
        "telework_adj"
      ),
      ignore.order = TRUE
    )

    telework_bau_final <- telework_bau %>%
      filter(year == max(year)) %>%
      pull(telework_adj)


    telework_10pct <- vmt_telework(
      .pass_tb = pass_tb_filtered,
      .mode = "PLDV",
      .telework_pct = 0.10,
      .enviro_factors = enviro_factors
    )

    telework_25pct <- vmt_telework(
      .pass_tb = pass_tb_filtered,
      .mode = "PLDV",
      .telework_pct = 0.25,
      .enviro_factors = enviro_factors
    )

    telework_50pct <- vmt_telework(
      .pass_tb = pass_tb_filtered,
      .mode = "PLDV",
      .telework_pct = 0.50,
      .enviro_factors = enviro_factors
    )

    testthat::expect_error(vmt_telework(
      .pass_tb = pass_tb_filtered,
      .mode = "BU",
      .telework_pct = 10,
      .enviro_factors = enviro_factors
    ))

    purrr::map(
      list(
        telework_10pct,
        telework_25pct,
        telework_50pct
      ),
      function(x) {
        test_adj <- x %>%
          filter(year == max(year)) %>%
          pull(telework_adj)

        testthat::expect_lt(test_adj, telework_bau_final)
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
  test_telework
)
