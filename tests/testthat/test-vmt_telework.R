


telework_adjust <- vmt_telework(
  .pass_tb = st_paul_passenger,
  .mode = "PLDV",
  .telework_pct = 10,
  .enviro_factors = enviro_factors
)

testthat::expect_equal(
  telework_adjust,
  tibble::tribble(
    ~year, ~telework_adj,
    "2015", 1,
    "2018", 1,
    "2020", 1,
    "2025", 0.954183333333333,
    "2030", 0.908366666666667,
    "2035", 0.86255,
    "2040", 0.816733333333333,
    "2045", 0.770916666666667,
    "2050", 0.7251
  )
)

testthat::expect_error(vmt_telework(
  .pass_tb = st_paul_passenger,
  .mode = "BU",
  .telework_pct = 10,
  .enviro_factors = enviro_factors
))
