telework_adjust <- vmt_telework(
  .pass_tb = st_paul_passenger,
  .mode = "PLDV",
  .telework_pct = 0.1,
  .enviro_factors = enviro_factors
)

testthat::expect_equal(
  telework_adjust,
  tibble::tribble(
    ~year, ~telework_adj,
    "2015", 1,
    "2018", 1,
    "2020", 1,
    "2025", 0.99931275,
    "2030", 0.9986255,
    "2035", 0.99793825,
    "2040", 0.997251,
    "2045", 0.997251,
    "2050", 0.997251
  )
)

testthat::expect_error(vmt_telework(
  .pass_tb = st_paul_passenger,
  .mode = "BU",
  .telework_pct = 10,
  .enviro_factors = enviro_factors
))
