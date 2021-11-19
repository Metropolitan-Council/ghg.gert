pass_trans <- vmt_transit_ridership(
  tb = st_paul_passenger,
  .mode = "PLDV",
  .transit_rider_pct = 0.01,
  .enviro_factors = enviro_factors
)

testthat::expect_equal(
  pass_trans,
  tibble::tribble(
    ~year, ~ctu, ~transit_adj,
    "2015", "St. Paul", 0,
    "2018", "St. Paul", 0,
    "2020", "St. Paul", 0,
    "2025", "St. Paul", 0.000783333333333333,
    "2030", "St. Paul", 0.00156666666666667,
    "2035", "St. Paul", 0.00235,
    "2040", "St. Paul", 0.00313333333333333,
    "2045", "St. Paul", 0.00391666666666667,
    "2050", "St. Paul", 0.0047
  )
)

testthat::expect_equal(
  vmt_transit_ridership(
    tb = st_paul_passenger,
    .mode = "BU",
    .transit_rider_pct = 0.1,
    .enviro_factors = enviro_factors
  ),
  tibble::tribble(
    ~year, ~ctu, ~transit_adj,
    "2015", "St. Paul", 1,
    "2018", "St. Paul", 1,
    "2020", "St. Paul", 1,
    "2025", "St. Paul", 1.01666666666667,
    "2030", "St. Paul", 1.03333333333333,
    "2035", "St. Paul", 1.05,
    "2040", "St. Paul", 1.06666666666667,
    "2045", "St. Paul", 1.08333333333333,
    "2050", "St. Paul", 1.1
  )
)


testthat::expect_equal(
  vmt_transit_ridership(
    tb = st_paul_passenger,
    .mode = "RI",
    .transit_rider_pct = 0.1,
    .enviro_factors = enviro_factors
  ),
  tibble::tribble(
    ~year, ~ctu, ~transit_adj,
    "2015", "St. Paul", 1,
    "2018", "St. Paul", 1,
    "2020", "St. Paul", 1,
    "2025", "St. Paul", 1.01666666666667,
    "2030", "St. Paul", 1.03333333333333,
    "2035", "St. Paul", 1.05,
    "2040", "St. Paul", 1.06666666666667,
    "2045", "St. Paul", 1.08333333333333,
    "2050", "St. Paul", 1.1
  )
)
