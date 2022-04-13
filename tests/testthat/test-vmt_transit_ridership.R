enviro_factors_edit <- enviro_factors

enviro_factors_edit$PLDV_TRANSIT_RATIO <- 47 / 100

pass_trans <- vmt_transit_ridership(
  tb = st_paul_passenger,
  .mode = "PLDV",
  .transit_rider_pct = 0.10,
  .enviro_factors = enviro_factors_edit
)

testthat::expect_equal(
  pass_trans,
  tibble::tribble(
    ~year, ~ctu, ~transit_adj,
    "2015", "St. Paul", 0,
    "2018", "St. Paul", 0,
    "2020", "St. Paul", 0,
    "2025", "St. Paul", 905.813374216667,
    "2030", "St. Paul", 1904.1519887,
    "2035", "St. Paul", 3001.87704445,
    "2040", "St. Paul", 4196.7014776,
    "2045", "St. Paul", 5488.62528658333,
    "2050", "St. Paul", 6877.6484667
  )
)

testthat::expect_equal(
  vmt_transit_ridership(
    tb = st_paul_passenger,
    .mode = "BU",
    .transit_rider_pct = 0.1,
    .enviro_factors = enviro_factors_edit
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
    .enviro_factors = enviro_factors_edit
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
