



testthat::expect_equal(
  vmt_autonomous_vehicle(
    .pass_tb = st_paul_passenger,
    .av_pct = 0.1,
    .mode = "PLDV"
  ),
  tibble::tribble(
    ~year, ~ctu, ~av_adj,
    "2015", "St. Paul", 1,
    "2018", "St. Paul", 1,
    "2020", "St. Paul", 1,
    "2025", "St. Paul", 0.9997815,
    "2030", "St. Paul", 0.997331666666667,
    "2035", "St. Paul", 0.983412,
    "2040", "St. Paul", 0.951016,
    "2045", "St. Paul", 0.921749166666667,
    "2050", "St. Paul", 0.9
  )
)


testthat::expect_equal(
  vmt_autonomous_vehicle(
    .pass_tb = st_paul_passenger,
    .av_pct = 0.1,
    .mode = "AV"
  ),
  tibble::tribble(
    ~year, ~ctu, ~av_adj,
    "2015", "St. Paul", 0,
    "2018", "St. Paul", 0,
    "2020", "St. Paul", 0,
    "2025", "St. Paul", 0.0002185,
    "2030", "St. Paul", 0.00266833333333333,
    "2035", "St. Paul", 0.016588,
    "2040", "St. Paul", 0.048984,
    "2045", "St. Paul", 0.0782508333333333,
    "2050", "St. Paul", 0.1
  )
)


testthat::expect_equal(
  vmt_autonomous_vehicle(
    .pass_tb = st_paul_passenger,
    .av_pct = 0.1,
    .mode = "BU"
  ),
  tibble::tribble(
    ~year, ~ctu, ~av_adj,
    "2015", "St. Paul", 1,
    "2018", "St. Paul", 1,
    "2020", "St. Paul", 1,
    "2025", "St. Paul", 0.9825,
    "2030", "St. Paul", 0.965,
    "2035", "St. Paul", 0.9475,
    "2040", "St. Paul", 0.93,
    "2045", "St. Paul", 0.9125,
    "2050", "St. Paul", 0.895
  )
)



testthat::expect_error(
  vmt_autonomous_vehicle(
    .pass_tb = st_paul_freight,
    .av_pct = 0.1,
    .mode = "AV"
  )
)
