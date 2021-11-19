



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
    "2025", "St. Paul", 0.998689496,
    "2030", "St. Paul", 0.991994905,
    "2035", "St. Paul", 0.966823637,
    "2040", "St. Paul", 0.92652396,
    "2045", "St. Paul", 0.9060991,
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
    "2025", "St. Paul", 0.001310504,
    "2030", "St. Paul", 0.008005095,
    "2035", "St. Paul", 0.033176363,
    "2040", "St. Paul", 0.07347604,
    "2045", "St. Paul", 0.0939009,
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
    "2025", "St. Paul", 0.895,
    "2030", "St. Paul", 0.895,
    "2035", "St. Paul", 0.895,
    "2040", "St. Paul", 0.895,
    "2045", "St. Paul", 0.895,
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
