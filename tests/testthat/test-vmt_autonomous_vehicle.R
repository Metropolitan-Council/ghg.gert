



testthat::expect_equal(
  vmt_autonomous_vehicle(
    .pass_tb = st_paul_passenger,
    .av_pct = 0.1,
    .mode = "PLDV"
  ),
  tibble::tribble(
    ~year,       ~ctu,  ~av_adj,
    "2015", "St. Paul",        1,
    "2018", "St. Paul",        1,
    "2020", "St. Paul",        1,
    "2025", "St. Paul", 0.998689,
    "2030", "St. Paul", 0.991995,
    "2035", "St. Paul", 0.966824,
    "2040", "St. Paul", 0.926524,
    "2045", "St. Paul", 0.906099,
    "2050", "St. Paul",      0.9
  )
)


testthat::expect_equal(
  vmt_autonomous_vehicle(
    .pass_tb = st_paul_passenger,
    .av_pct = 0.1,
    .mode = "AV"
  ),
  tibble::tribble(
    ~year,       ~ctu,  ~av_adj,
    "2015", "St. Paul",        0,
    "2018", "St. Paul",        0,
    "2020", "St. Paul",        0,
    "2025", "St. Paul", 0.001311,
    "2030", "St. Paul", 0.008005,
    "2035", "St. Paul", 0.033176,
    "2040", "St. Paul", 0.073476,
    "2045", "St. Paul", 0.093901,
    "2050", "St. Paul",      0.1
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
