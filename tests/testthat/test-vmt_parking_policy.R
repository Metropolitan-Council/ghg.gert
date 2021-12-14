


parking_adj <- vmt_parking_policy(
  tb = st_paul_passenger,
  .mode = "PLDV",
  .parking_price = 44,
  .enviro_factors = enviro_factors
)

testthat::expect_equal(
  parking_adj,
  tibble::tribble(
    ~year,       ~ctu,   ~park_price_adj,
    "2015", "St. Paul",                 1,
    "2018", "St. Paul",                 1,
    "2020", "St. Paul",                 1,
    "2025", "St. Paul", 0.861270572380502,
    "2030", "St. Paul", 0.861270572380502,
    "2035", "St. Paul", 0.861270572380502,
    "2040", "St. Paul", 0.861270572380502,
    "2045", "St. Paul", 0.861270572380502,
    "2050", "St. Paul", 0.861270572380502
  )
)


testthat::expect_equal(
  vmt_parking_policy(
    tb = st_paul_passenger,
    .mode = "PLDV",
    .parking_price = 66,
    .enviro_factors = enviro_factors
  ),
  tibble::tribble(
    ~year,       ~ctu,   ~park_price_adj,
    "2015", "St. Paul",                 1,
    "2018", "St. Paul",                 1,
    "2020", "St. Paul",                 1,
    "2025", "St. Paul", 0.791905858570754,
    "2030", "St. Paul", 0.791905858570754,
    "2035", "St. Paul", 0.791905858570754,
    "2040", "St. Paul", 0.791905858570754,
    "2045", "St. Paul", 0.791905858570754,
    "2050", "St. Paul", 0.791905858570754
  )
)


testthat::expect_equal(
  vmt_parking_policy(
    tb = st_paul_passenger,
    .mode = "BU",
    .parking_price = 66,
    .enviro_factors = enviro_factors
  ),
  tibble::tribble(
    ~year, ~ctu, ~park_price_adj,
    "2015", "St. Paul", 1,
    "2018", "St. Paul", 1,
    "2020", "St. Paul", 1,
    "2025", "St. Paul", 1.02972774091708,
    "2030", "St. Paul", 1.02972774091708,
    "2035", "St. Paul", 1.02972774091708,
    "2040", "St. Paul", 1.02972774091708,
    "2045", "St. Paul", 1.02972774091708,
    "2050", "St. Paul", 1.02972774091708
  )
)



sut_park <- vmt_parking_policy(
  tb = st_paul_freight,
  .mode = "SUT",
  .freight_parking_price = 2,
  .enviro_factors = enviro_factors
)

testthat::expect_equal(
  sut_park,
  tibble::tribble(
    ~year, ~ctu, ~park_price_adj,
    "2015", "St. Paul", 1,
    "2018", "St. Paul", 1,
    "2020", "St. Paul", 1,
    "2025", "St. Paul", 0.86,
    "2030", "St. Paul", 0.86,
    "2035", "St. Paul", 0.86,
    "2040", "St. Paul", 0.86,
    "2045", "St. Paul", 0.86,
    "2050", "St. Paul", 0.86
  )
)



testthat::expect_error(vmt_parking_policy(
  tb = st_paul_passenger,
  .mode = "SUT",
  .freight_parking_price = 2,
  .enviro_factors = enviro_factors
))



testthat::expect_equal(
  vmt_parking_policy(
    tb = st_paul_passenger,
    .mode = "PLDV",
    .parking_price = 0,
    .enviro_factors = enviro_factors
  ),
  tibble::tribble(
    ~year, ~ctu, ~park_price_adj,
    "2015", "St. Paul", 1,
    "2018", "St. Paul", 1,
    "2020", "St. Paul", 1,
    "2025", "St. Paul", 1,
    "2030", "St. Paul", 1,
    "2035", "St. Paul", 1,
    "2040", "St. Paul", 1,
    "2045", "St. Paul", 1,
    "2050", "St. Paul", 1
  )
)
