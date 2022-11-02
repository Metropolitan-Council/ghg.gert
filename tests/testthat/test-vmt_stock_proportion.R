


stock_prop <- vmt_stock_proportion(
  .tb = st_paul_passenger,
  .mode = "PLDV",
  .stock = "BEVStock"
)

testthat::expect_equal(
  stock_prop,
  tibble::tribble(
    ~ctu, ~year, ~mode, ~mode_stock_adj,
    "St. Paul", "2015", "PLDV", 0.000124416171770741,
    "St. Paul", "2018", "PLDV", 0.00183961037234905,
    "St. Paul", "2020", "PLDV", 0.00290452704867203,
    "St. Paul", "2025", "PLDV", 0.0339617869619354,
    "St. Paul", "2030", "PLDV", 0.032572608100096,
    "St. Paul", "2035", "PLDV", 0.0284313792805206,
    "St. Paul", "2040", "PLDV", 0.0557598257322424,
    "St. Paul", "2045", "PLDV", 0.0652959890826254,
    "St. Paul", "2050", "PLDV", 0.0743881827076878
  )
)


testthat::expect_equal(
  vmt_stock_proportion(
    .tb = st_paul_passenger,
    .mode = "PLDV",
    .stock = "SIStock"
  ),
  tibble::tribble(
    ~ctu, ~year, ~mode, ~mode_stock_adj,
    "St. Paul", "2015", "PLDV", 0.984004911880514,
    "St. Paul", "2018", "PLDV", 0.978740272020054,
    "St. Paul", "2020", "PLDV", 0.975471602629528,
    "St. Paul", "2025", "PLDV", 0.918940731680755,
    "St. Paul", "2030", "PLDV", 0.899473528569536,
    "St. Paul", "2035", "PLDV", 0.884909565264855,
    "St. Paul", "2040", "PLDV", 0.826590446319469,
    "St. Paul", "2045", "PLDV", 0.79960899315738,
    "St. Paul", "2050", "PLDV", 0.773883546749061
  )
)


testthat::expect_equal(
  vmt_stock_proportion(
    .tb = st_paul_passenger,
    .mode = "BU",
    .stock = "BCIStock"
  ),
  tibble::tribble(
    ~ctu, ~year, ~mode, ~mode_stock_adj,
    "St. Paul", "2015", "BU", 1,
    "St. Paul", "2018", "BU", 1,
    "St. Paul", "2020", "BU", 1,
    "St. Paul", "2025", "BU", 1,
    "St. Paul", "2030", "BU", 1,
    "St. Paul", "2035", "BU", 1,
    "St. Paul", "2040", "BU", 1,
    "St. Paul", "2045", "BU", 1,
    "St. Paul", "2050", "BU", 1
  )
)


testthat::expect_equal(
  vmt_stock_proportion(
    .tb = st_paul_passenger,
    .mode = "RU",
    .stock = "EVStock"
  ),
  tibble::tribble(
    ~ctu, ~year, ~mode, ~mode_stock_adj,
    "St. Paul", "2015", "RU", 1,
    "St. Paul", "2018", "RU", 1,
    "St. Paul", "2020", "RU", 1,
    "St. Paul", "2025", "RU", 1,
    "St. Paul", "2030", "RU", 1,
    "St. Paul", "2035", "RU", 1,
    "St. Paul", "2040", "RU", 1,
    "St. Paul", "2045", "RU", 1,
    "St. Paul", "2050", "RU", 1
  )
)



testthat::expect_error(
  vmt_stock_proportion(
    .tb = st_paul_passenger,
    .mode = "BU",
    .stock = "SIStock"
  )
)
