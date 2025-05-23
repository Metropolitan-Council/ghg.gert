stock_prop <- vmt_stock_proportion(
  .tb = st_paul_passenger,
  .mode = "PLDV",
  .stock = "BEVStock"
)

testthat::expect_equal(
  stock_prop %>% select(-geog_id),
  tibble::tribble(
    ~geog_name, ~year, ~mode, ~mode_stock_adj,
    "Saint Paul", "2015", "PLDV", 0.000124416171770741,
    "Saint Paul", "2018", "PLDV", 0.00183961037234905,
    "Saint Paul", "2020", "PLDV", 0.00290452704867203,
    "Saint Paul", "2025", "PLDV", 0.0339617869619354,
    "Saint Paul", "2030", "PLDV", 0.032572608100096,
    "Saint Paul", "2035", "PLDV", 0.0284313792805206,
    "Saint Paul", "2040", "PLDV", 0.0557598257322424,
    "Saint Paul", "2045", "PLDV", 0.0652959890826254,
    "Saint Paul", "2050", "PLDV", 0.0743881827076878
  )
)


testthat::expect_equal(
  vmt_stock_proportion(
    .tb = st_paul_passenger,
    .mode = "PLDV",
    .stock = "SIStock"
  ) %>% select(-geog_id),
  tibble::tribble(
    ~geog_name, ~year, ~mode, ~mode_stock_adj,
    "Saint Paul", "2015", "PLDV", 0.984004911880514,
    "Saint Paul", "2018", "PLDV", 0.978740272020054,
    "Saint Paul", "2020", "PLDV", 0.975471602629528,
    "Saint Paul", "2025", "PLDV", 0.918940731680755,
    "Saint Paul", "2030", "PLDV", 0.899473528569536,
    "Saint Paul", "2035", "PLDV", 0.884909565264855,
    "Saint Paul", "2040", "PLDV", 0.826590446319469,
    "Saint Paul", "2045", "PLDV", 0.79960899315738,
    "Saint Paul", "2050", "PLDV", 0.773883546749061
  )
)


testthat::expect_equal(
  vmt_stock_proportion(
    .tb = st_paul_passenger,
    .mode = "BU",
    .stock = "BCIStock"
  ) %>% select(-geog_id),
  tibble::tribble(
    ~geog_name, ~year, ~mode, ~mode_stock_adj,
    "Saint Paul", "2015", "BU", 1,
    "Saint Paul", "2018", "BU", 1,
    "Saint Paul", "2020", "BU", 1,
    "Saint Paul", "2025", "BU", 1,
    "Saint Paul", "2030", "BU", 1,
    "Saint Paul", "2035", "BU", 1,
    "Saint Paul", "2040", "BU", 1,
    "Saint Paul", "2045", "BU", 1,
    "Saint Paul", "2050", "BU", 1
  )
)


testthat::expect_equal(
  vmt_stock_proportion(
    .tb = st_paul_passenger,
    .mode = "RU",
    .stock = "EVStock"
  ) %>% select(-geog_id),
  tibble::tribble(
    ~geog_name, ~year, ~mode, ~mode_stock_adj,
    "Saint Paul", "2015", "RU", 1,
    "Saint Paul", "2018", "RU", 1,
    "Saint Paul", "2020", "RU", 1,
    "Saint Paul", "2025", "RU", 1,
    "Saint Paul", "2030", "RU", 1,
    "Saint Paul", "2035", "RU", 1,
    "Saint Paul", "2040", "RU", 1,
    "Saint Paul", "2045", "RU", 1,
    "Saint Paul", "2050", "RU", 1
  )
)



testthat::expect_error(
  vmt_stock_proportion(
    .tb = st_paul_passenger,
    .mode = "BU",
    .stock = "SIStock"
  )
)
