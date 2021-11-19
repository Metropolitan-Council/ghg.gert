

testthat::expect_equal(
  vmt_dynamic_ride_share_reduction(
    .pass_tb = st_paul_passenger,
    .drs_pct = 0.1,
    .enviro_factors = enviro_factors
  ) %>%
    filter(
      mode == "PLDV",
      var == "PMT"
    ),
  tibble::tribble(
    ~mode, ~var, ~ctu, ~year, ~value, ~aeo_mode, ~type,
    "PLDV", "PMT", "St. Paul", "2015", 2787816.29, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2018", 2795486.876, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2020", 2800600.6, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2025", 2802988.25245613, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2030", 2790216.00160949, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2035", 2725570.44606412, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2040", 2617859.65865118, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2045", 2565917.88947372, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2050", 2554375.383, "LDV", "P"
  )
)


testthat::expect_equal(
  vmt_dynamic_ride_share_reduction(
    .pass_tb = st_paul_passenger,
    .drs_pct = 0,
    .enviro_factors = enviro_factors
  ) %>%
    filter(
      mode == "PLDV",
      var == "PMT"
    ),
  st_paul_passenger %>%
    filter(
      mode == "PLDV",
      var == "PMT"
    )
)



testthat::expect_equal(
  vmt_dynamic_ride_share_reduction(
    .pass_tb = st_paul_passenger,
    .drs_pct = 0.3,
    .enviro_factors = enviro_factors
  ) %>%
    filter(
      mode == "PLDV",
      var == "PMT"
    ),
  tibble::tribble(
    ~mode, ~var, ~ctu, ~year, ~value, ~aeo_mode, ~type,
    "PLDV", "PMT", "St. Paul", "2015", 2787816.29, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2018", 2795486.876, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2020", 2800600.6, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2025", 2795631.9573684, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2030", 2745183.62482848, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2035", 2538515.61819235, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2040", 2202651.91595354, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2045", 2034095.26842116, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2050", 1986736.409, "LDV", "P"
  )
)
