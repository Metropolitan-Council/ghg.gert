

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
    "PLDV", "PMT", "St. Paul", "2025", 2802986.8603496, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2030", 2790216.26881905, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2035", 2725571.46939664, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2040", 2617859.77166972, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2045", 2565917.6062908, "LDV", "P",
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
    "PLDV", "PMT", "St. Paul", "2025", 2795627.7810488, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2030", 2745184.42645715, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2035", 2538518.68818992, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2040", 2202652.25500916, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2045", 2034094.4188724, "LDV", "P",
    "PLDV", "PMT", "St. Paul", "2050", 1986736.409, "LDV", "P"
  )
)
