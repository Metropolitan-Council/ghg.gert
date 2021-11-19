testthat::test_that("Expected number of rows", {
  t_with_drs <- adj_fleet_shares(
    .bev_pct_sales = 0,
    .phev_pct_sales = 0,
    .hev_pct_sales = 0,
    .pass_tb = st_paul_passenger,
    .freight_tb = st_paul_freight,
    .vmt_fee = 0,
    .payd_fee = 0,
    .gas_tax = 0,
    .av_pct = 0,
    .drs_pct = .10,
    .enviro_factors = enviro_factors
  )


  testthat::expect_equal(nrow(t_with_drs$pass), nrow(st_paul_passenger))
  testthat::expect_equal(nrow(t_with_drs$freight), nrow(st_paul_freight))

  t_with_av <- adj_fleet_shares(
    .bev_pct_sales = 0,
    .phev_pct_sales = 0,
    .hev_pct_sales = 0,
    .pass_tb = st_paul_passenger,
    .freight_tb = st_paul_freight,
    .vmt_fee = 0,
    .payd_fee = 0,
    .gas_tax = 0,
    .av_pct = .05,
    .drs_pct = 0,
    .enviro_factors = enviro_factors
  )

  testthat::expect_equal(nrow(t_with_av$freight), nrow(st_paul_freight))
})

t_with_av <- adj_fleet_shares(
  .bev_pct_sales = 0,
  .phev_pct_sales = 0,
  .hev_pct_sales = 0,
  .pass_tb = st_paul_passenger,
  .freight_tb = st_paul_freight,
  .vmt_fee = 0,
  .payd_fee = 0,
  .gas_tax = 0,
  .av_pct = 0.05,
  .drs_pct = 0,
  .enviro_factors = enviro_factors
)

t_with_av$pass %>%
  filter(var == "AVStock")

pass_av <- t_with_av$pass %>%
  filter(
    var == "TotStock",
    mode == "PLDV"
  )


testthat::expect_equal(
  tolerance = 0.001,
  pass_av$value[1:7],
  c(
    154401.15,
    161234.142,
    165789.47,
    166827.35,
    170255.75,
    170439.518,
    169406.28
  )
)


# Stock adjustments only adjust the proportion of the total vehicles
# The total number of vehicles should NOT change


t_hev_bev_phev <- adj_fleet_shares(
  .bev_pct_sales = .20,
  .phev_pct_sales = .15,
  .hev_pct_sales = .40,
  .pass_tb = st_paul_passenger,
  .freight_tb = st_paul_freight,
  .vmt_fee = 0,
  .payd_fee = 0,
  .gas_tax = 0,
  .drs_pct = 0,
  .av_pct = 0,
  .enviro_factors = enviro_factors
)


test_total_table <- left_join(
  st_paul_passenger %>%
    filter(
      mode == "PLDV",
      str_detect(var, "Tot")
    ),
  t_hev_bev_phev$pass %>%
    filter(
      mode == "PLDV",
      str_detect(var, "Tot")
    ),
  c("mode", "var", "ctu", "year", "aeo_mode", "type"),
  suffix = c(".orig", ".adj")
) %>%
  mutate(diff = round(value.orig - value.adj)) %>%
  filter(diff != 0)

testthat::expect_equal(nrow(test_total_table), 0)



testthat::expect_warning(
  adj_fleet_shares(
    .bev_pct_sales = 1,
    .pass_tb = st_paul_passenger,
    .freight_tb = st_paul_freight %>%
      filter(ctu == "St. Paul"),
    .vmt_fee = 0,
    .payd_fee = 0,
    .gas_tax = 0,
    .drs_pct = 0,
    .av_pct = 0,
    .enviro_factors = enviro_factors
  )
)

# t_hev_bev_phev$freight


freight_test_total_table <- left_join(
  st_paul_freight %>%
    filter(
      mode %in% c("SUT", "CUT"),
      str_detect(var, "Tot")
    ),
  t_hev_bev_phev$freight %>%
    filter(
      mode %in% c("SUT", "CUT"),
      str_detect(var, "Tot")
    ),
  c("mode", "var", "ctu", "year", "aeo_mode", "type"),
  suffix = c(".orig", ".adj")
) %>%
  mutate(diff = round(value.orig - value.adj)) %>%
  filter(diff != 0)

testthat::expect_equal(nrow(freight_test_total_table), 0)



t_with_drs <- adj_fleet_shares(
  .bev_pct_sales = 0,
  .phev_pct_sales = 0,
  .hev_pct_sales = 0,
  .pass_tb = st_paul_passenger,
  .freight_tb = st_paul_freight,
  .vmt_fee = 0,
  .payd_fee = 0,
  .gas_tax = 0,
  .av_pct = 0,
  .drs_pct = .10,
  .enviro_factors = enviro_factors
)

testthat::expect_equal(
  t_with_drs$pass %>%
    filter(
      mode %in% c(
        "PLDV"
      ),
      str_detect(var, "BEV")
    ),
  tibble::tribble(
    ~mode, ~var, ~ctu, ~year, ~value, ~aeo_mode, ~type,
    "PLDV", "BEVStock", "St. Paul", "2015", 19.21, "LDV", "P",
    "PLDV", "BEVStock", "St. Paul", "2018", 296.608, "LDV", "P",
    "PLDV", "BEVStock", "St. Paul", "2020", 481.54, "LDV", "P",
    "PLDV", "BEVStock", "St. Paul", "2025", 5662.04013688712, "LDV", "P",
    "PLDV", "BEVStock", "St. Paul", "2030", 5523.3879512438, "LDV", "P",
    "PLDV", "BEVStock", "St. Paul", "2035", 4764.09114897209, "LDV", "P",
    "PLDV", "BEVStock", "St. Paul", "2040", 9085.7997046668, "LDV", "P",
    "PLDV", "BEVStock", "St. Paul", "2045", 10659.132348616, "LDV", "P",
    "PLDV", "BEVStock", "St. Paul", "2050", 12349.062, "LDV", "P",
    "PLDV", "BEVExist", "St. Paul", "2015", 10.82, "LDV", "P",
    "PLDV", "BEVExist", "St. Paul", "2018", 291.128, "LDV", "P",
    "PLDV", "BEVExist", "St. Paul", "2020", 478, "LDV", "P",
    "PLDV", "BEVExist", "St. Paul", "2025", 5602.35845260616, "LDV", "P",
    "PLDV", "BEVExist", "St. Paul", "2030", 5224.50980631635, "LDV", "P",
    "PLDV", "BEVExist", "St. Paul", "2035", 3704.25907809269, "LDV", "P",
    "PLDV", "BEVExist", "St. Paul", "2040", 7398.5995735068, "LDV", "P",
    "PLDV", "BEVExist", "St. Paul", "2045", 8680.637780793, "LDV", "P",
    "PLDV", "BEVExist", "St. Paul", "2050", 10057.626, "LDV", "P",
    "PLDV", "BEVSales", "St. Paul", "2015", 8.39, "LDV", "P",
    "PLDV", "BEVSales", "St. Paul", "2018", 5.48, "LDV", "P",
    "PLDV", "BEVSales", "St. Paul", "2020", 3.54, "LDV", "P",
    "PLDV", "BEVSales", "St. Paul", "2025", 59.69167117592, "LDV", "P",
    "PLDV", "BEVSales", "St. Paul", "2030", 298.87814492745, "LDV", "P",
    "PLDV", "BEVSales", "St. Paul", "2035", 1059.8320708794, "LDV", "P",
    "PLDV", "BEVSales", "St. Paul", "2040", 1687.20013116, "LDV", "P",
    "PLDV", "BEVSales", "St. Paul", "2045", 1978.485506832, "LDV", "P",
    "PLDV", "BEVSales", "St. Paul", "2050", 2291.436, "LDV", "P",
    "PLDV", "BEVElec", "St. Paul", "2015", 3.579, "LDV", "P",
    "PLDV", "BEVElec", "St. Paul", "2018", 3.579, "LDV", "P",
    "PLDV", "BEVElec", "St. Paul", "2020", 3.579, "LDV", "P",
    "PLDV", "BEVElec", "St. Paul", "2025", 3.579, "LDV", "P",
    "PLDV", "BEVElec", "St. Paul", "2030", 3.579, "LDV", "P",
    "PLDV", "BEVElec", "St. Paul", "2035", 3.579, "LDV", "P",
    "PLDV", "BEVElec", "St. Paul", "2040", 3.579, "LDV", "P",
    "PLDV", "BEVElec", "St. Paul", "2045", 3.579, "LDV", "P",
    "PLDV", "BEVElec", "St. Paul", "2050", 3.579, "LDV", "P"
  )
)
