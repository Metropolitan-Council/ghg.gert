# Gasoline -----
testthat::test_that("Gasoline direct emissions correct", {
  # vmt in this table is in thousands,
  # so multiply by 1000 to get to miles
  si_vmt_test <- tibble::tribble(
    ~type, ~stock, ~scenario, ~geog_name, ~year, ~mode, ~aeo_mode, ~vmt, ~class,
    "P", "SIStock", "BAU", "Saint Paul", "2015", "PLDV", "LDV", 22.1801380413249, "SI",
    "P", "SIStock", "BAU", "Saint Paul", "2018", "PLDV", "LDV", 22.1221709054726, "SI",
    "P", "SIStock", "BAU", "Saint Paul", "2020", "PLDV", "LDV", 22.0886226208252, "SI",
    "P", "SIStock", "BAU", "Saint Paul", "2025", "PLDV", "LDV", 20.8536041006151, "SI",
    "P", "SIStock", "BAU", "Saint Paul", "2030", "PLDV", "LDV", 20.4559473396012, "SI",
    "P", "SIStock", "BAU", "Saint Paul", "2035", "PLDV", "LDV", 20.1702772754979, "SI",
    "P", "SIStock", "BAU", "Saint Paul", "2040", "PLDV", "LDV", 18.8835183369188, "SI",
    "P", "SIStock", "BAU", "Saint Paul", "2045", "PLDV", "LDV", 18.3082802849089, "SI",
    "P", "SIStock", "BAU", "Saint Paul", "2050", "PLDV", "LDV", 17.7590876643119, "SI"
  ) %>%
    mutate(vmt = vmt * 1000) %>%
    left_join(geog_index %>% select(geog_name, geog_id), by = "geog_name")


  si_dir_ghg <- calc_ghg_direct(
    tb_vmt = si_vmt_test,
    tb = st_paul_passenger,
    .mode = "PLDV",
    .fuel_type = "SI",
    .aeo_scenario = "REF",
    .miles_per_gallon = "SIMPG",
    .factor_values = factor_values,
    .enviro_factors = enviro_factors
  )

  testthat::expect_equal(dim(si_dir_ghg)[1], 9)

  # there is something wrong with the DIR-GHG column
  # out_sum_long %>%
  #   filter(geog_name == "Saint Paul",
  #          mode == "PLDV",
  #          class == "SI") %>%
  #   mutate(em = `DIR-GHG`/VMT)
})
# Diesel -----

testthat::test_that("Diesel direct emissions correct", {
  ci_vmt_test <- tibble::tribble(
    ~type, ~stock, ~scenario, ~geog_name, ~year, ~mode, ~aeo_mode, ~vmt, ~class,
    "P", "CIStock", "BAU", "Saint Paul", "2015", "PLDV", "LDV", 0.319054791655997, "CI",
    "P", "CIStock", "BAU", "Saint Paul", "2018", "PLDV", "LDV", 0.321548660412793, "CI",
    "P", "CIStock", "BAU", "Saint Paul", "2020", "PLDV", "LDV", 0.323142027478914, "CI",
    "P", "CIStock", "BAU", "Saint Paul", "2025", "PLDV", "LDV", 0.327578572046905, "CI",
    "P", "CIStock", "BAU", "Saint Paul", "2030", "PLDV", "LDV", 0.324350232511664, "CI",
    "P", "CIStock", "BAU", "Saint Paul", "2035", "PLDV", "LDV", 0.319227114245039, "CI",
    "P", "CIStock", "BAU", "Saint Paul", "2040", "PLDV", "LDV", 0.315217992720498, "CI",
    "P", "CIStock", "BAU", "Saint Paul", "2045", "PLDV", "LDV", 0.314529126022104, "CI",
    "P", "CIStock", "BAU", "Saint Paul", "2050", "PLDV", "LDV", 0.313899242344753, "CI"
  ) %>%
    left_join(geog_index %>% select(geog_name, geog_id), by = "geog_name")



  ci_dir_ghg <- calc_ghg_direct(
    tb_vmt = ci_vmt_test,
    tb = st_paul_passenger,
    .mode = "PLDV",
    .fuel_type = "CI",
    .aeo_scenario = "REF",
    .miles_per_gallon = "CIMPG",
    .factor_values = factor_values,
    .enviro_factors = enviro_factors
  )


  testthat::expect_equal(dim(ci_dir_ghg)[1], 9)
})
# battery electric -----

testthat::test_that("Battery direct ghg", {
  bev_vmt_test <- tibble::tribble(
    ~type, ~stock, ~scenario, ~geog_name, ~year, ~mode, ~aeo_mode, ~vmt, ~class,
    "P", "BEVStock", "BAU", "Saint Paul", "2015", "PLDV", "LDV", 0.319054791655997, "BEV",
    "P", "BEVStock", "BAU", "Saint Paul", "2018", "PLDV", "LDV", 0.321548660412793, "BEV",
    "P", "BEVStock", "BAU", "Saint Paul", "2020", "PLDV", "LDV", 0.323142027478914, "BEV",
    "P", "BEVStock", "BAU", "Saint Paul", "2025", "PLDV", "LDV", 0.327578572046905, "BEV",
    "P", "BEVStock", "BAU", "Saint Paul", "2030", "PLDV", "LDV", 0.324350232511664, "BEV",
    "P", "BEVStock", "BAU", "Saint Paul", "2035", "PLDV", "LDV", 0.319227114245039, "BEV",
    "P", "BEVStock", "BAU", "Saint Paul", "2040", "PLDV", "LDV", 0.315217992720498, "BEV",
    "P", "BEVStock", "BAU", "Saint Paul", "2045", "PLDV", "LDV", 0.314529126022104, "BEV",
    "P", "BEVStock", "BAU", "Saint Paul", "2050", "PLDV", "LDV", 0.313899242344753, "BEV"
  ) %>%
    left_join(geog_index %>% select(geog_name, geog_id), by = "geog_name")


  fcm <- calc_fuel_cost_mile(
    st_paul_passenger,
    .mode = "PLDV",
    .aeo_scenario = "REF",
    .miles_per_gallon = "BEVElec",
    .fuel_cost_gallon = enviro_factors$ELEC_FUEL_COST_KWH,
    .fuel_economy = fuel_economy,
    .enviro_factors = enviro_factors,
    .factor_values = factor_values
  )

  testthat::expect_no_warning(
    calc_ghg_direct(
      tb_vmt = bev_vmt_test,
      tb = st_paul_passenger,
      .mode = "PLDV",
      .fuel_type = "ER",
      .aeo_scenario = "REF",
      .miles_per_gallon = "BEVElec"
    )
  )

  bev_dir_ghg <- calc_ghg_direct(
    tb_vmt = bev_vmt_test,
    tb = st_paul_passenger,
    .mode = "PLDV",
    .fuel_type = "ER",
    .aeo_scenario = "REF",
    .miles_per_gallon = "BEVElec"
  )


  bev_dir_ghg_decarb <- calc_ghg_direct(
    tb_vmt = bev_vmt_test,
    tb = st_paul_passenger,
    .mode = "PLDV",
    .fuel_type = "ER",
    .aeo_scenario = "REF",
    .miles_per_gallon = "BEVElec"
  ) %>%
    filter(year == "2050")

  # when grid is fully decarbonized,
  # BEV emissions are 0
  testthat::expect_equal(bev_dir_ghg_decarb$dir_ghg %>% sum(na.rm = T), 0, tolerance = 0.1)
})
