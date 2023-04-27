# Gasoline -----

si_vmt_test <- tibble::tribble(
  ~type, ~stock, ~scenario, ~ctu, ~year, ~mode, ~aeo_mode, ~vmt, ~class,
  "P", "SIStock", "BAU", "St. Paul", "2015", "PLDV", "LDV", 22.1801380413249, "SI",
  "P", "SIStock", "BAU", "St. Paul", "2018", "PLDV", "LDV", 22.1221709054726, "SI",
  "P", "SIStock", "BAU", "St. Paul", "2020", "PLDV", "LDV", 22.0886226208252, "SI",
  "P", "SIStock", "BAU", "St. Paul", "2025", "PLDV", "LDV", 20.8536041006151, "SI",
  "P", "SIStock", "BAU", "St. Paul", "2030", "PLDV", "LDV", 20.4559473396012, "SI",
  "P", "SIStock", "BAU", "St. Paul", "2035", "PLDV", "LDV", 20.1702772754979, "SI",
  "P", "SIStock", "BAU", "St. Paul", "2040", "PLDV", "LDV", 18.8835183369188, "SI",
  "P", "SIStock", "BAU", "St. Paul", "2045", "PLDV", "LDV", 18.3082802849089, "SI",
  "P", "SIStock", "BAU", "St. Paul", "2050", "PLDV", "LDV", 17.7590876643119, "SI"
) %>%
  mutate(vmt = vmt*1000)


si_dir_ghg <- calc_ghg_direct(
  tb_vmt = si_vmt_test,
  tb = st_paul_passenger,
  .mode = "PLDV",
  .fuel_type = "SI",
  .aeo_scenario = "REF",
  .miles_per_gallon = "SIMPG",
  .enviro_factors = enviro_factors
)

testthat::expect_equal(dim(si_dir_ghg)[1], 9)

# there is something wrong with the DIR-GHG column
# out_sum_long %>%
#   filter(ctu == "St. Paul",
#          mode == "PLDV",
#          class == "SI") %>%
#   mutate(em = `DIR-GHG`/VMT)

# Diesel -----


ci_vmt_test <- tibble::tribble(
  ~type, ~stock, ~scenario, ~ctu, ~year, ~mode, ~aeo_mode, ~vmt, ~class,
  "P", "CIStock", "BAU", "St. Paul", "2015", "PLDV", "LDV", 0.319054791655997, "CI",
  "P", "CIStock", "BAU", "St. Paul", "2018", "PLDV", "LDV", 0.321548660412793, "CI",
  "P", "CIStock", "BAU", "St. Paul", "2020", "PLDV", "LDV", 0.323142027478914, "CI",
  "P", "CIStock", "BAU", "St. Paul", "2025", "PLDV", "LDV", 0.327578572046905, "CI",
  "P", "CIStock", "BAU", "St. Paul", "2030", "PLDV", "LDV", 0.324350232511664, "CI",
  "P", "CIStock", "BAU", "St. Paul", "2035", "PLDV", "LDV", 0.319227114245039, "CI",
  "P", "CIStock", "BAU", "St. Paul", "2040", "PLDV", "LDV", 0.315217992720498, "CI",
  "P", "CIStock", "BAU", "St. Paul", "2045", "PLDV", "LDV", 0.314529126022104, "CI",
  "P", "CIStock", "BAU", "St. Paul", "2050", "PLDV", "LDV", 0.313899242344753, "CI"
)


ci_dir_ghg <- calc_ghg_direct(
  tb_vmt = ci_vmt_test,
  tb = st_paul_passenger,
  .mode = "PLDV",
  .fuel_type = "CI",
  .aeo_scenario = "REF",
  .miles_per_gallon = "CIMPG"
)


testthat::expect_equal(dim(ci_dir_ghg)[1], 9)

# battery electric -----

testthat::test_that("Battery direct ghg", {
  fcm <- calc_fuel_cost_mile(
    st_paul_passenger,
    .mode = "PLDV",
    .aeo_scenario = "REF",
    "BEVElec",
    .fuel_cost_gallon = enviro_factors$ELEC_FUEL_COST_KWH
  )

  testthat::expect_warning(calc_ghg_direct(
    tb_vmt = ci_vmt_test,
    tb = st_paul_passenger,
    .mode = "PLDV",
    .fuel_type = "BEV",
    .aeo_scenario = "REF",
    .miles_per_gallon = "BEVElec",
    .grid_decarbonization_pct = 0
  ))

  bev_dir_ghg <- calc_ghg_direct(
    tb_vmt = ci_vmt_test,
    tb = st_paul_passenger,
    .mode = "PLDV",
    .fuel_type = "BEV",
    .aeo_scenario = "REF",
    .miles_per_gallon = "BEVElec",
    .grid_decarbonization_pct = 1
  )

  bev_dir_ghg
})
