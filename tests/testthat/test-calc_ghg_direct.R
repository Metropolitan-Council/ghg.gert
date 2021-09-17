
# Gasoline -----

si_vmt_test <- tibble::tribble(
  ~type, ~stock, ~scenario, ~ctu, ~year, ~mode, ~aeo_mode, ~vmt, ~class,
  "P", "SIStock", "BAU", "St. Paul", "2015", "PLDV", "LDV", 0, "SI",
  "P", "SIStock", "BAU", "St. Paul", "2018", "PLDV", "LDV", 22.1221709054726, "SI",
  "P", "SIStock", "BAU", "St. Paul", "2020", "PLDV", "LDV", 0, "SI",
  "P", "SIStock", "BAU", "St. Paul", "2025", "PLDV", "LDV", 20.8536041006151, "SI",
  "P", "SIStock", "BAU", "St. Paul", "2030", "PLDV", "LDV", 20.4559473396012, "SI",
  "P", "SIStock", "BAU", "St. Paul", "2035", "PLDV", "LDV", 20.1702772754979, "SI",
  "P", "SIStock", "BAU", "St. Paul", "2040", "PLDV", "LDV", 18.8835183369188, "SI",
  "P", "SIStock", "BAU", "St. Paul", "2045", "PLDV", "LDV", 18.3082802849089, "SI",
  "P", "SIStock", "BAU", "St. Paul", "2050", "PLDV", "LDV", 17.7590876643119, "SI"
)


si_dir_ghg <- calc_ghg_direct(
  tb_vmt = si_vmt_test,
  tb = transportation_data$passenger,
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
  ~scenario, ~mode, ~stock, ~ctu, ~year, ~aeo_mode, ~type, ~vmt, ~class,
  "BAU", "PLDV", "CIStock", "St. Paul", "2015", "LDV", "P", NA, "CI",
  "BAU", "PLDV", "CIStock", "St. Paul", "2018", "LDV", "P", 32154.8660412793, "CI",
  "BAU", "PLDV", "CIStock", "St. Paul", "2020", "LDV", "P", NA, "CI",
  "BAU", "PLDV", "CIStock", "St. Paul", "2025", "LDV", "P", 32757.8572046905, "CI",
  "BAU", "PLDV", "CIStock", "St. Paul", "2030", "LDV", "P", 32435.0232511664, "CI",
  "BAU", "PLDV", "CIStock", "St. Paul", "2035", "LDV", "P", 31922.7114245039, "CI",
  "BAU", "PLDV", "CIStock", "St. Paul", "2040", "LDV", "P", 31521.7992720498, "CI",
  "BAU", "PLDV", "CIStock", "St. Paul", "2045", "LDV", "P", 31452.9126022104, "CI",
  "BAU", "PLDV", "CIStock", "St. Paul", "2050", "LDV", "P", 31389.9242344753, "CI"
)


si_dir_ghg <- calc_ghg_direct(
  tb_vmt = ci_vmt_test,
  tb = transportation_data$passenger,
  .mode = "PLDV",
  .fuel_type = "CI",
  .aeo_scenario = "REF",
  .miles_per_gallon = "CIMPG"
)
