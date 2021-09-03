
# Gasoline ------

vmt_test <- tibble::tribble(
  ~scenario, ~mode, ~stock, ~ctu, ~year, ~aeo_mode, ~type, ~vmt, ~class,
  "BAU", "PLDV", "SIStock", "St. Paul", "2015", "LDV", "P", NA, "SI",
  "BAU", "PLDV", "SIStock", "St. Paul", "2018", "LDV", "P", 2212217.09054726, "SI",
  "BAU", "PLDV", "SIStock", "St. Paul", "2020", "LDV", "P", NA, "SI",
  "BAU", "PLDV", "SIStock", "St. Paul", "2025", "LDV", "P", 2085360.41006151, "SI",
  "BAU", "PLDV", "SIStock", "St. Paul", "2030", "LDV", "P", 2045594.73396012, "SI",
  "BAU", "PLDV", "SIStock", "St. Paul", "2035", "LDV", "P", 2017027.72754979, "SI",
  "BAU", "PLDV", "SIStock", "St. Paul", "2040", "LDV", "P", 1888351.83369188, "SI",
  "BAU", "PLDV", "SIStock", "St. Paul", "2045", "LDV", "P", 1830828.02849089, "SI",
  "BAU", "PLDV", "SIStock", "St. Paul", "2050", "LDV", "P", 1775908.76643119, "SI"
)


si_fuel_use <- calc_fuel_use(
  tb_vmt = vmt_test,
  tb = transportation_data$passenger,
  .mode = "PLDV",
  .fuel_type = "SI",
  .aeo_scenario = "REF",
  .miles_per_gallon = "SIMPG",
  .is_av = 0
)


tibble::tribble(
  ~type, ~scenario, ~mode, ~ctu, ~year, ~AEOScen, ~aeo_mode, ~class, ~fuel_use,
  "P", "BAU", "PLDV", "St. Paul", "2015", "REF", "LDV", "SI", NA,
  "P", "BAU", "PLDV", "St. Paul", "2018", "REF", "LDV", "SI", 59547349.0661272,
  "P", "BAU", "PLDV", "St. Paul", "2020", "REF", "LDV", "SI", NA,
  "P", "BAU", "PLDV", "St. Paul", "2025", "REF", "LDV", "SI", 57313473.7460134,
  "P", "BAU", "PLDV", "St. Paul", "2030", "REF", "LDV", "SI", 57063873.2846238,
  "P", "BAU", "PLDV", "St. Paul", "2035", "REF", "LDV", "SI", 57110973.1138399,
  "P", "BAU", "PLDV", "St. Paul", "2040", "REF", "LDV", "SI", 54269603.4878194,
  "P", "BAU", "PLDV", "St. Paul", "2045", "REF", "LDV", "SI", 53405665.1978367,
  "P", "BAU", "PLDV", "St. Paul", "2050", "REF", "LDV", "SI", 52580712.8421712
)


out_sum_long %>%
  filter(
    mode == "PLDV",
    class == "CI"
  )


# Diesel ------
