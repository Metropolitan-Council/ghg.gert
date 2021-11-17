

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


si_veh_test <- vmt_vehicle_occupancy(
  tb = transportation_data$passenger,
  .tb_vmt = si_vmt_test,
  .mode = "PLDV",
  .stock = "SIStock",
  .transit_avo_pct = 0,
  .enviro_factors = enviro_factors
) %>%
  filter(ctu == "St. Paul")

testthat::expect_equal(
  si_veh_test$occupancy_adj,
  c(
    1.23679344, 1.23679344, 1.23679344, 1.23679344, 1.23679344,
    1.23679344, 1.23679344, 1.23679344, 1.23679344
  )
)

