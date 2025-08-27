# TODO write tests
vmt_test <- tibble::tribble(
  ~scenario, ~mode, ~stock, ~geog_name, ~year, ~aeo_mode, ~type, ~vmt, ~class,
  "BAU", "PLDV", "SIStock", "Saint Paul", "2015", "LDV", "P", NA, "SI",
  "BAU", "PLDV", "SIStock", "Saint Paul", "2018", "LDV", "P", 2212217.09054726, "SI",
  "BAU", "PLDV", "SIStock", "Saint Paul", "2020", "LDV", "P", NA, "SI",
  "BAU", "PLDV", "SIStock", "Saint Paul", "2025", "LDV", "P", 2085360.41006151, "SI",
  "BAU", "PLDV", "SIStock", "Saint Paul", "2030", "LDV", "P", 2045594.73396012, "SI",
  "BAU", "PLDV", "SIStock", "Saint Paul", "2035", "LDV", "P", 2017027.72754979, "SI",
  "BAU", "PLDV", "SIStock", "Saint Paul", "2040", "LDV", "P", 1888351.83369188, "SI",
  "BAU", "PLDV", "SIStock", "Saint Paul", "2045", "LDV", "P", 1830828.02849089, "SI",
  "BAU", "PLDV", "SIStock", "Saint Paul", "2050", "LDV", "P", 1775908.76643119, "SI"
)


tibble::tribble(
  ~scenario, ~type, ~mode, ~geog_name, ~year, ~aeo_mode, ~class, ~vmt_cost,
  "BAU", "P", "PLDV", "Saint Paul", "2015", "LDV", "SI", NA,
  "BAU", "P", "PLDV", "Saint Paul", "2018", "LDV", "SI", 1358.25802262972,
  "BAU", "P", "PLDV", "Saint Paul", "2020", "LDV", "SI", NA,
  "BAU", "P", "PLDV", "Saint Paul", "2025", "LDV", "SI", 1316.04627872009,
  "BAU", "P", "PLDV", "Saint Paul", "2030", "LDV", "SI", 1305.50821442049,
  "BAU", "P", "PLDV", "Saint Paul", "2035", "LDV", "SI", 1294.79893341947,
  "BAU", "P", "PLDV", "Saint Paul", "2040", "LDV", "SI", 1215.10895235209,
  "BAU", "P", "PLDV", "Saint Paul", "2045", "LDV", "SI", 1181.00870650218,
  "BAU", "P", "PLDV", "Saint Paul", "2050", "LDV", "SI", 1148.0583143645
)
