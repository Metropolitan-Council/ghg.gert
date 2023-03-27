## Passenger, gasoline-----
si_test_table <- tibble::tribble(
  ~type, ~ghg_embodied_source, ~mode, ~class, ~ctu, ~year, ~aeo_mode, ~ghg_embodied,
  "P", "SISales", "PLDV", "SI", "St. Paul", "2015", "LDV", 610302.943242237,
  "P", "SISales", "PLDV", "SI", "St. Paul", "2018", "LDV", 633901.98119463,
  "P", "SISales", "PLDV", "SI", "St. Paul", "2020", "LDV", 649634.702099744,
  "P", "SISales", "PLDV", "SI", "St. Paul", "2025", "LDV", 616221.844852737,
  "P", "SISales", "PLDV", "SI", "St. Paul", "2030", "LDV", 617631.727216358,
  "P", "SISales", "PLDV", "SI", "St. Paul", "2035", "LDV", 616071.899345271,
  "P", "SISales", "PLDV", "SI", "St. Paul", "2040", "LDV", 583946.600495268,
  "P", "SISales", "PLDV", "SI", "St. Paul", "2045", "LDV", 578675.358252104,
  "P", "SISales", "PLDV", "SI", "St. Paul", "2050", "LDV", 573404.11600894
)


si_emb_ghg <-
  calc_ghg_embodied(
    tb = st_paul_passenger,
    .mode = "PLDV",
    .class = "SI",
    .sales_mode = "SISales",
    .fuel_type = "SI-EMB"
  ) %>%
  mutate(ghg_embodied = ghg_embodied * 1000)

testthat::expect_equal(
  si_emb_ghg$ghg_embodied,
  si_test_table$ghg_embodied
)


## Bus, battery -----

bu_test_table <- tibble::tribble(
  ~year, ~type, ~scenario, ~mode, ~class, ~ctu, ~ghg_embodied,
  "2015", "P", "BAU", "BU", "BEV", "St. Paul", 0,
  "2018", "P", "BAU", "BU", "BEV", "St. Paul", 0,
  "2020", "P", "BAU", "BU", "BEV", "St. Paul", 0,
  "2025", "P", "BAU", "BU", "BEV", "St. Paul", 0,
  "2030", "P", "BAU", "BU", "BEV", "St. Paul", 0,
  "2035", "P", "BAU", "BU", "BEV", "St. Paul", 0,
  "2040", "P", "BAU", "BU", "BEV", "St. Paul", 0
)

bu_bev <-
  calc_ghg_embodied(
    tb = st_paul_passenger,
    .mode = "BU",
    .class = "BEV",
    .sales_mode = "BEVSales",
    .fuel_type = "BU-BEV-EMB"
  ) %>%
  mutate(ghg_embodied = ghg_embodied * 1000)


testthat::expect_equal(
  bu_bev$ghg_embodied[1:7],
  bu_test_table$ghg_embodied
)
