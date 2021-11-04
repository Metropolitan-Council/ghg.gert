
st_paul_passenger <- transportation_data$passenger %>%
  filter(ctu == "St. Paul")

## Passenger, gasoline-----
si_test_table <- tibble::tribble(
  ~year, ~type, ~scenario,  ~mode, ~class,       ~ctu,    ~ghg_embodied,
  "2015",   "P",     "BAU", "PLDV",   "SI", "St. Paul", 104625.267021283,
  "2018",   "P",     "BAU", "PLDV",   "SI", "St. Paul", 110093.385085127,
  "2020",   "P",     "BAU", "PLDV",   "SI", "St. Paul",  113738.79712769,
  "2025",   "P",     "BAU", "PLDV",   "SI", "St. Paul", 122071.558346099,
  "2030",   "P",     "BAU", "PLDV",   "SI", "St. Paul", 114793.808456975,
  "2035",   "P",     "BAU", "PLDV",   "SI", "St. Paul", 97857.9895132779,
  "2040",   "P",     "BAU", "PLDV",   "SI", "St. Paul", 56501.0455869516
)

si_emb_ghg <-
  calc_ghg_embodied(
    tb = st_paul_passenger,
    .mode = "PLDV",
    .class = "SI",
    .sales_mode = "SISales",
    .fuel_type = "SI-EMB"
  )

testthat::expect_equal(
  si_emb_ghg$ghg_embodied[1:7],
  si_test_table$ghg_embodied)


## Bus, battery -----

bu_test_table <- tibble::tribble(
  ~year, ~type, ~scenario, ~mode, ~class,       ~ctu, ~ghg_embodied,
  "2015",   "P",     "BAU",  "BU",  "BEV", "St. Paul",             0,
  "2018",   "P",     "BAU",  "BU",  "BEV", "St. Paul",         90.78,
  "2020",   "P",     "BAU",  "BU",  "BEV", "St. Paul",         151.3,
  "2025",   "P",     "BAU",  "BU",  "BEV", "St. Paul",        1966.9,
  "2030",   "P",     "BAU",  "BU",  "BEV", "St. Paul",        6203.3,
  "2035",   "P",     "BAU",  "BU",  "BEV", "St. Paul",        2874.7,
  "2040",   "P",     "BAU",  "BU",  "BEV", "St. Paul",          6052
)

bu_bev <-
  calc_ghg_embodied(
    tb = st_paul_passenger,
    .mode = "BU",
    .class = "BEV",
    .sales_mode = "BEVSales",
    .fuel_type = "BU-BEV-EMB"
  )

testthat::expect_equal(
  bu_bev$ghg_embodied[1:7],
  bu_test_table$ghg_embodied)

