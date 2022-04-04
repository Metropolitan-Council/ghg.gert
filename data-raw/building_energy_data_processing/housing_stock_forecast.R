library(ghg.sp)
library(dplyr)
library(tidyr)

ctu_county <- councilR::import_from_gis(query = "CountiesAndCTUs") %>%
  select(COCTU_ID, CTU_NAME)

housing_stock_forecast <- readxl::read_xlsx("data-raw/building_energy_data_processing/building-data/FORECAST_LU_TAZCTU.xlsx") %>%
  select(-TAZ2012) %>%
  group_by(COCTU_ID) %>%
  summarize(across(3:66, sum)) %>%
  select(COCTU_ID, 25:32) %>%
  left_join(ctu_county %>%
    sf::st_drop_geometry()) %>%
  select(-COCTU_ID) %>%
  group_by(CTU_NAME) %>%
  summarize(across(1:8, sum, na.rm = T)) %>%
  pivot_longer(cols = 2:9) %>%
  mutate(
    year = stringr::str_sub(name, start = -2, end = -1) %>%
      paste0("20", .),
    type = stringr::str_sub(name, 1, 3),
    type = ifelse(type == "SFD", "single_family_units", "multifamily_units")
  ) %>%
  ungroup() %>%
  select(
    ctu_name = CTU_NAME,
    year,
    metric = type,
    value
  ) %>%
  filter(year %in% c(
    2018,
    2040
  ))


# c("COUSUBNS", "COCTU_ID", "TAZ2012", "HH2010", "HH2014", "HH2018",
#   "HH2020", "HH2030", "HH2040", "POPINHH10", "POPINHH14", "POPINHH18",
#   "POPINHH20", "POPINHH30", "POPINHH40", "POP2010", "POP2014",
#   "POP2018", "POP2020", "POP2030", "POP2040", "EMP2010", "EMP2014",
#   "EMP2018", "EMP2020", "EMP2030", "EMP2040", "SFD_Units18", "SFD_Units20",
#   "SFD_Units30", "SFD_Units40", "MF_Units18", "MF_Units20", "MF_Units30",
#   "MF_Units40", "MH_Units", "SFD_Acre18", "SFD_Acre20", "SFD_Acre30",
#   "SFD_Acre40", "MF_Acre18", "MF_Acre20", "MF_Acre30", "MF_Acre40",
#   "MH_Acre", "Res_Acre18", "Res_Acre20", "Res_Acre30", "Res_Acre40",
#   "IND_Acre18", "IND_Acre20", "IND_Acre30", "IND_Acre40", "INS_Acre18",
#   "INS_Acre20", "INS_Acre30", "INS_Acre40", "COM_Acre18", "COM_Acre20",
#   "COM_Acre30", "COM_Acre40", "NonRes_Acre18", "NonRes_Acre20",
#   "NonRes_Acre30", "NonRes_Acre40", "RES_CONSUM_PCT", "NONRES_CONSUM_PCT",
#   "RES ACRES CHANGE")
