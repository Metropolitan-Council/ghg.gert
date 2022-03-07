library(ghg.sp)
library(dplyr)
library(tidyr)

ctu_county <- councilR::import_from_gis(query = "CountiesAndCTUs") %>%
  select(COCTU_ID, CTU_NAME)

housing_stock_forecast <- readxl::read_xlsx("data-raw/building_energy_data_processing/building-data/FORECAST_LU_TAZCTU.xlsx") %>%
  group_by(COCTU_ID) %>%
  summarize(across(4:67, sum)) %>%
  select(COCTU_ID, 25:32) %>%
  left_join(ctu_county %>%
              sf::st_drop_geometry()) %>%
  group_by(COCTU_ID, CTU_NAME) %>%
  pivot_longer(cols = 2:9) %>%
  mutate(year = stringr::str_sub(name, start = -2, end= -1) %>%
           paste0("20", .),
         type =  stringr::str_sub(name, 1, 3),
         type =   ifelse(type == "SFD", "single_family_units", "multifamily_units")) %>%
  ungroup() %>%
  select(ctu_name = CTU_NAME,
         year,
         metric = type,
         value)
