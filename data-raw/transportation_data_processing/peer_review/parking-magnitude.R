pkgload::load_all()
library(tidyverse)

new_passenger <- transportation_data$passenger %>%
  filter(ctu %in% c(
    "St. Paul",
    "Minneapolis",
    "Plymouth",
    "Lake Elmo"
  ) | ctu == "All")

new_freight <- transportation_data$freight %>%
  filter(ctu %in% c(
    "St. Paul",
    "Minneapolis",
    "Plymouth",
    "Lake Elmo"
  ) | ctu == "All")


parking5 <- run_module_transportation(
  .parking_price = 5, .scenario = "p5",
  pass_tb = new_passenger,
  freight_tb = new_freight
)

parking10 <- run_module_transportation(
  .parking_price = 10, .scenario = "p10",
  pass_tb = new_passenger,
  freight_tb = new_freight
)

parking15 <- run_module_transportation(
  .parking_price = 15, .scenario = "p15",
  pass_tb = new_passenger,
  freight_tb = new_freight
)


parking <- parking5$passenger_all %>%
  bind_rows(
    # parking5$freight_all,
    parking10$passenger_all,
    # parking10$freight_all,
    parking15$passenger_all
    # parking15$freight_all
  ) %>%
  filter(year == "2040") %>%
  group_by(ctu, scenario, year) %>% # mode, sector
  summarise(emissions = sum(dir_ghg, na.rm = T)) %>%
  arrange(scenario)
# pivot_wider(names_from = scenario, values_from = emissions)

parking %>% View()


transportation_data$passenger %>%
  filter(var == "PARK")
