library(ghg.sp)
library(tidyverse)

parking5 <- run_scenario_transportation(.parking_price = 5, .scenario = "p5",
                                        .selected_ctu = "Plymouth")
parking10 <- run_scenario_transportation(.parking_price = 10, .scenario = "p10",
                                         .selected_ctu = "Plymouth")

parking <- parking5$passenger_all %>%
  bind_rows(parking5$freight_all) %>%
  bind_rows(parking10$passenger_all) %>%
  bind_rows(parking10$freight_all) %>%
  filter(year == "2040") %>%
  group_by(ctu, scenario, year) %>% # mode, sector
  summarise(emissions = sum(dir_ghg, na.rm = T)) %>%
  pivot_wider(names_from = scenario, values_from = emissions)

parking %>%
  mutate(pct_diff = (p5 - p10)/p5) %>% View


transportation_data$passenger %>%
  filter(var == "PARK") %>% View
