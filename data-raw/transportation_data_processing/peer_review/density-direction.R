library(ghg.ccap)
library(tidyverse)

popdens_decrease <- run_scenario_transportation(
  .selected_ctu = "Minneapolis",
  .pop_dens_pct_change = -0.2,
  .scenario = "pop_decrease"
)

popdens_increase <- run_scenario_transportation(
  .selected_ctu = "Minneapolis",
  .pop_dens_pct_change = 0.2,
  .scenario = "pop_increase"
)

popdens_bau <- run_scenario_transportation(
  .selected_ctu = "Minneapolis",
  .pop_dens_pct_change = 0,
  .scenario = "pop_bau"
)

empdens_decrease <- run_scenario_transportation(
  .selected_ctu = "Minneapolis",
  .emp_dens_pct_change = -0.2,
  .scenario = "emp_decrease"
)

empdens_increase <- run_scenario_transportation(
  .selected_ctu = "Minneapolis",
  .emp_dens_pct_change = 0.2,
  .scenario = "emp_increase"
)

empdens_bau <- run_scenario_transportation(
  .selected_ctu = "Minneapolis",
  .emp_dens_pct_change = -0,
  .scenario = "emp_bau"
)

popdens_decrease$passenger_all %>%
  bind_rows(popdens_decrease$freight_all) %>%
  bind_rows(popdens_bau$passenger_all) %>%
  bind_rows(popdens_bau$freight_all) %>%
  bind_rows(popdens_increase$passenger_all) %>%
  bind_rows(popdens_increase$freight_all) %>%
  bind_rows(empdens_decrease$passenger_all) %>%
  bind_rows(empdens_decrease$freight_all) %>%
  bind_rows(empdens_bau$passenger_all) %>%
  bind_rows(empdens_bau$freight_all) %>%
  bind_rows(empdens_increase$passenger_all) %>%
  bind_rows(empdens_increase$freight_all) %>%
  filter(year == "2040") %>%
  group_by(ctu, scenario, year) %>% # mode, sector
  summarise(emissions = sum(dir_ghg, na.rm = T)) %>%
  separate(scenario, into = c("density type", "change"), sep = "_") %>%
  pivot_wider(names_from = change, values_from = emissions) %>%
  mutate(flag = if_else(decrease < bau, "reducing density reduces emissions", NA_character_)) %>%
  data.frame()
