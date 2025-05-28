library(ghg.ccap)
library(tidyverse)

.ctu <- "all"

current <- run_scenario_land_use(.selected_ctu = .ctu, .conservation_tillage_intervention = "current_conservation_tillage")
double <- run_scenario_land_use(.selected_ctu = .ctu, .conservation_tillage_intervention = "double_conservation_tillage")
all <- run_scenario_land_use(.selected_ctu = .ctu, .conservation_tillage_intervention = "maximum_conservation_tillage")


together <- bind_rows(current, double) %>%
  bind_rows(all) %>%
  filter(str_detect(var, "stock")) %>%
  select(geog_name, year, var, conservation_tillage_intervention, value) %>%
  pivot_wider(names_from = conservation_tillage_intervention, values_from = value)

together %>%
  filter(maximum_conservation_tillage > current_conservation_tillage)
