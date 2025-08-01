rm(list=ls())
library(ghg.ccap)
library(tidyverse)

.ctu <- "all"
.ctu <- "Benton Twp."

test <- run_module_waste(
  .selected_ctu = .ctu,
  .diverted_to_organics_pct = 28,
  .diverted_to_organics_start = 2025,
  .diverted_to_organics_end = 2042,
)






current <- run_module_waste(waste_tb = waste_data$ctu$projections,
                            waste_char = waste_data$characterization,
                            .selected_ctu = .ctu,
                            .methane_recovery_pct = 0.5,
                            .methane_recovery_start = 2030,
                            .methane_recovery_end = 2040,
                            .anaerobic_digestion_pct = 0)
current %>% filter(source %in% c("Waste to energy"))
current %>% arrange(inventory_year, source)
waste_data$ctu$projections %>% filter(source %in% c("Waste to energy"))


waste_data$ctu$baseline %>%
  filter(inventory_year == 2022) %>%
  group_by(geog_id) %>%
  mutate(total_activity = sum(value_activity),
         pct_of_total = value_activity / total_activity) %>%
  ggplot() +
  geom_histogram(aes(x=pct_of_total)) +
  facet_wrap(~source, scales = "free_y")


# current <- run_scenario_land_use(.selected_ctu = .ctu, .conservation_tillage_intervention = "current_conservation_tillage")
# double <- run_scenario_land_use(.selected_ctu = .ctu, .conservation_tillage_intervention = "double_conservation_tillage")
# all <- run_scenario_land_use(.selected_ctu = .ctu, .conservation_tillage_intervention = "maximum_conservation_tillage")
#
#
# together <- bind_rows(current, double) %>%
#   bind_rows(all) %>%
#   filter(str_detect(var, "stock")) %>%
#   select(geog_name, year, var, conservation_tillage_intervention, value) %>%
#   pivot_wider(names_from = conservation_tillage_intervention, values_from = value)
#
# together %>%
#   filter(maximum_conservation_tillage > current_conservation_tillage)
