rm(list=ls())
library(ghg.ccap)
library(tidyverse)

.ctu <- "all"
.ctu <- "Eagan"







# Make a base tibble
test_regional_inv <- expand_grid(
  inventory_year = 2005:2022,
  source = c("Landfill", "MSW_Compost", "Onsite", "Organics",
             "Recycling", "Waste to energy", "Wastewater")
) %>%
  mutate(
    geog_id = "00000000",
    geog_name = "Regional",
    geog_level = "REGION",
    # fake population: starts at 100,000 in 2005, grows by ~1% per year
    geog_pop = round(100000 * (1.01 ^ (inventory_year - 2005))),
    # assign some dummy values (different rules depending on source)
    value_activity = case_when(
      source == "Landfill" ~ round(runif(n(), 5000, 7000)),
      source == "MSW_Compost" ~ round(runif(n(), 100, 300)),
      source == "Onsite" ~ round(runif(n(), 50, 150)),
      source == "Organics" ~ round(runif(n(), 1000, 3000)),
      source == "Recycling" ~ round(runif(n(), 4000, 6000)),
      source == "Waste to energy" ~ round(runif(n(), 2000, 4000)),
      source == "Wastewater" ~ NA_real_
    ),
    units_activity = case_when(
      source == "Wastewater" ~ "use population as scalar",
      TRUE ~ "metric tons MSW"
    ),
    data_type = case_when(
      source == "Wastewater" ~ "use population as scalar",
      TRUE ~ "dummy placeholder data"
    )
  )



test_regional_proj <- expand_grid(
  inventory_year = 2023:2050,
  source = c("Landfill", "MSW_Compost", "Onsite", "Organics",
             "Recycling", "Waste to energy", "Wastewater")
) %>%
  mutate(
    geog_id = "00000000",
    geog_name = "Regional",
    geog_level = "REGION",
    # fake population: starts at 100,000 in 2005, grows by ~1% per year
    geog_pop = round(max(test_regional_inv$geog_pop) * (1.01 ^ (inventory_year - 2023))),
    # assign some dummy values (different rules depending on source)
    value_activity = case_when(
      source == "Landfill" ~ round(runif(n(), 5000, 7000)),
      source == "MSW_Compost" ~ round(runif(n(), 100, 300)),
      source == "Onsite" ~ round(runif(n(), 50, 150)),
      source == "Organics" ~ round(runif(n(), 1000, 3000)),
      source == "Recycling" ~ round(runif(n(), 4000, 6000)),
      source == "Waste to energy" ~ round(runif(n(), 2000, 4000)),
      source == "Wastewater" ~ NA_real_
    ),
    units_activity = case_when(
      source == "Wastewater" ~ "use population as scalar",
      TRUE ~ "metric tons MSW"
    ),
    data_type = case_when(
      source == "Wastewater" ~ "use population as scalar",
      TRUE ~ "dummy placeholder data"
    )
  )


test_solid_waste_baseline <-
  ghg.ccap::waste_data$inventory %>%
  filter(inventory_year == 2022 & source !="Wastewater") %>%
  filter(geog_name == "Anoka") %>%
  mutate(geog_name = "Regional",
         geog_id = "00000000",
         geog_level = "REGION") %>%
  dplyr::select(-data_type, -inventory_year) %>%
  group_by(geog_id) %>%
  mutate(total_activity = sum(value_activity),
         pct_of_total = value_activity / total_activity) %>%
  ungroup() %>%
  left_join(
    tibble(
      source = c("Recycling", "Organics", "Waste to energy", "Landfill", "Onsite", "MSW_Compost"),
      # Add MPCA target percentages for 2030
      # From MPCA Metropolitan Solid Waste Management Policy Plan 2022-2042
      # Table 2: MMSW management system objectives in percentages (2021-2042)
      target2030 = c(0.474, 0.276, 0.2, 0.05, 0, 0)
    )
  ) %>%
  mutate(
    changeFromTarget = pct_of_total - target2030,
    action = case_when(
      changeFromTarget < 0 ~ paste0("Increase ",source),
      changeFromTarget > 0 ~ paste0("Decrease ",source),
      TRUE ~ paste0("Keep ",source)
    )
  )



test <- run_module_waste(
  .selected_ctu = "Regional",
  tb_inv = test_regional_inv,
  tb_future = test_regional_proj,
  tb_base = test_solid_waste_baseline,
  tb_char = ghg.ccap::waste_data$characterization,
  tb_target = ghg.ccap::waste_data$mpca,
  .waste_reduction_pct = 0.1,
  .waste_reduction_start = 2025,
  .waste_reduction_end = 2030,
  .source_diversion_start = 2030,
  .source_diversion_end = 2040,
  .diverted_to_landfill_pct = 0.1,
  .diverted_to_recycle_pct = 0.4,
  .diverted_to_organics_pct = 0.3,
  .diverted_to_wte_pct = 0.2,
  .diverted_to_onsite_pct = 0
  )


test


rbind(test$emissions$inv,
      test$emissions$future) %>%
  rename(emissions_year = inventory_year) %>%
  group_by(emissions_year) %>%
  summarize(value_emissions = sum(value_emissions)) %>%
  ggplot() +
  # Base fill (2005-2022, gray)
  geom_ribbon(aes(x = emissions_year, ymin = 0, ymax = value_emissions),
              fill = "gray80", alpha = 0.7)








## Next we need to build out the actual data for the 11-county region










