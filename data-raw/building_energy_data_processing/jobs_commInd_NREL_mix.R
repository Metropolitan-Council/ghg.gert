
load("data/demographic_data.rda")

jobs <- demographic_data %>%
  filter(sp_categories %in% c("commercial_jobs", "industrial_jobs")) %>%
  group_by(geog_name, geog_id, sp_categories, geog_level) %>%
  mutate(
    baseline_adjustment = value_change_from_base[inventory_year == 2022][1],
    value_change_from_2022 = value_change_from_base - baseline_adjustment
  ) %>%
  ungroup() %>%
  select(-baseline_adjustment)

# here-specific path of this CPRG file: "_energy/data-raw/nrel_slope/nrel_emissions_inv_city.RDS"
nrel_slope_cprg_city_activityAndEmissions <- readRDS("C:/Users/LimeriSA/Documents/Projects/ghg-cprg/_energy/data-raw/nrel_slope/nrel_slope_cprg_city_activityAndEmissions.RDS") %>%
  select(-geometry)


