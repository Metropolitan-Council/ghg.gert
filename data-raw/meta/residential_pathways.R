library(tidyverse)

gcam <- read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/ccap-graphics/_meta/data/gcam/mpca_subsector_gcam.RDS")
unique(gcam$subsector_mc)

res_scenarios <- gcam %>%
  filter(subsector_mc == "Residential natural gas",
         scenario %in% c("Net-Zero Pathway",
                         "PPP after Fed RB",
                         "CP after Fed RB")
  )

res_targets <- res_scenarios %>%
  filter(emissions_year %in% c(2030, 2050))

# load needed objects
regional_housing_forecast <- read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/ccap-graphics/_meta/data/regional_housing_forecast.RDS") %>%
  mutate(geog_id = "1")

# regional density_output (seven county, but only importance is no change in density here)
density_output <- run_scenario_land_use()

# bau
bau_results <- run_scenario_building(
  res_tb = regional_housing_forecast,
  res_tb_bau = regional_housing_forecast,
  .baseline_year = 2022,
  .selected_ctu = "CCAP Region",
  .density_output = density_output
)

