### import midwest EIA RECS data

eia_recs <- readxl::read_xlsx(
  "data-raw/building_energy_data_processing/eia_midwest_res_fuel.xlsx",
  sheet = "physical units",
  skip = 3
) %>%
  janitor::clean_names()

eia_recs$x1

eia_housing_type <- eia_recs[17:21,] %>%
  mutate(mwh = as.numeric(electricity_k_wh)/1000,
         mcf = as.numeric(natural_gas_ccf)/10) %>%
  select(housing_type = x1,
         mwh,
         mcf)

eia_housing_age <- eia_recs[32:40,] %>%
  mutate(mwh = as.numeric(electricity_k_wh)/1000,
         mcf = as.numeric(natural_gas_ccf)/10) %>%
  select(year_built = x1,
         mwh,
         mcf)

eia_housing_sqft <- eia_recs[42:47,] %>%
  mutate(mwh = as.numeric(electricity_k_wh)/1000,
         mcf = as.numeric(natural_gas_ccf)/10) %>%
  select(sqft = x1,
         mwh,
         mcf)

eia_recs_energy_usage <- list(
  eia_housing_type = eia_housing_type,
  eia_housing_age = eia_housing_age,
  eia_housing_sqft = eia_housing_sqft
)

usethis::use_data(eia_recs_energy_usage, overwrite = TRUE)
