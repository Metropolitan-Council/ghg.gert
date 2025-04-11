#### import random forest models from inventory repo to project mwh demand forward

# load in predictor data
urbansim <- readr::read_rds("data-raw/meta/urbansim_allyrs.RDS")

ccap_ctu <- readr::read_rds("data-raw/meta/ccap_ctu.RDS")
ccap_county <-readr::read_rds("data-raw/meta/ccap_county.RDS")

# mn_parcel <-  readr::read_rds(
#   "https://github.com/Metropolitan-Council/ghg-cprg/raw/182-develop-city-utility-demand-model/_meta/data/ctu_parcel_data_2021.RDS"
# )

# current weather
noaa <- readr::read_rds(
  "https://github.com/Metropolitan-Council/ghg-cprg/raw/182-develop-city-utility-demand-model/_meta/data/noaa_weather_monthly.rds"
) %>%
  group_by(inventory_year) %>%
  summarize(
    heating_degree_days = sum(heating_degree_days),
    cooling_degree_days = sum(cooling_degree_days),
    temperature = mean(dry_bulb_temp)
  )

# use average cooling_degree_days from recent years until future model emerges
cdd <- noaa %>%
  filter(inventory_year >= 2018) %>%
  pull(cooling_degree_days) %>%
  mean

#future weather
# mnclim <- read_csv("data-raw/climate/MnClimat.csv") %>%
#   janitor::clean_names()
# mnclim_scaling <- mnclim %>%
#   filter(time_frame == "yearly")


busi_rf <- readr::read_rds(
  "https://github.com/Metropolitan-Council/ghg-cprg/raw/182-develop-city-utility-demand-model/_energy/data/ctu_business_elec_random_forest.RDS"
)

res_rf <- readr::read_rds(
  "https://github.com/Metropolitan-Council/ghg-cprg/raw/182-develop-city-utility-demand-model/_energy/data/ctu_residential_elec_random_forest.RDS"
)


## create residential dataset for prediction:

residential <- c(
  "total_pop",
  "total_households",
  "total_residential_units",
  "manufactured_homes",
  "single_fam_det_sl_own",
  "single_fam_det_ll_own",
  "single_fam_det_rent",
  "single_fam_attached_own",
  "single_fam_attached_rent",
  "multi_fam_own",
  "multi_fam_rent"
)


### create 2010-2025 urbansim residential dataset
urbansim_res <- urbansim %>%
  filter(variable %in% residential) %>%
  pivot_wider(
    id_cols = c(coctu_id, inventory_year),
    names_from = variable,
    values_from = value
  ) %>%
  filter(!is.na(coctu_id)) %>%
  mutate(
    ctu_id = str_sub(coctu_id, -7, -1),
    county_id = as.numeric(str_remove(coctu_id, paste0("0", ctu_id))),
    ctu_id = as.numeric(ctu_id)
  ) %>%
  left_join(
    ccap_ctu %>% st_drop_geometry() %>%
      distinct(ctu_name, ctu_id, thrive_designation),
    by = c("ctu_id")
  ) %>%
  left_join(
    ccap_county %>% st_drop_geometry() %>%
      mutate(county_id = as.numeric(str_sub(county_id, -3, -1))) %>%
      select(county_name, county_id),
    by = c("county_id")
  ) %>%
  mutate(cooling_degree_days = cdd)


coctu_res_predict <- urbansim_res %>%
  mutate(mwh_predicted = predict(res_rf, newdata = .))
