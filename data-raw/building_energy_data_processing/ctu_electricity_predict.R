#### import random forest models from inventory repo to project mwh demand forward
load('data/demographic_data.rda')
# load in predictor data
urbansim_res <- demographic_data %>%
  filter(sp_categories %in% c( "multifamily_units",
                               "population",
                               "single_family_attached",
                               "single_family_large_lot",
                               "single_family_small_lot",
                               "total_households")) %>%
  pivot_wider(
    id_cols = c(geog_name, geog_id,ctu_class,  geog_id_type, inventory_year),
    names_from = sp_categories,
    values_from = value
  )


urbansim_busi <- demographic_data %>%
  filter(sp_categories %in% c("commercial_jobs",
                              "industrial_jobs",
                              "jobs")) %>%
  pivot_wider(
    id_cols = c(geog_name, geog_id,ctu_class,  geog_id_type, inventory_year),
    names_from = sp_categories,
    values_from = value
  )

ccap_ctu <- readr::read_rds("data-raw/meta/ccap_ctu.RDS")
ccap_county <-readr::read_rds("data-raw/meta/ccap_county.RDS")

coctu_res_mwh <- readr::read_rds(
  "https://github.com/Metropolitan-Council/ghg-cprg/raw/refine-ctu-electricity-prediction/_energy/data-raw/predicted_coctu_residential_mwh.rds")

coctu_busi_mwh <- readr::read_rds(
  "https://github.com/Metropolitan-Council/ghg-cprg/raw/refine-ctu-electricity-prediction/_energy/data-raw/predicted_coctu_business_mwh.rds")

### residential predictions

res_mwh <- bind_rows(
  coctu_res_mwh %>%
    mutate(geog_id = substr(coctu_id_gnis, 4, 11)) %>%
    group_by(geog_id, ctu_name, ctu_class, inventory_year, data_source) %>%
    summarize(mwh = sum(residential_mwh)) %>%
    mutate(geog_level = "ctu") %>%
    rename(geog_name = ctu_name),
  coctu_res_mwh %>%
    mutate(geog_id = substr(coctu_id_gnis, 1, 3)) %>%
    group_by(geog_id, county_name, inventory_year, data_source) %>%
    summarize(mwh = sum(residential_mwh)) %>%
    rename(geog_name = county_name) %>%
    mutate(geog_level = "county")
)

## electricity_res_coefficients

electricity_res <- left_join(res_mwh,
                             urbansim_res %>%
                               select(-geog_name),
                             by = c("geog_id", "ctu_class", "inventory_year")
)


unit_model_res <- lm(
  mwh ~ multifamily_units +
    single_family_large_lot +
    single_family_small_lot +
    single_family_attached,
  data = electricity_res %>%
    filter(geog_level != "county")
)

summary(unit_model_res)

# extract coefficients
res_unit_coefs <- data.frame(term = names(unit_model_res$coefficients),
                         estimate = unit_model_res$coefficients) %>%
  select(term, estimate) %>%
  filter(term != "(Intercept)")


### business predictions

busi_mwh <- bind_rows(
  coctu_busi_mwh %>%
    mutate(geog_id = substr(coctu_id_gnis, 4, 11)) %>%
    group_by(geog_id, ctu_name, ctu_class, inventory_year, data_source) %>%
    summarize(mwh = sum(business_mwh)) %>%
    mutate(geog_level = "ctu") %>%
    rename(geog_name = ctu_name),
  coctu_busi_mwh %>%
    mutate(geog_id = substr(coctu_id_gnis, 1, 3)) %>%
    group_by(geog_id, county_name, inventory_year, data_source) %>%
    summarize(mwh = sum(business_mwh)) %>%
    rename(geog_name = county_name) %>%
    mutate(geog_level = "county")
)

## electricity_res_coefficients

electricity_busi <- left_join(busi_mwh,
                             urbansim_busi %>%
                               select(-geog_name),
                             by = c("geog_id", "ctu_class", "inventory_year")
)

#performs better
electricity_busi_max_year <- electricity_busi %>%
  group_by(geog_id, geog_name, ctu_class) %>%
  mutate(max_year = max(inventory_year)) %>%
  filter(inventory_year == max_year) %>%
  ungroup()


unit_model_busi <- lm(
  mwh ~ commercial_jobs +
    industrial_jobs,
  data = electricity_busi_max_year %>%
    filter(geog_level != "county")
)

summary(unit_model_busi)

# extract coefficients
busi_unit_coefs <- data.frame(term = names(unit_model_busi$coefficients),
                             estimate = unit_model_busi$coefficients) %>%
  select(term, estimate) %>%
  filter(term != "(Intercept)")
