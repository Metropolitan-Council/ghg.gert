#### import random forest models from inventory repo to project mcf demand forward
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
    id_cols = c(geog_name, geog_id, geog_level,  geog_id_type, inventory_year),
    names_from = sp_categories,
    values_from = value
  )


urbansim_busi <- demographic_data %>%
  filter(sp_categories %in% c("commercial_jobs",
                              "industrial_jobs",
                              "jobs")) %>%
  pivot_wider(
    id_cols = c(geog_name, geog_id,geog_level,  geog_id_type, inventory_year),
    names_from = sp_categories,
    values_from = value
  )

ccap_ctu <- readr::read_rds("data-raw/meta/ccap_ctu.RDS")
ccap_county <-readr::read_rds("data-raw/meta/ccap_county.RDS")

coctu_res_mcf <- readr::read_rds(
  "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_energy/data-raw/predicted_coctu_residential_mcf.rds")

coctu_busi_mcf <- readr::read_rds(
  "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_energy/data-raw/predicted_coctu_business_mcf.rds")

### residential predictions

res_mcf <- bind_rows(
  coctu_res_mcf %>%
    mutate(geog_id = substr(coctu_id_gnis, 4, 11)) %>%
    group_by(geog_id, ctu_name, ctu_class, inventory_year, data_source) %>%
    summarize(mcf = sum(residential_mcf)) %>%
    rename(geog_name = ctu_name,
           geog_level = ctu_class),
  coctu_res_mcf %>%
    mutate(geog_id = substr(coctu_id_gnis, 1, 3)) %>%
    group_by(geog_id, county_name, inventory_year, data_source) %>%
    summarize(mcf = sum(residential_mcf)) %>%
    rename(geog_name = county_name) %>%
    mutate(geog_level = "county")
)

## electricity_res_coefficients

electricity_res <- left_join(res_mcf,
                             urbansim_res %>%
                               select(-geog_name),
                             by = c("geog_id", "geog_level", "inventory_year")
)


unit_model_res <- lm(
  mcf ~ multifamily_units +
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

busi_mcf <- bind_rows(
  coctu_busi_mcf %>%
    mutate(geog_id = substr(coctu_id_gnis, 4, 11)) %>%
    group_by(geog_id, ctu_name, ctu_class , inventory_year, data_source) %>%
    summarize(mcf = sum(business_mcf)) %>%
    rename(geog_name = ctu_name,
           geog_level = ctu_class),
  coctu_busi_mcf %>%
    mutate(geog_id = substr(coctu_id_gnis, 1, 3)) %>%
    group_by(geog_id, county_name, inventory_year, data_source) %>%
    summarize(mcf = sum(business_mcf)) %>%
    rename(geog_name = county_name) %>%
    mutate(geog_level = "county")
)

## electricity_res_coefficients

electricity_busi <- left_join(busi_mcf,
                              urbansim_busi %>%
                                select(-geog_name),
                              by = c("geog_id", "geog_level", "inventory_year")
)

#performs better
electricity_busi_max_year <- electricity_busi %>%
  group_by(geog_id, geog_name, geog_level) %>%
  mutate(max_year = max(inventory_year)) %>%
  filter(inventory_year == max_year) %>%
  ungroup()


unit_model_busi <- lm(
  mcf ~ commercial_jobs +
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

mcf_coefficients <- bind_rows(res_unit_coefs,
                              busi_unit_coefs) %>%
  rename(var = term, mcf_per_unit = estimate)

usethis::use_data(mcf_coefficients, overwrite = TRUE)
