pkgload::load_all()

# load from CPRG repository
coctu_vmt_forecast <- readRDS(url("https://github.com/Metropolitan-Council/ghg-cprg/raw/refs/heads/integrate-mndot-forecast-vmt/_transportation/data/mndot_vmt_ctu_gap_filled.RDS"))

ctu_vmt_forecast <- coctu_vmt_forecast %>%
  group_by(gnis, inventory_year) %>%
  summarize(final_city_vmt = sum(final_city_vmt),
            final_vmt_source = first(final_vmt_source))


ctu_vmt_source <- ctu_vmt_forecast %>%
  group_by(gnis, final_vmt_source) %>%
  summarize(vmt_years = paste0(min(as.numeric(inventory_year)), "-", max(as.numeric(inventory_year)), collapse = ", ")) %>%
  ungroup() %>%
  arrange(gnis, final_vmt_source)


pmt_exist <- transportation_data$passenger %>%
  filter(mode == "PLDV",
         var == "PMT")

pldv_avo <- transportation_data$passenger %>%
  filter(var == "AVO",
         mode == "PLDV") %>%
  select(
    geog_id, geog_name,
    AVO =  value) %>%
  unique()

pmt_new <- ctu_vmt_forecast %>%
  # only pull out the years that are in the current dataset
  filter(inventory_year %in% transportation_data$passenger$year) %>%
  left_join(pldv_avo, by = c("gnis" = "geog_id")) %>%
  rowwise() %>%
  mutate(value =
           # PMT is VMT multiplied by average vehicle occupancy
           # then annualized with 340
           (final_city_vmt * AVO) * 340,
         mode = "PLDV",
         var = "PMT",
         geog_id = gnis,
         year = as.character(inventory_year)) %>%
  ungroup() %>%
  select(mode, var, geog_name, geog_id, year, value)


pmt_replace <- pmt_new %>%
  left_join(pmt_exist %>% select(-value),
            by = join_by(mode, var, geog_name, geog_id, year))


testthat::expect_equal(
  pmt_replace %>%
    filter(is.na(geog_id)| is.na(geog_name)| is.na(value)| is.na(aeo_mode)| is.na(type)) %>%
    nrow(),
  0
)


# replace in our transportation_data
transportation_data$passenger  <- transportation_data$passenger %>%
  filter(!(var == "PMT" & mode == "PLDV")) %>%
  bind_rows(pmt_replace)


usethis::use_data(transportation_data, overwrite = TRUE)


# update documentation
ctu_vmt_year_source <- ctu_vmt_source %>%
  mutate(geog_id = gnis) %>%
  full_join(pmt_replace %>% select(geog_id, geog_name) %>% unique())


transportation_index$ctu_vmt_year_source <- ctu_vmt_year_source

usethis::use_data(transportation_index, overwrite = TRUE)


