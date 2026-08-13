# update passenger light-duty vehicle AVO to most recent TBI
# create average using Imagine 2050 Community Designation
pkgload::load_all()
source("data-raw/transportation_data_processing/_tbi_load.R")

# pull modeling dataset, which has imagine designations for each CTU
vmt_model_data <- readRDS(url("https://github.com/Metropolitan-Council/ghg-cprg/raw/refs/heads/main/_transportation/data/vmt_model_data.RDS"))

ctu_imagine <- vmt_model_data %>%
  select(ctu_name, gnis, imagine_designation) %>%
  unique()

# get index of CD levels
hh_cd_levels <- hh_region %>%
  select(cd_2050, cd_2050_broad, cd_2050_rsd) %>%
  unique()


avo_imagine <- trip %>%
  filter(
    hh_id %in% hh_region$hh_id,
    mode_type %in% c(
      "Household Vehicle",
      "Other Vehicle",
      "For-Hire Vehicle"
    ),
    # origin and destination in MPO area
    trip_o_in_mpo == TRUE,
    trip_d_in_mpo == TRUE,
    # ensure observed trip duration,
    # reasonable distance
    # origin or destination in our counties
    duration_seconds > 0,
    as.character(trip_o_county) %in% cprg_tbi_hh_counties,
    as.character(trip_d_county) %in% cprg_tbi_hh_counties,
    distance_miles < 720,
    distance_miles > 0
  ) %>%
  left_join(hh_region, join_by(survey_year, hh_id)) %>%
  filter(
    linked_trip_weight > 0,
    !is.na(cd_2050)
  ) %>%
  srvyr::as_survey_design(id = linked_trip_id, weights = linked_trip_weight) %>%
  group_by(cd_2050) %>%
  summarize(
    num_travelers_numeric = round(srvyr::survey_mean(num_hh_travelers_int, na.rm = T), digits = 2),
    n_trips = srvyr::survey_total(),
    n_trips_sample = n(), .groups = "keep"
  ) %>%
  ungroup()

# region level AVO, no CD grouping
avo_region <- trip %>%
  filter(
    hh_id %in% hh_region$hh_id,
    mode_type %in% c(
      "Household Vehicle",
      "Other Vehicle",
      "For-Hire Vehicle"
    ),
    # origin and destination in MPO area
    trip_o_in_mpo == TRUE,
    trip_d_in_mpo == TRUE,
    # ensure observed trip duration,
    # reasonable distance
    # origin or destination in our counties
    duration_seconds > 0,
    as.character(trip_o_county) %in% cprg_tbi_hh_counties,
    as.character(trip_d_county) %in% cprg_tbi_hh_counties,
    distance_miles < 720,
    distance_miles > 0
  ) %>%
  left_join(hh_region, join_by(survey_year, hh_id)) %>%
  filter(
    linked_trip_weight > 0,
    !is.na(cd_2050)
  ) %>%
  srvyr::as_survey_design(id = linked_trip_id, weights = linked_trip_weight) %>%
  summarize(
    num_travelers_numeric = round(srvyr::survey_mean(num_hh_travelers_int, na.rm = T), digits = 2),
    n_trips = srvyr::survey_total(),
    n_trips_sample = n(), .groups = "keep"
  ) %>%
  ungroup()


# compare new with previous ------
avo_new <- avo_imagine %>%
  left_join(hh_cd_levels, join_by(cd_2050)) %>%
  left_join(geog_index,
    by = c("cd_2050" = "imagine_designation")
  ) %>%
  mutate(
    var = "AVO",
    mode = "PLDV",
    aeo_mode = "LDV",
    type = "P",
    value = num_travelers_numeric
  ) %>%
  select(geog_id, var, mode, value, aeo_mode, type) %>%
  unique()

avo_exist <- transportation_data$passenger %>%
  filter(
    mode == "PLDV",
    var == "AVO"
  ) %>%
  unique()

# average AVO has increased
# avo_exist$value %>% mean()
# avo_new$value %>% mean()

# replace AVO -----

avo_replace <- avo_exist %>%
  select(-value) %>%
  left_join(avo_new)

# make sure no NA values
testthat::expect_equal(
  avo_replace %>%
    filter(is.na(geog_id) | is.na(geog_name) | is.na(value) | is.na(aeo_mode) | is.na(type)) %>%
    nrow(),
  0
)


# replace in our transportation_data object
transportation_data$passenger <- transportation_data$passenger %>%
  filter(!(var == "AVO" & mode == "PLDV")) %>%
  bind_rows(avo_replace)


usethis::use_data(transportation_data, overwrite = TRUE)
