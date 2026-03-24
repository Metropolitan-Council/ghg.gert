pkgload::load_all()

# County VMT ----


# County AVO -----
source("data-raw/transportation_data_processing/_tbi_load.R")


# avo by county
avo_county <- trip %>%
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
    duration_minutes > 0,
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
  group_by(hh_county) %>%
  summarize(
    num_travelers_numeric = round(srvyr::survey_mean(num_hh_travelers_int, na.rm = T), digits = 2),
    n_trips = srvyr::survey_total(),
    n_trips_sample = n()
  ) %>%
  ungroup() %>%
  mutate(geog_name = stringr::str_replace(hh_county, " MN", " County")) %>%
  left_join(geog_index) %>%
  mutate(
    var = "AVO",
    mode = "PLDV",
    aeo_mode = "LDV",
    type = "P",
    value = num_travelers_numeric
  ) %>%
  select(geog_id, geog_name, var, mode, value, aeo_mode, type)


co_non_pldv <- vehicle_occupancy %>%
  filter(!mode %in% c("PLDV", "BU")) %>%
  select(-geog_id, -geog_name) %>%
  unique() %>%
  cross_join(
    avo_county %>%
      select(geog_id, geog_name) %>%
      unique()
  )

co_bu_avo <- vehicle_occupancy %>%
  filter(
    mode == "BU"
  ) %>%
  select(mode, var, value, aeo_mode, type) %>%
  unique() %>%
  filter(value == min(value)) %>%
  cross_join(
    avo_county %>%
      select(geog_id, geog_name) %>%
      unique()
  )

vehicle_occupancy_full <- bind_rows(
  vehicle_occupancy,
  avo_county, co_non_pldv, co_bu_avo
)
# County Vehicle counts ----
