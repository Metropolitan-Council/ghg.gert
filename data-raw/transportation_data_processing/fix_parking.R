# replace parking values with up to date TBI
pkgload::load_all()
library(stringr)
source("data-raw/transportation_data_processing/_tbi_load.R")

tbi_parking_cost <- trip %>%
  filter(
    hh_id %in% hh$hh_id,
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
    park_type %in% c(
      "Paid via cash, credit card, or ticket(s)",
      "Parking reservation service (e.g., SpotHero, ParkMobile)",
      "Used a parking pass (any type)"
    ),
    distance_miles < 720,
    distance_miles > 0
  ) %>%
  filter(
    linked_trip_weight > 0,
    !is.na(vehicle_park_cost)
  ) %>%
  srvyr::as_survey_design(id = linked_trip_id, weights = linked_trip_weight) %>%
  group_by(trip_d_city) %>%
  summarize(
    vehicle_park_cost = srvyr::survey_mean(vehicle_park_cost, na.rm = T),
    n_trips = srvyr::survey_total(),
    n_trips_sample = n()
  ) %>%
  ungroup() %>%
  filter(n_trips_sample >= 10) %>%
  arrange(-n_trips_sample) %>%
  mutate(geog_name = stringr::str_remove(trip_d_city, "Twp.") %>%
    str_replace("St. ", "Saint ") %>%
    str_remove("(unorg.)") %>%
    str_remove_all("[:punct:]") %>%
    str_trim())


parking_cost <-
  transportation_data$passenger %>%
  filter(
    var == "PARK",
    mode == "PLDV"
  ) %>%
  left_join(
    tbi_parking_cost %>%
      select(geog_name, vehicle_park_cost),
    by = join_by(geog_name)
  ) %>%
  mutate(value = case_when(
    # use TBI if possible
    !is.na(vehicle_park_cost) ~ vehicle_park_cost,
    # otherwise use $0.01
    TRUE ~ 0.01
  )) %>%
  select(names(transportation_data$passenger)) %>%
  bind_rows(
    transportation_data$freight %>%
      filter(var == "PARK") %>%
      mutate(value = case_when(
        # use existing data and
        # change 0.10 to 0.01
        value == 1 ~ value,
        value == 0.1 ~ 0.01,
        TRUE ~ value
      ))
  ) %>%
  select(-year) %>%
  unique()


transportation_data$passenger <- transportation_data$passenger %>%
  filter(!(var == "PARK"))

transportation_data$freight <- transportation_data$freight %>%
  filter(!(var == "PARK"))

usethis::use_data(transportation_data, overwrite = TRUE)

usethis::use_data(parking_cost, overwrite = TRUE)
