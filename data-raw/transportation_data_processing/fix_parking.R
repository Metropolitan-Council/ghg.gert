# replace parking values with up to date TBI
pkgload::load_all()
library(stringr)


# load TBI data if it doesn't already exist
if (!fs::file_exists("data-raw/transportation_data_processing/tbi/TravelBehaviorInventory2023Trip.csv")) {
  download.file(
    url = "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_tbi_home_interview2019/csv_society_tbi_home_interview2019.zip",
    destfile = "data-raw/transportation_data_processing/tbi/csv_society_tbi_home_interview2019.zip"
  )

  download.file(
    url = "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_tbi_home_interview2021/csv_society_tbi_home_interview2021.zip",
    destfile = "data-raw/transportation_data_processing/tbi/csv_society_tbi_home_interview2021.zip"
  )

  download.file(
    url = "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_tbi_home_interview2023/csv_society_tbi_home_interview2023.zip",
    destfile = "data-raw/transportation_data_processing/tbi/csv_society_tbi_home_interview2023.zip"
  )

  list.files("data-raw/transportation_data_processing/tbi/", full.names = TRUE) %>%
    purrr::map(
      function(x) {
        zip::unzip(x, exdir = "data-raw/transportation_data_processing/tbi/")
      }
    )
}

# warning that these are hefty, around 4gb
trip <- bind_rows(
  read.csv("data-raw/transportation_data_processing/tbi/TravelBehaviorInventory2019LinkedTrip.csv"),
  read.csv("data-raw/transportation_data_processing/tbi/TravelBehaviorInventory2021LinkedTrip.csv"),
  read.csv("data-raw/transportation_data_processing/tbi/TravelBehaviorInventory2023LinkedTrip.csv")
)

hh <- bind_rows(
  read.csv("data-raw/transportation_data_processing/tbi/TravelBehaviorInventory2023Household.csv"),
  read.csv("data-raw/transportation_data_processing/tbi/TravelBehaviorInventory2021Household.csv"),
  read.csv("data-raw/transportation_data_processing/tbi/TravelBehaviorInventory2019Household.csv")
)


cprg_tbi_hh_counties <- c(
  "Anoka MN", "Carver MN",
  "Dakota MN", "Hennepin MN",
  "Ramsey MN",
  "Scott MN",
  "Washington MN"
)

# filter to only HH in our region
# get community designation info
hh_region <- hh %>%
  filter(
    hh_county %in% cprg_tbi_hh_counties,
    hh_in_mpo == TRUE
  ) %>%
  select(
    hh_id, hh_county, survey_year,
    starts_with("cd_20"),
    hh_city
  ) %>%
  unique()


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
    park_type %in% c("Paid via cash, credit card, or ticket(s)",
                     "Parking reservation service (e.g., SpotHero, ParkMobile)",
                     "Used a parking pass (any type)"),
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
  filter(var == "PARK",
         mode == "PLDV") %>%
  left_join(tbi_parking_cost %>%
              select(geog_name, vehicle_park_cost),
            by = join_by(geog_name)) %>%
  mutate(value = case_when(
    # use TBI if possible
    !is.na(vehicle_park_cost) ~ vehicle_park_cost,
    # otherwise use $0.01
    TRUE ~ 0.01
  )) %>%
  select(names(transportation_data$passenger)) %>%
  bind_rows(
    transportation_data$freight %>%
      filter(var == "PARK")
  ) %>% bind_rows(
    transportation_data$freight %>% filter(var == "PARK") %>%
      mutate(value = case_when(
        # use TBI if possible
        value == 1 ~ value,
        value == ~ 0.1 ~ 0.01,
        TRUE ~ value
      )))


transportation_data$passenger <- transportation_data$passenger %>%
  filter(!(var == "PARK"))

transportation_data$freight <- transportation_data$freight %>%
  filter(!(var == "PARK"))

usethis::use_data(transportation_data, overwrite = TRUE)

usethis::use_data(parking_cost, overwrite = TRUE)


rm(trip, hh, parking_cost, tbi_parking_cost)
