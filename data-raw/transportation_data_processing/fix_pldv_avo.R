# update passenger light-duty vehicle AVO to most recent TBI
# create average using Imagine 2050 Community Designation
pkgload::load_all()

# pull modeling dataset, which has imagine designations for each CTU
vmt_model_data <- readRDS(url("https://github.com/Metropolitan-Council/ghg-cprg/raw/refs/heads/main/_transportation/data/vmt_model_data.RDS"))

ctu_imagine <- vmt_model_data %>%
  select(ctu_name, gnis, imagine_designation) %>%
  unique()

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

# get index of CD levels
hh_cd_levels <- hh_region %>%
  select(cd_2050, cd_2050_broad, cd_2050_rsd) %>%
  unique()


avo_imagine <- trip %>%
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
    distance_miles < 720,
    distance_miles > 0
  ) %>%
  left_join(hh_region, join_by(survey_year, hh_id)) %>%
  filter(
    linked_trip_weight > 0,
    !is.na(cd_2050)
  ) %>%
  srvyr::as_survey_design(id = linked_trip_id, weights = linked_trip_weight) %>%
  group_by(cd_2050_broad) %>%
  summarize(
    num_travelers_numeric = round(srvyr::survey_mean(num_hh_travelers_int, na.rm = T), digits = 2),
    n_trips = srvyr::survey_total(),
    n_trips_sample = n()
  ) %>%
  ungroup()

# region level AVO, no CD grouping
avo_region <- trip %>%
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
    distance_miles < 720,
    distance_miles > 0
  ) %>%
  left_join(hh_region, join_by(survey_year, hh_id)) %>%
  filter(
    linked_trip_weight > 0,
    !is.na(cd_2050)
  ) %>%
  srvyr::as_survey_design(id = linked_trip_id, weights = linked_trip_weight) %>%
  # group_by(cd_2050_broad) %>%
  summarize(
    num_travelers_numeric = round(srvyr::survey_mean(num_hh_travelers_int, na.rm = T), digits = 2),
    n_trips = srvyr::survey_total(),
    n_trips_sample = n()
  ) %>%
  ungroup()


# compare new with previous ------
avo_new <- avo_imagine %>%
  left_join(hh_cd_levels, join_by(cd_2050_broad)) %>%
  left_join(ctu_imagine,
    by = c("cd_2050" = "imagine_designation")
  ) %>%
  mutate(
    var = "AVO",
    mode = "PLDV",
    aeo_mode = "LDV",
    type = "P",
    value = num_travelers_numeric
  ) %>%
  select(geog_id = gnis, var, mode, value, aeo_mode, type) %>%
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

rm(trip, hh)
