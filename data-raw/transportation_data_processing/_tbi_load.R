# load TBI data if it doesn't already exist
if (!fs::file_exists("data-raw/transportation_data_processing/tbi/TravelBehaviorInventory2023Trip.csv")) {
  pkgload::load_all()
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

if (exists("trip") & exists("hh")) {
  message("TBI data already loaded")
} else {
  message("Loading TBI data, this may take a few moments...")

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

  trip_purpose <- bind_rows(
    read.csv("data-raw/transportation_data_processing/tbi/TravelBehaviorInventory2019TripPurpose.csv"),
    read.csv("data-raw/transportation_data_processing/tbi/TravelBehaviorInventory2021TripPurpose.csv"),
    read.csv("data-raw/transportation_data_processing/tbi/TravelBehaviorInventory2023TripPurpose.csv")
  )

  cprg_tbi_hh_counties <- c(
    "Anoka MN",
    "Carver MN",
    "Dakota MN",
    "Hennepin MN",
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
}
