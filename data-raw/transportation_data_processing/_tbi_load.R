# load TBI data if it doesn't already exist
# tbi/ is not tracked in git (~4GB unzipped); this block recreates it
tbi_dir <- "data-raw/transportation_data_processing/tbi"

if (!fs::file_exists(file.path(tbi_dir, "tbi_home_interview2023/csv/TravelBehaviorInventory2023LinkedTrip.csv"))) {
  pkgload::load_all()
  dir.create(tbi_dir, recursive = TRUE, showWarnings = FALSE)

  # zips are large; R's default 60s timeout is too short
  old_timeout <- getOption("timeout")
  options(timeout = max(1800, old_timeout))

  # Met Council GIS data server (the old Geospatial Commons links are dead)
  for (yr in c(2019, 2021, 2023)) {
    zip_name <- paste0("tbi_home_interview", yr, ".zip")
    download.file(
      url = paste0("https://gisdata.metc.state.mn.us/gisdata/Society/", zip_name),
      destfile = file.path(tbi_dir, zip_name),
      mode = "wb"
    )
  }
  options(timeout = old_timeout)

  list.files(tbi_dir, pattern = "\\.zip$", full.names = TRUE) %>%
    purrr::map(
      function(x) {
        zip::unzip(x, exdir = tbi_dir)
      }
    )
}

if (exists("trip") & exists("hh")) {
  message("TBI data already loaded")
} else {
  message("Loading TBI data, this may take a few moments...")

  # warning that these are hefty, around 4gb
  trip <- bind_rows(
    read.csv("data-raw/transportation_data_processing/tbi/tbi_home_interview2019/csv/TravelBehaviorInventory2019LinkedTrip.csv"),
    read.csv("data-raw/transportation_data_processing/tbi/tbi_home_interview2021/csv/TravelBehaviorInventory2021LinkedTrip.csv"),
    read.csv("data-raw/transportation_data_processing/tbi/tbi_home_interview2023/csv/TravelBehaviorInventory2023LinkedTrip.csv")
  )

  hh <- bind_rows(
    read.csv("data-raw/transportation_data_processing/tbi/tbi_home_interview2019/csv/TravelBehaviorInventory2019Household.csv"),
    read.csv("data-raw/transportation_data_processing/tbi/tbi_home_interview2021/csv/TravelBehaviorInventory2021Household.csv"),
    read.csv("data-raw/transportation_data_processing/tbi/tbi_home_interview2023/csv/TravelBehaviorInventory2023Household.csv")
  )

  trip_purpose <- bind_rows(
    read.csv("data-raw/transportation_data_processing/tbi/tbi_home_interview2019/csv/TravelBehaviorInventory2019TripPurpose.csv"),
    read.csv("data-raw/transportation_data_processing/tbi/tbi_home_interview2021/csv/TravelBehaviorInventory2021TripPurpose.csv"),
    read.csv("data-raw/transportation_data_processing/tbi/tbi_home_interview2023/csv/TravelBehaviorInventory2023TripPurpose.csv")
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
