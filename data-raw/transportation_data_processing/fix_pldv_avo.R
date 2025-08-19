# update passenger light-duty vehicle AVO to most recent TBI
pkgload::load_all()

if(!fs::file_exists("data-raw/transportation_data_processing/tbi/TravelBehaviorInventory2023Trip.csv")){

  download.file(url = "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_tbi_home_interview2019/csv_society_tbi_home_interview2019.zip",
                destfile = "data-raw/transportation_data_processing/tbi/csv_society_tbi_home_interview2019.zip")

  download.file(url = "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_tbi_home_interview2021/csv_society_tbi_home_interview2021.zip",
                destfile = "data-raw/transportation_data_processing/tbi/csv_society_tbi_home_interview2021.zip")

  download.file(url = "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_tbi_home_interview2023/csv_society_tbi_home_interview2023.zip",
                destfile = "data-raw/transportation_data_processing/tbi/csv_society_tbi_home_interview2023.zip")

  list.files("data-raw/transportation_data_processing/tbi/", full.names = TRUE) %>%
    purrr::map(
      function(x){
        zip::unzip(x, exdir = "data-raw/transportation_data_processing/tbi/")
      }
    )
}


trip <- bind_rows(read.csv("data-raw/transportation_data_processing/tbi/TravelBehaviorInventory2023Trip.csv"),
                  read.csv("data-raw/transportation_data_processing/tbi/TravelBehaviorInventory2021Trip.csv"),
                  read.csv("data-raw/transportation_data_processing/tbi/TravelBehaviorInventory2019Trip.csv"))

hh <- bind_rows(read.csv("data-raw/transportation_data_processing/tbi/TravelBehaviorInventory2023Household.csv"),
                read.csv("data-raw/transportation_data_processing/tbi/TravelBehaviorInventory2021Household.csv"),
                read.csv("data-raw/transportation_data_processing/tbi/TravelBehaviorInventory2019Household.csv"))

cprg_tbi_hh_counties <- c(
  "Anoka MN", "Carver MN",
  "Dakota MN", "Hennepin MN",
  "Ramsey MN",
  "Scott MN",
  "Washington MN"
)

thrive_broad <- thrive %>%
  ungroup() %>%
  select(com_des, urban_sub_rural, urban_rural) %>%
  unique() %>%
  filter(as.character(com_des) %in% unique(hh$thrive_community_type)) %>%
  filter(!(com_des == "Agricultural" & urban_sub_rural == "Urban"),
         !(com_des == "Diversified Rural" & urban_sub_rural == "Urban"),
         !(com_des == "Rural Residential" & urban_sub_rural == "Urban"),

         !(com_des == "Emerging Suburban Edge" & urban_rural == "Urban"),
         !(com_des == "Suburban Edge" & urban_rural == "Urban"),
         !(com_des == "Suburban" & urban_rural == "Urban"))

hh_region <- hh %>%
  filter(hh_county %in% cprg_tbi_hh_counties,
         hh_in_mpo == TRUE) %>%
  select(hh_id, hh_county, survey_year,
         starts_with("cd_20")) %>%
  unique()


trip %>%
  filter(hh_id %in% hh$hh_id,
         mode_type %in% c(
           "Household Vehicle",
           "Other Vehicle",
           "For-Hire Vehicle"),
         trip_o_in_mpo == TRUE,
         trip_d_in_mpo == TRUE,
         duration_minutes > 0,
         as.character(trip_o_county) %in% cprg_tbi_hh_counties,
         as.character(trip_d_county) %in% cprg_tbi_hh_counties,
         distance_miles < 720,
         distance_miles > 0) %>%
  left_join(hh_region) %>%
  filter(trip_weight > 0,
         !is.na(cd_2050)) %>%
  srvyr::as_survey_design(id = trip_id, weights = trip_weight) %>%
  group_by(cd_2050_broad) %>%
  summarize(num_travelers_numeric = srvyr::survey_mean(num_hh_travelers_int, na.rm = T),
            n_trips = srvyr::survey_total(),
            n_trips_sample = n())


avo_exist <- transportation_data$passenger %>%
  filter(mode == "PLDV",
         var == "AVO") %>%
  select(mode, var, ctu, value) %>%
  unique()

avo_exist$value %>% mean()

demographic_data
