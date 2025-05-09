##### bring in UrbanSim projection data for COCTUs and output CTU and County numbers

library(imputeTS)

ccap_ctu <- readRDS(file.path(here::here(), "data-raw/meta/ccap_ctu.RDS"))
ccap_county <- readRDS(file.path(here::here(), "data-raw/meta/ccap_county.RDS"))

##### read in and reformat UrbanSim output
# read in urbansim metadata
us_meta <- readxl::read_xlsx(
  "data-raw/meta/urbansim/datadictionary.xlsx"
) %>%
  janitor::clean_names()

### trunk path
us_path <- here::here("data-raw", "meta", "urbansim")
### list year files
us_list <- list.files(us_path)[1:5]

# function to read and process files
us_format <- function(year_folder) {
  file_full_path <- file.path(us_path, year_folder)  # Construct folder path
  files_in_folder <- list.files(file_full_path, full.names = TRUE)  # List files in folder

  # Read all files in the folder and bind them
  urbansim_data <- read.csv(files_in_folder) %>%
    pivot_longer(
      cols = 2:112,  # Adjust column selection as needed
      names_to = "variable"
    ) %>%
    left_join(us_meta, by = "variable") %>%
    mutate(
      coctu_id_full = stringr::str_pad(coctu_id, width = 11, pad = "0", side = "left"),
      ctu_id_gnis = substr(
        coctu_id_full,
        4,
        11),
      county_id_fips = substr(stringr::str_pad(coctu_id, width = 11, pad = "0", side = "left"),
                                1,3),
      inventory_year = as.numeric(year_folder)
      )
}

# Read and combine all files, assigning inventory year
us_formatted <- lapply(us_list, us_format) %>% bind_rows()%>%
  filter(!is.na(status)) %>%
  #only retain variables marked as ready for public display
  filter(status != "needs clarification")

#save intermediate file for model prediction
saveRDS(us_formatted, "data-raw/meta/urbansim_allyrs.RDS")

# reduce to categories of interest
us_formatted <- mutate(us_formatted,
                 sp_categories = case_when(
  variable == "total_households" ~ "total_households",
  variable == "total_pop" ~ "population",
  variable == "total_job_spaces" ~ "jobs",
  variable %in% c("jobs_sector_1",
                  "jobs_sector_2",
                  "jobs_sector_3") ~ "industrial_jobs",
  variable %in% c("jobs_sector_4",
                  "jobs_sector_5",
                  "jobs_sector_6",
                  "jobs_sector_7",
                  "jobs_sector_8",
                  "jobs_sector_9",
                  "jobs_sector_10") ~ "commercial_jobs",
  variable %in% c("single_fam_det_sl_own",
                  "single_fam_det_rent",
                  "manufactured_homes") ~ "single_family_small_lot",
  variable == "single_fam_det_ll_own" ~ "single_family_large_lot",
  variable %in% c("single_fam_attached_own",
                  "single_fam_attached_rent") ~ "single_family_attached",
  variable %in% c("multi_fam_own",
                  "multi_fam_rent") ~ "multifamily_units")
) %>%
  filter(!is.na(sp_categories))

us_ctu <- bind_rows(
  us_formatted%>%
  group_by(inventory_year,
           # coctu_id,
           ctu_id_gnis,
           sp_categories) %>%
  dplyr::summarize(value = sum(value)) %>%
  left_join(ccap_ctu %>% sf::st_drop_geometry() %>%
              dplyr::distinct(ctu_name,ctu_class,ctu_id_gnis)) %>%
  mutate(geog_name = dplyr::if_else(ctu_class == "TOWNSHIP",
                            paste(ctu_name, "Twp."),
                            ctu_name),
         geog_level = "ctu",
         geog_id = ctu_id_gnis,
         geog_id_type = "ctu_gnis") %>%
  ungroup(),
  us_formatted%>%
    group_by(inventory_year,
             # coctu_id,
             county_id_fips,
             sp_categories) %>%
    dplyr::summarize(value = sum(value)) %>%
    left_join(ccap_county %>% sf::st_drop_geometry() %>%
                dplyr::distinct(county_name,county_id_fips)) %>%
    mutate(ctu_class = "county",
           geog_id = county_id_fips,
           geog_id_type = "county_fips") %>%
    dplyr::rename(geog_name = county_name) %>%
    ungroup()
)  %>%
  select(inventory_year, geog_name, geog_id, geog_id_type, sp_categories, value, ctu_class) %>%
  ## fill in interstitial years (and backdate to 2005)
  group_by(geog_name, geog_id, geog_id_type, sp_categories, ctu_class) %>%
  tidyr::complete(inventory_year = tidyr::full_seq(c(2005, 2050), 1)) %>% # add interstitial years and expand to 2025
  dplyr::arrange(geog_name, geog_id, geog_id_type, sp_categories, inventory_year) %>%
  mutate(value = zoo::na.approx(value, x = inventory_year, rule = 2)) %>% # allow extrapolation
  ungroup()

# urbansim_meta <- tibble::tribble(
#   ~"Column", ~"Class", ~"Description",
#   "inventory_year", class(urbansim$inventory_year), "Inventory year",
#   "coctu_id", class(urbansim$coctu_id), "Unique county-city identifier",
#   "ctu_id", class(urbansim$ctu_id), "City-township-unorganized identifier",
#   "sp_categories", class(urbansim$variable), "Short variable name",
#   "value", class(urbansim$value), "County-city value of variable")

### calculate differences from 2021 (current base value)

# calculate urbansim deltas from base year to each other year
demographic_data <- us_ctu %>%
  left_join(
    us_ctu %>%
      dplyr::filter(inventory_year == 2021) %>%
      dplyr::rename(base_value = value) %>%
      select(-inventory_year)
  ) %>%
  mutate(value_change_from_base = value - base_value) %>%
  select(-base_value)


usethis::use_data(demographic_data, overwrite = TRUE)

