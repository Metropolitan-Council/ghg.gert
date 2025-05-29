##### bring in UrbanSim projection data for COCTUs and output CTU and County numbers
library(dplyr)
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
  file_full_path <- file.path(us_path, year_folder)
  files_in_folder <- list.files(file_full_path, full.names = TRUE)

  # read all files in the folder and bind them
  urbansim_data <- read.csv(files_in_folder) %>%
    filter(!is.na(coctu_id)) %>%
    pivot_longer(
      cols = 2:112,
      names_to = "variable"
    ) %>%
    left_join(us_meta, by = "variable") %>%
    mutate(
      ctu_id_gnis = substr(
        as.character(coctu_id),
        nchar(as.character(coctu_id)) - 7,
        nchar(as.character(coctu_id))
      ),
      county_id_fips = stringr::str_pad(
        substr(
          as.character(coctu_id),
          1,
          nchar(as.character(coctu_id)) - 8
        ),
        width = 3, pad = "0", side = "left"
      ),
      inventory_year = as.numeric(year_folder)
    )
}

# Read and combine all files, assigning inventory year
us_formatted <- lapply(us_list, us_format) %>%
  bind_rows() %>%
  filter(
    !is.na(status),
    ctu_id_gnis != "00649741"
  ) %>%
  # only retain variables marked as ready for public display
  filter(status != "needs clarification") %>%
  # reduce to categories of interest
  mutate(sp_categories = case_when(
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
    variable %in% c("js_type_12") ~ "industrial_job_space",
    variable %in% c("js_type_1011",
                    "js_type_13",
                    "js_type_14") ~ "commercial_job_space",
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


demographic_data_ctu <- us_formatted %>%
  group_by(
    inventory_year,
    # coctu_id,
    ctu_id_gnis,
    sp_categories
  ) %>%
  dplyr::summarize(value = sum(value), .groups = "keep") %>%
  left_join(geog_index, by = c("ctu_id_gnis" = "geog_id")) %>%
  ungroup() %>%
  select(inventory_year, geog_name, geog_id = ctu_id_gnis, geog_id_type, geog_level, sp_categories, value)

demographic_data_county <- us_formatted %>%
  group_by(
    inventory_year,
    # coctu_id,
    county_id_fips,
    sp_categories
  ) %>%
  dplyr::summarize(value = sum(value), .groups = "keep") %>%
  left_join(
    ccap_county %>% sf::st_drop_geometry() %>%
      dplyr::distinct(geog_name, geog_level, county_id) %>%
      mutate(
        geog_name = paste(geog_name, "County"),
        county_id_fips = substr(county_id, 3, 5)
      ),
    by = join_by(county_id_fips)
  ) %>%
  mutate(geog_id_type = "county_fips") %>%
  ungroup() %>%
  select(inventory_year, geog_name, geog_id = county_id_fips, geog_id_type, geog_level, sp_categories, value)

demographic_data <- bind_rows(
  demographic_data_ctu,
  demographic_data_county
) %>%
  group_by(geog_name, geog_id, geog_id_type, sp_categories, geog_level) %>%
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

usethis::use_data(demographic_data, overwrite = TRUE)
