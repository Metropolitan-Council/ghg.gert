##### bring in UrbanSim projection data for COCTUs and output CTU and County numbers

ccap_ctu <- readRDS(file.path(here::here(), "data-raw/meta/ccap_ctu.RDS"))

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
  file_full_path <- file.path(us_path, year_folder) # Construct folder path
  files_in_folder <- list.files(file_full_path, full.names = TRUE) # List files in folder

  # Read all files in the folder and bind them
  urbansim_data <- read.csv(files_in_folder) %>%
    pivot_longer(
      cols = 2:112, # Adjust column selection as needed
      names_to = "variable"
    ) %>%
    left_join(us_meta, by = "variable") %>%
    mutate(
      ctu_id = as.numeric(substr(
        as.character(coctu_id),
        nchar(as.character(coctu_id)) - 6,
        nchar(as.character(coctu_id))
      )),
      inventory_year = as.numeric(year_folder)
    )
}

# Read and combine all files, assigning inventory year
us_formatted <- lapply(us_list, us_format) %>%
  bind_rows() %>%
  filter(!is.na(status)) %>%
  # only retain variables marked as ready for public display
  filter(status != "needs clarification")

# reduce to categories of interest
demographic_data <- mutate(us_formatted,
  sp_categories = case_when(
    variable == "total_households" ~ "households",
    variable == "total_pop" ~ "population",
    variable == "total_job_spaces" ~ "jobs",
    variable %in% c(
      "jobs_sectors_1",
      "jobs_sectors_2",
      "jobs_sectors_3"
    ) ~ "industrial_jobs",
    variable %in% c(
      "jobs_sectors_4",
      "jobs_sectors_5",
      "jobs_sectors_6",
      "jobs_sectors_7",
      "jobs_sectors_8",
      "jobs_sectors_9",
      "jobs_sectors_10"
    ) ~ "commercial_jobs",
    variable == "max_detached" ~ "single_family_units",
    variable == "max_multifam" ~ "multifamily_units"
  )
) %>%
  filter(!is.na(sp_categories)) %>%
  group_by(
    inventory_year,
    # coctu_id,
    ctu_id,
    sp_categories
  ) %>%
  summarize(value = sum(value)) %>%
  left_join(ccap_ctu %>% sf::st_drop_geometry() %>%
    distinct(ctu_name, ctu_class, ctu_id)) %>%
  mutate(ctu_name = if_else(ctu_class == "TOWNSHIP",
    paste(ctu_name, "Twp."),
    ctu_name
  )) %>%
  ungroup()
select(inventory_year, ctu_id, sp_categories, value, ctu_name)

# urbansim_meta <- tibble::tribble(
#   ~"Column", ~"Class", ~"Description",
#   "inventory_year", class(urbansim$inventory_year), "Inventory year",
#   "coctu_id", class(urbansim$coctu_id), "Unique county-city identifier",
#   "ctu_id", class(urbansim$ctu_id), "City-township-unorganized identifier",
#   "sp_categories", class(urbansim$variable), "Short variable name",
#   "value", class(urbansim$value), "County-city value of variable")

usethis::use_data(demographic_data, overwrite = TRUE)
