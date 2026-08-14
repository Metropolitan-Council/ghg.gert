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
    tidyr::pivot_longer(
      cols = 2:112,
      names_to = "variable"
    ) %>%
    left_join(us_meta, by = "variable") %>%
    mutate(
      geog_id = substr(
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
      coctu_id = stringr::str_pad(
        as.character(coctu_id),
        width = 11, pad = "0", side = "left"
      ),
      emissions_year = as.numeric(year_folder)
    )
}

# Read and combine all files, assigning emissions year
us_formatted <- lapply(us_list, us_format) %>%
  bind_rows() %>%
  filter(
    !is.na(status),
    geog_id != "00649741"
  ) %>%
  # only retain variables marked as ready for public display
  filter(status != "needs clarification") %>%
  # reduce to categories of interest
  mutate(sp_categories = case_when(
    variable == "total_households" ~ "total_households",
    variable == "total_pop" ~ "population",
    variable == "total_job_spaces" ~ "jobs",
    variable %in% c(
      "jobs_sector_1",
      "jobs_sector_2",
      "jobs_sector_3"
    ) ~ "industrial_jobs",
    variable %in% c(
      "jobs_sector_4",
      "jobs_sector_5",
      "jobs_sector_6",
      "jobs_sector_7",
      "jobs_sector_8",
      "jobs_sector_9",
      "jobs_sector_10"
    ) ~ "commercial_jobs",
    variable %in% c("js_type_12") ~ "industrial_job_space",
    variable %in% c(
      "js_type_1011",
      "js_type_13",
      "js_type_14"
    ) ~ "commercial_job_space",
    variable %in% c(
      "manufactured_homes"
    ) ~ "manufactured_homes",
    variable %in% c(
      "single_fam_det_sl_own",
      "single_fam_det_rent",
      "single_fam_det_ll_own"
    ) ~ "single_family_detached",
    variable %in% c(
      "single_fam_attached_own",
      "single_fam_attached_rent"
    ) ~ "single_family_attached",
    variable %in% c(
      "multi_fam_own",
      "multi_fam_rent"
    ) ~ "multifamily_units"
  )) %>%
  filter(!is.na(sp_categories))


##### Check whether UrbanSim subcategories sum to their aggregate rows
# total_households should equal SFD + SFA + MF + manufactured;
# jobs (total_job_spaces) should equal commercial_jobs + industrial_jobs.
# If these don't match, scaling must use subcategory sums as the denominator.
us_hh_check <- us_formatted %>%
  mutate(role = case_when(
    sp_categories == "total_households" ~ "total",
    sp_categories %in% c(
      "single_family_detached", "single_family_attached",
      "multifamily_units", "manufactured_homes"
    ) ~ "subtype"
  )) %>%
  filter(!is.na(role)) %>%
  group_by(coctu_id, emissions_year, role) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = role, values_from = value) %>%
  mutate(diff = total - subtype)

us_job_check <- us_formatted %>%
  mutate(role = case_when(
    sp_categories == "jobs" ~ "total",
    sp_categories %in% c("commercial_jobs", "industrial_jobs") ~ "subtype"
  )) %>%
  filter(!is.na(role)) %>%
  group_by(coctu_id, emissions_year, role) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = role, values_from = value) %>%
  mutate(diff = total - subtype)

hh_mismatch <- sum(abs(us_hh_check$diff) > 0.01, na.rm = TRUE)
job_mismatch <- sum(abs(us_job_check$diff) > 0.01, na.rm = TRUE)

message(
  "UrbanSim internal consistency check:\n",
  "  Households: ", hh_mismatch, "/", nrow(us_hh_check),
  " COCTU-years where total_households ≠ sum(subtypes)",
  if (hh_mismatch > 0) paste0(
    " (max diff: ", round(max(abs(us_hh_check$diff), na.rm = TRUE), 1), ")"
  ),
  "\n  Jobs: ", job_mismatch, "/", nrow(us_job_check),
  " COCTU-years where jobs ≠ sum(subtypes)",
  if (job_mismatch > 0) paste0(
    " (max diff: ", round(max(abs(us_job_check$diff), na.rm = TRUE), 1), ")"
  )
)


##### Save unscaled UrbanSim output before Imagine 2050 adjustment
# This preserves the raw UrbanSim modeling results at the COCTU × year level
# for comparison against the post-model-adjusted Imagine 2050 forecasts.
saveRDS(
  us_formatted %>%
    select(coctu_id, geog_id, county_id_fips, emissions_year, sp_categories, value),
  file.path(here::here(), "data-raw/urbansim_demographic_data_raw.RDS")
)


##### Aggregate to CTU, county, and region levels (unscaled)

demographic_data_ctu <- us_formatted %>%
  group_by(
    emissions_year,
    geog_id,
    sp_categories
  ) %>%
  dplyr::summarize(value = sum(value), .groups = "drop") %>%
  left_join(geog_index, by = "geog_id") %>%
  ungroup() %>%
  select(emissions_year, geog_name, geog_id, geog_id_type, geog_level, sp_categories, value)

demographic_data_county <- us_formatted %>%
  group_by(
    emissions_year,
    county_id_fips,
    sp_categories
  ) %>%
  dplyr::summarize(value = sum(value), .groups = "drop") %>%
  mutate(geog_id = paste0("27", county_id_fips)) %>%
  left_join(geog_index, by = "geog_id") %>%
  ungroup() %>%
  select(emissions_year, geog_name, geog_id, geog_id_type, geog_level, sp_categories, value)

demographic_data_region <- us_formatted %>%
  group_by(
    emissions_year,
    sp_categories
  ) %>%
  dplyr::summarize(value = sum(value), .groups = "drop") %>%
  mutate(geog_name = "Twin Cities Region") %>%
  left_join(geog_index, by = "geog_name") %>%
  ungroup() %>%
  select(emissions_year, geog_name, geog_id, geog_id_type, geog_level, sp_categories, value)


demographic_data <- bind_rows(
  demographic_data_ctu,
  demographic_data_county,
  demographic_data_region
) %>%
  group_by(geog_name, geog_id, geog_id_type, sp_categories, geog_level) %>%
  tidyr::complete(emissions_year = tidyr::full_seq(c(2005, 2050), 1)) %>%
  dplyr::arrange(geog_name, geog_id, geog_id_type, sp_categories, emissions_year) %>%
  mutate(value = zoo::na.approx(value, x = emissions_year, rule = 2)) %>%
  ungroup()


##### Scale to Imagine 2050 post-model-adjusted targets
#
# Both demographic_data and imagine_targets are now interpolated to annual,
# so every year gets its own scalar rather than only the sparse forecast years.

# Define which sp_categories roll up under each scaling group
household_categories <- c(
  "total_households", "single_family_detached",
  "single_family_attached", "multifamily_units", "manufactured_homes"
)
job_categories <- c(
  "jobs", "commercial_jobs", "industrial_jobs",
  "commercial_job_space", "industrial_job_space"
)

# Load Imagine 2050 targets (output of compile_imagine_2050_forecasts.R)
imagine_targets <- readRDS(
  file.path(here::here(), "data-raw/meta/imagine_2050_forecasts.RDS")
)

# Interpolate Imagine targets to annual.
# Imagine has sparse years (2020, [2022 for employment], 2030, 2040, 2050).
# Interpolate within 2020–2050; years outside that range get no scalar (→ 1).
imagine_annual <- imagine_targets %>%
  group_by(geog_id, geog_name, imagine_variable, sp_categories_match) %>%
  tidyr::complete(emissions_year = tidyr::full_seq(c(2020, 2050), 1)) %>%
  dplyr::arrange(emissions_year) %>%
  mutate(imagine_value = zoo::na.approx(imagine_value, x = emissions_year, rule = 2)) %>%
  ungroup()

# Compute scalars from the sum of subcategories, NOT from the aggregate rows.
# UrbanSim's total_households ≠ sum(SFD + SFA + MF + manufactured) and
# jobs ≠ sum(commercial_jobs + industrial_jobs) — they are independently
# reported. Deriving the scalar from the subcategory sum guarantees that
# the scaled subcategories reaggregate to the Imagine target.
household_subtypes <- c(
  "single_family_detached", "single_family_attached",
  "multifamily_units", "manufactured_homes"
)
job_subtypes <- c("commercial_jobs", "industrial_jobs")

us_hh_sum <- demographic_data %>%
  filter(sp_categories %in% household_subtypes) %>%
  group_by(geog_id, emissions_year) %>%
  summarize(us_sum = sum(value), .groups = "drop")

us_job_sum <- demographic_data %>%
  filter(sp_categories %in% job_subtypes) %>%
  group_by(geog_id, emissions_year) %>%
  summarize(us_sum = sum(value), .groups = "drop")

us_pop <- demographic_data %>%
  filter(sp_categories == "population") %>%
  select(geog_id, emissions_year, us_sum = value)

imagine_hh <- imagine_annual %>%
  filter(sp_categories_match == "total_households") %>%
  select(geog_id, emissions_year, imagine_value)

imagine_jobs <- imagine_annual %>%
  filter(sp_categories_match == "jobs") %>%
  select(geog_id, emissions_year, imagine_value)

imagine_pop <- imagine_annual %>%
  filter(sp_categories_match == "population") %>%
  select(geog_id, emissions_year, imagine_value)

scalar_hh <- us_hh_sum %>%
  inner_join(imagine_hh, by = c("geog_id", "emissions_year")) %>%
  mutate(scalar_hh = if_else(us_sum == 0, 1, imagine_value / us_sum)) %>%
  select(geog_id, emissions_year, scalar_hh)

scalar_jobs <- us_job_sum %>%
  inner_join(imagine_jobs, by = c("geog_id", "emissions_year")) %>%
  mutate(scalar_jobs = if_else(us_sum == 0, 1, imagine_value / us_sum)) %>%
  select(geog_id, emissions_year, scalar_jobs)

scalar_pop <- us_pop %>%
  inner_join(imagine_pop, by = c("geog_id", "emissions_year")) %>%
  mutate(scalar_pop = if_else(us_sum == 0, 1, imagine_value / us_sum)) %>%
  select(geog_id, emissions_year, scalar_pop)

# Apply scalars to subcategories and job space rows.
# total_households and jobs rows are replaced directly with Imagine values
# so they exactly match the target the subcategories now sum to.
# Years before 2020 (outside Imagine coverage) get scalar = 1 via the
# left_join producing NA, caught by the TRUE ~ 1 fallback.
demographic_data <- demographic_data %>%
  left_join(scalar_hh, by = c("geog_id", "emissions_year")) %>%
  left_join(scalar_jobs, by = c("geog_id", "emissions_year")) %>%
  left_join(scalar_pop, by = c("geog_id", "emissions_year")) %>%
  left_join(
    imagine_hh %>% rename(imagine_hh = imagine_value),
    by = c("geog_id", "emissions_year")
  ) %>%
  left_join(
    imagine_jobs %>% rename(imagine_jobs = imagine_value),
    by = c("geog_id", "emissions_year")
  ) %>%
  mutate(
    scalar = case_when(
      sp_categories %in% household_subtypes & !is.na(scalar_hh) ~ scalar_hh,
      sp_categories %in% c("commercial_job_space", "industrial_job_space") &
        !is.na(scalar_jobs) ~ scalar_jobs,
      sp_categories %in% job_subtypes & !is.na(scalar_jobs) ~ scalar_jobs,
      sp_categories == "population" & !is.na(scalar_pop) ~ scalar_pop,
      TRUE ~ 1
    ),
    value = case_when(
      # replace aggregate rows directly with Imagine values
      sp_categories == "total_households" & !is.na(imagine_hh) ~ imagine_hh,
      sp_categories == "jobs" & !is.na(imagine_jobs) ~ imagine_jobs,
      # scale everything else proportionally
      TRUE ~ value * scalar
    )
  ) %>%
  select(-scalar_hh, -scalar_jobs, -scalar_pop, -scalar, -imagine_hh, -imagine_jobs)

# Report scaling diagnostics
n_matched <- scalars %>% filter(scalar != 1) %>% nrow()
message(
  "Imagine 2050 scaling applied: ",
  n_matched, " geog × year × category combinations adjusted"
)

# Spot-check: compare scaled totals against Imagine targets
if (interactive()) {
  check <- demographic_data %>%
    filter(sp_categories == "total_households") %>%
    inner_join(
      imagine_annual %>% filter(sp_categories_match == "total_households"),
      by = c("geog_id", "emissions_year")
    ) %>%
    mutate(diff = abs(value - imagine_value))
  message("Max household scaling residual: ", max(check$diff, na.rm = TRUE))
}


# calculate urbansim deltas from base year to each other year
# BASELINE YEAR SHOULD BE UPDATEABLE IN BUILDING ENERGY FLOW
demographic_data <- demographic_data %>%
  left_join(
    demographic_data %>%
      dplyr::filter(emissions_year == 2022) %>%
      dplyr::rename(base_value = value) %>%
      select(-emissions_year)
  ) %>%
  mutate(value_change_from_base = value - base_value) %>%
  select(-base_value)

anti_join(
  demographic_data,
  geog_index
) %>%
  distinct(geog_name, geog_level)

usethis::use_data(demographic_data, overwrite = TRUE)
