#### Import planned land use from Thrive 2040 and convert to usable residential categories
library(readxl)


## read in standard council planned land use data
landuse <- readxl::read_xls("./data-raw/land_use_data_processing/Thrive_2040/PlannedLandUseData.xls") %>%
  janitor::clean_names()

colnames(landuse)

length(unique(landuse$hsgden_rng)) # 11
length(unique(landuse$pluse_desc)) # 784
length(unique(landuse$hsg_den)) # 404
length(unique(landuse$metc_desc)) # 2271

# are there categories without housing density that we want?
no_dens <- filter(landuse, is.na(hsg_den)) %>% distinct(pluse_desc)
### flags: 	Low Density Residential, ...Mixed Use..., Residential - Business,
# Urban Expansion, Urban Planning Areas


landuse_density <- filter(landuse, !is.na(hsg_den)) %>%
  # extract available housing density numbers from descriptions
  mutate(
    hsg_den = stringr::str_to_lower(hsg_den),
    units_string = stringr::str_extract(hsg_den, "[^unit]+"),
    unit_minimum = case_when(
      stringr::str_detect(units_string, "-") ~
        stringr::str_extract(units_string, "^[^-]+"), # range provided
      TRUE ~ stringr::str_extract(units_string, "\\d+(\\.\\d+)?") # no range, unit is min and max
    ) %>% as.numeric(),
    unit_maximum = case_when(
      stringr::str_detect(units_string, "-") ~
        stringr::str_extract(units_string, "[^-]+$"), # range provided
      TRUE ~ stringr::str_extract(units_string, "\\d+(\\.\\d+)?") # no range, unit is min and max
    ) %>% as.numeric(),
    unit_mean = (unit_minimum + unit_maximum) / 2,
    area_string = stringr::str_trim(sub(".*\\bper\\s*", "", hsg_den)),
    # convert to single area number
    area_numbers = stringr::str_extract_all(area_string, "\\d+(\\.\\d+)?"), # stores multiple numbers (ranges) as vector
    # compute numeric value:
    acreage = purrr::map_dbl(area_numbers, ~ {
      nums <- as.numeric(.x)
      if (length(nums) == 2) {
        mean(nums)
      } # range → mean
      else if (length(nums) == 1) {
        nums[1]
      } # single number
      else {
        1
      } # "acre" → assume 1
    }),
    units_per_acre = unit_mean / acreage
  )
# #extract housing information where available
# mutate(housing_type = case_when(
#        grepl("SFD",metc_desc) ~ "single_family_detached",
#        grepl("SFA",metc_desc) ~ "single_family_attached",
#        grepl("MF",metc_desc) ~ "multifamily_home",
#        TRUE  ~ "Unassigned"
# )
# )

ctu_planned_land_use_council <- landuse_density %>%
  # remove cats with no acreage
  filter(acres > 0) %>%
  group_by(
    geog_name = ctu_name, ctu_landuse_desc = pluse_desc, hsg_den, units_per_acre, unit_minimum,
    unit_maximum, per_acres = acreage, unit_mean
  ) %>%
  summarize(acres = sum(acres)) %>%
  ungroup() %>%
  left_join(geog_index)


### read in met council planned land use parcel clipout
### expectation is this will better match city submissions

landuse_parcel <- readxl::read_xlsx("./data-raw/land_use_data_processing/Thrive_2040/ctu_plu_parcel_clip.xlsx") %>%
  janitor::clean_names()

landuse_density_parcel <- filter(landuse_parcel, !is.na(hsg_den)) %>%
  # extract available housing density numbers from descriptions
  mutate(
    hsg_den = stringr::str_to_lower(hsg_den),
    units_string = stringr::str_extract(hsg_den, "[^unit]+"),
    unit_minimum = case_when(
      stringr::str_detect(units_string, "-") ~
        stringr::str_extract(units_string, "^[^-]+"), # range provided
      TRUE ~ stringr::str_extract(units_string, "\\d+(\\.\\d+)?") # no range, unit is min and max
    ) %>% as.numeric(),
    unit_maximum = case_when(
      stringr::str_detect(units_string, "-") ~
        stringr::str_extract(units_string, "[^-]+$"), # range provided
      TRUE ~ stringr::str_extract(units_string, "\\d+(\\.\\d+)?") # no range, unit is min and max
    ) %>% as.numeric(),
    unit_mean = (unit_minimum + unit_maximum) / 2,
    area_string = stringr::str_trim(sub(".*\\bper\\s*", "", hsg_den)),
    # convert to single area number
    area_numbers = stringr::str_extract_all(area_string, "\\d+(\\.\\d+)?"), # stores multiple numbers (ranges) as vector
    # compute numeric value:
    acreage = purrr::map_dbl(area_numbers, ~ {
      nums <- as.numeric(.x)
      if (length(nums) == 2) {
        mean(nums)
      } # range → mean
      else if (length(nums) == 1) {
        nums[1]
      } # single number
      else {
        1
      } # "acre" → assume 1
    }),
    units_per_acre = unit_mean / acreage
  )

ctu_planned_land_use_parcel <- landuse_density_parcel %>%
  # remove cats with no acreage
  filter(sum_acres > 0) %>%
  group_by(
    geog_name = ctu_name, ctu_landuse_desc = pluse_desc, hsg_den, units_per_acre, unit_minimum,
    unit_maximum, per_acres = acreage, unit_mean
  ) %>%
  summarize(acres = sum(sum_acres)) %>%
  ungroup() %>%
  left_join(geog_index)

# usethis::use_data(ctu_planned_land_use_residential, overwrite = TRUE)

# diagnostic
# landuse %>%
#   filter(ctu_name == "Plymouth") %>%
#   group_by(pluse_desc) %>%
#   summarize(acres = sum(acres)) %>%
#   filter(acres != 0)


### alternative - use met council regionalization numbers

planned_land_use_regionalized <- filter(landuse, !is.na(hsgden_rng)) %>%
  mutate(
    minimum_density_per_acre = case_when(
      hsgden_rng == "RURAL" ~ 1 / 39.9,
      hsgden_rng == "EXURBAN" ~ 1 / 2.5,
      hsgden_rng == "LOW" ~ 1,
      hsgden_rng == "MEDIUM" ~ 4,
      hsgden_rng == "HIGH" ~ 8,
      hsgden_rng == "VERYHIGH" ~ 12,
      hsgden_rng == "URBAN" ~ 20,
      TRUE ~ NA
    ),
    maximum_density_per_acre = case_when(
      hsgden_rng == "RURAL" ~ 1 / 2.51,
      hsgden_rng == "EXURBAN" ~ 1 / 1.1,
      hsgden_rng == "LOW" ~ 4,
      hsgden_rng == "MEDIUM" ~ 8,
      hsgden_rng == "HIGH" ~ 12,
      hsgden_rng == "VERYHIGH" ~ 20,
      hsgden_rng == "URBAN" ~ 100,
      TRUE ~ NA
    ),
    expected_density = (maximum_density_per_acre + minimum_density_per_acre) / 2
  ) %>%
  filter(!is.na(expected_density)) %>%
  group_by(ctu_name, hsgden_rng) %>%
  summarize(
    acres = sum(acres),
    minimum_density_per_acre = mean(minimum_density_per_acre),
    maximum_density_per_acre = mean(maximum_density_per_acre),
    expected_density = mean(expected_density)
  ) %>%
  mutate(housing_density = stringr::str_to_sentence(hsgden_rng)) %>%
  ungroup() %>%
  select(
    geog_name = ctu_name, housing_density, acres, minimum_density_per_acre,
    maximum_density_per_acre,expected_density
  ) %>%
  left_join(geog_index) %>%
  # remove cats with no acreage
  filter(acres > 0)

planned_land_use <- list(
  ctu_planned_land_use_council = ctu_planned_land_use_council,
  ctu_planned_land_use_parcel = ctu_planned_land_use_parcel,
  planned_land_use_regionalized = planned_land_use_regionalized
)

usethis::use_data(planned_land_use, overwrite = TRUE)
