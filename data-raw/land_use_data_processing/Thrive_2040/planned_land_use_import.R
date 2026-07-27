#### Import planned land use from Thrive 2040 and convert to usable residential categories
devtools::load_all(".")

# helper: normalize CTU names to match geog_index conventions
normalize_ctu_name <- function(x) {
  stringr::str_replace(x, "St\\. ", "Saint ")
}

## read in standard council planned land use data
landuse <- readxl::read_xls("./data-raw/land_use_data_processing/Thrive_2040/PlannedLandUseData.xls") %>%
  janitor::clean_names()

landuse_density <- filter(landuse, !is.na(hsg_den)) %>%
  mutate(
    hsg_den = stringr::str_to_lower(hsg_den),
    units_string = stringr::str_extract(hsg_den, "[^unit]+"),
    unit_minimum = case_when(
      stringr::str_detect(units_string, "-") ~
        stringr::str_extract(units_string, "^[^-]+"),
      TRUE ~ stringr::str_extract(units_string, "\\d+(\\.\\d+)?")
    ) %>% as.numeric(),
    unit_maximum = case_when(
      stringr::str_detect(units_string, "-") ~
        stringr::str_extract(units_string, "[^-]+$"),
      TRUE ~ stringr::str_extract(units_string, "\\d+(\\.\\d+)?")
    ) %>% as.numeric(),
    unit_mean = (unit_minimum + unit_maximum) / 2,
    area_string = stringr::str_trim(sub(".*\\bper\\s*", "", hsg_den)),
    area_numbers = stringr::str_extract_all(area_string, "\\d+(\\.\\d+)?"),
    acreage = purrr::map_dbl(area_numbers, ~ {
      nums <- as.numeric(.x)
      if (length(nums) == 2) {
        mean(nums)
      } else if (length(nums) == 1) {
        nums[1]
      } else {
        1
      }
    }),
    units_per_acre = unit_mean / acreage
  )

ctu_planned_land_use_council <- landuse_density %>%
  filter(acres > 0) %>%
  group_by(
    geog_name = normalize_ctu_name(ctu_name),
    ctu_landuse_desc = pluse_desc, hsg_den, units_per_acre, unit_minimum,
    unit_maximum, per_acres = acreage, unit_mean
  ) %>%
  summarize(acres = sum(acres)) %>%
  ungroup() %>%
  left_join(geog_index)

### read in met council planned land use parcel clipout
landuse_parcel <- readxl::read_xlsx("./data-raw/land_use_data_processing/Thrive_2040/ctu_plu_parcel_clip.xlsx") %>%
  janitor::clean_names()

landuse_density_parcel <- filter(landuse_parcel, !is.na(hsg_den)) %>%
  mutate(
    hsg_den = stringr::str_to_lower(hsg_den),
    units_string = stringr::str_extract(hsg_den, "[^unit]+"),
    unit_minimum = case_when(
      stringr::str_detect(units_string, "-") ~
        stringr::str_extract(units_string, "^[^-]+"),
      TRUE ~ stringr::str_extract(units_string, "\\d+(\\.\\d+)?")
    ) %>% as.numeric(),
    unit_maximum = case_when(
      stringr::str_detect(units_string, "-") ~
        stringr::str_extract(units_string, "[^-]+$"),
      TRUE ~ stringr::str_extract(units_string, "\\d+(\\.\\d+)?")
    ) %>% as.numeric(),
    unit_mean = (unit_minimum + unit_maximum) / 2,
    area_string = stringr::str_trim(sub(".*\\bper\\s*", "", hsg_den)),
    area_numbers = stringr::str_extract_all(area_string, "\\d+(\\.\\d+)?"),
    acreage = purrr::map_dbl(area_numbers, ~ {
      nums <- as.numeric(.x)
      if (length(nums) == 2) {
        mean(nums)
      } else if (length(nums) == 1) {
        nums[1]
      } else {
        1
      }
    }),
    units_per_acre = unit_mean / acreage
  )

ctu_planned_land_use_parcel <- landuse_density_parcel %>%
  filter(sum_acres > 0) %>%
  group_by(
    geog_name = normalize_ctu_name(ctu_name),
    ctu_landuse_desc = pluse_desc, hsg_den, units_per_acre, unit_minimum,
    unit_maximum, per_acres = acreage, unit_mean
  ) %>%
  summarize(acres = sum(sum_acres)) %>%
  ungroup() %>%
  left_join(geog_index)


planned_land_use <- list(
  ctu_planned_land_use_council = ctu_planned_land_use_council,
  ctu_planned_land_use_parcel = ctu_planned_land_use_parcel
)

usethis::use_data(planned_land_use, overwrite = TRUE)
