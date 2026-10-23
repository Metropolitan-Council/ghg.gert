#### Import planned land use from Thrive 2040 and convert to usable residential categories
devtools::load_all(".")
source("data-raw/meta/compile_townships.R")

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
  summarize(acres = sum(acres), .groups = "keep") %>%
  ungroup() %>%
  left_join(geog_index, by = "geog_name")

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
  summarize(acres = sum(sum_acres), .groups = "keep") %>%
  ungroup() %>%
  left_join(geog_index, by = "geog_name")


planned_land_use <- list(
  ctu_planned_land_use_council = ctu_planned_land_use_council,
  ctu_planned_land_use_parcel = ctu_planned_land_use_parcel
)

# township groups ----
# Group rows keep each member township's own land use categories: the
# township name is prefixed to ctu_landuse_desc (e.g. "Benton Twp. -
# Agricultural"), so categories stay distinct per township rather than
# merging same-named categories across townships. Matched on GNIS id; each
# category keeps its unit_mean, so the land use module still gets an
# acre-weighted density across the group. Member township rows themselves
# are unchanged.
density_cols <- c(
  "ctu_landuse_desc", "hsg_den", "units_per_acre", "unit_minimum",
  "unit_maximum", "per_acres", "unit_mean"
)

twp_member_ids <- twp_group_lookup()$geog_id

planned_land_use <- purrr::imap(planned_land_use, function(df, nm) {
  df_labeled <- df %>%
    mutate(ctu_landuse_desc = if_else(
      geog_id %in% twp_member_ids,
      paste(geog_name, "-", ctu_landuse_desc),
      ctu_landuse_desc
    ))

  out <- twp_group_replace(
    df,
    twp_group_sum(df_labeled,
      value_cols = "acres",
      group_cols = density_cols,
      key = "geog_id",
      designation_col = "imagine_designation",
      .fill = list(
        geog_id_type = TWP_GROUP_ID_TYPE,
        geog_level = TWP_GROUP_LEVEL
      )
    ) %>%
      mutate(geog_short_name = geog_name)
  )
  twp_group_check_sum(out, "acres", nm, key = "geog_id")
  out
})

usethis::use_data(planned_land_use, overwrite = TRUE)
