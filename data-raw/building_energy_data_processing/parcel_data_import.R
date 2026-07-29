# Script to import ancillary housing data from MN geospatial commons

library(dplyr)
library(tidyr)
library(readr)
library(sf)
library(arrow)

ccap_ctu <- readRDS(file.path(here::here(), "data-raw/meta/ccap_ctu.RDS"))

# --- download and cache raw parcel data ---

parcel_cache_path <- file.path(here::here(), "data-raw/meta/mn_parcel_raw.parquet")

if (file.exists(parcel_cache_path)) {
  cli::cli_alert_info("Loading cached parcel data from {parcel_cache_path}")
  mn_parcel <- arrow::read_parquet(parcel_cache_path)
} else {
  cli::cli_alert_info("Downloading parcel data from MN Geospatial Commons...")

  import_from_gpkg_all_layers <- function(link, .crs = 4326, .quiet = TRUE) {
    old_timeout <- getOption("timeout")
    options(timeout = max(600, old_timeout))
    on.exit(options(timeout = old_timeout))

    temp <- tempfile()
    download.file(link, temp, quiet = .quiet, mode = "wb")

    file_name <- link %>%
      strsplit("/") %>%
      `[[`(1) %>%
      tail(1) %>%
      gsub(pattern = "gpkg_", replacement = "") %>%
      gsub(pattern = ".zip", replacement = "")

    gpkg_path <- unzip(temp, paste0(file_name, ".gpkg"))
    on.exit(fs::file_delete(gpkg_path), add = TRUE)

    layers <- sf::st_layers(gpkg_path)$name

    purrr::map(layers, function(layer) {
      sf::read_sf(gpkg_path, layer = layer, quiet = .quiet) %>%
        sf::st_transform(crs = .crs)
    }) %>%
      dplyr::bind_rows(.id = "layer_name")
  }

  mn_parcel <- import_from_gpkg_all_layers(
    "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metrogis/plan_regonal_parcels_2021/gpkg_plan_regonal_parcels_2021.zip"
  ) %>%
    sf::st_drop_geometry() %>%
    select(
      CO_NAME, CTU_NAME, CTU_ID_TXT,
      DWELL_TYPE, HOME_STYLE,
      USECLASS1, USECLASS2, XUSECLASS1,
      FIN_SQ_FT, EMV_BLDG, YEAR_BUILT
    )

  arrow::write_parquet(mn_parcel, parcel_cache_path)
  cli::cli_alert_success("Cached parcel data to {parcel_cache_path}")
}


# --- classify parcels ---
# Parcel data is extremely messy and incomplete. Multiple columns with 100s of
# variable names get distilled into sensible mc_classification categories.
# Later case_when blocks intentionally override earlier ones where needed.

mn_parcel <- mn_parcel %>%
  mutate(
    mc_classification = case_when(
      ## RESIDENTIAL
      # Multifamily
      grepl("apartment|condo|apt|nursing|astd|eldry|fraternity|sorority", DWELL_TYPE, ignore.case = TRUE) ~ "multifamily_units",
      grepl("Apartment|APARMENT|Apt|Elderly Liv Fac|Housing - Low Income > 3 Units|HRA|Nursing|Sr Citizens", USECLASS1, ignore.case = TRUE) ~ "multifamily_units",
      grepl("APT|condo", HOME_STYLE, ignore.case = TRUE) ~ "multifamily_units",
      grepl("Apartment|APARMENT|Apt|Elderly Liv Fac|Housing - Low Income > 3 Units|HRA|Nursing|Sr Citizens", USECLASS2, ignore.case = TRUE) ~ "multifamily_units",

      # Manufactured homes
      grepl("mobile|manufactured", DWELL_TYPE, ignore.case = TRUE) ~ "manufactured_home",
      grepl("manufactured|MH", USECLASS1, ignore.case = TRUE) ~ "manufactured_home",
      grepl("manufactured|MH", HOME_STYLE, ignore.case = TRUE) ~ "manufactured_home",
      grepl("manufactured|MH", USECLASS2, ignore.case = TRUE) ~ "manufactured_home",

      # Single family attached
      grepl("townh|duplex|triplex|two-family|two family|three family|two residences|twin|multi res", DWELL_TYPE, ignore.case = TRUE) ~ "single_family_attached",
      grepl("Res 2-3|Double Bungalow|Duplex|Apartment|Low Income < 4 Units|Townh|Triplex", USECLASS1, ignore.case = TRUE) ~ "single_family_attached",
      grepl("Quad|Townh|duplex", HOME_STYLE, ignore.case = TRUE) ~ "single_family_attached",
      grepl("Res 2-3|Double Bungalow|Duplex|Apartment|Low Income < 4 Units|Townh|Triplex", USECLASS2, ignore.case = TRUE) ~ "single_family_attached",

      # Single family detached
      grepl("Frame|Cabin|BUNGALOW|SPLIT|Rambler|Log", HOME_STYLE, ignore.case = TRUE) ~ "single_family_detached",
      grepl("single|s.fam", DWELL_TYPE, ignore.case = TRUE) ~ "single_family_detached",
      grepl("Res 1 unit|CABIN|Residential|Zero Lot Line", USECLASS1, ignore.case = TRUE) ~ "single_family_detached",
      grepl("Res 1 unit|CABIN|Residential|Zero Lot Line", USECLASS2, ignore.case = TRUE) ~ "single_family_detached",
      TRUE ~ NA
    )
  ) %>%
  mutate(
    mc_classification = case_when(
      # Commercial
      grepl("store", DWELL_TYPE, ignore.case = TRUE) ~ "commercial",
      DWELL_TYPE %in% c(
        "RETAIL STR", "RESTAURANT", "REST FSTFD", "BAR/TAVERN", "MARKET",
        "DISCNT STR", "AUTO SHWRM", "AUTO CENTR", "POST OFFIC",
        "DAYCARECTR", "CLASSROOM", "THEATER", "BANK", "CARWASH", "BWLNGALLEY",
        "LAUNDROMAT", "MORTUARY", "COUNTRYCLB", "HAIRSALON", "CLUBHOUSE",
        "HEALTH CLB", "STABLE", "BED & BREAKFAST",
        "PRKNG STRC", "OFC,MD/DTL", "OFFICE", "SHPCTR, COM", "SHPCTR,NBH",
        "SERVC GAR", "VET HSPTL", "CHURCH", "CONV STORE", "SERVC STN",
        "HOTEL", "MOTEL", "OFC,CORPTE", "HOSPITALS", "CREAMERY", "DEPT STORE"
      ) ~ "commercial",
      grepl("Commercial|NON-PROFIT COMM|Com Ma & Pa|College|Skyways|Comm Services|SERVICE STATION|Golf Course|Condo|Cooperative|Restaurant|Marina|Arena|church", USECLASS1, ignore.case = TRUE) ~ "commercial",
      grepl("Commercial|Arena|Charit", USECLASS2, ignore.case = TRUE) ~ "commercial",
      grepl("church|Inst|Colleges|Private|Apprenticeship Training Facilities|hospitals", XUSECLASS1, ignore.case = TRUE) ~ "commercial",

      # Industrial
      DWELL_TYPE %in% c(
        "INDL,MANFG", "MFG/PROCES", "INDUSTRIAL IMPROVED", "SHED,UTIL",
        "INDUSTRIAL VACANT", "GREENHOUSE", "UTILITIES", "UTIL,TELCM",
        "GARG/STRG", "HANGAR/MTC", "SHED,EQUIP"
      ) ~ "industrial",
      grepl("Industrial|INDUSTIAL|Machinery|Utilit|Railroad|El Gen Mach|Utilities|Pub Util", USECLASS1, ignore.case = TRUE) ~ "industrial",
      grepl("Industrial", USECLASS2, ignore.case = TRUE) ~ "industrial",
      grepl("railroad|airport", XUSECLASS1, ignore.case = TRUE) ~ "industrial",

      # Public buildings
      grepl("School|Public|Municipal|County|State Property|FEDERAL|state", USECLASS1, ignore.case = TRUE) ~ "public_building",
      grepl("Cities|state|hwy dept|county|waste control|township|federal property|public schools|Commission", XUSECLASS1, ignore.case = TRUE) ~ "public_building",

      # Agricultural
      grepl("Agricultural|Farm|AG|Green Acres|Preserve|Managed Forrest", USECLASS1, ignore.case = TRUE) ~ "agriculture",
      grepl("Ag", USECLASS2, ignore.case = TRUE) ~ "agriculture",
      grepl("farm", XUSECLASS1, ignore.case = TRUE) ~ "agriculture",
      TRUE ~ mc_classification
    )
  ) %>%
  mutate(
    mc_classification = case_when(
      grepl("vacant|forfeit", DWELL_TYPE, ignore.case = TRUE) ~ "vacant",
      grepl("Vacant|Res V Land|VAC LAND|Unimproved|Wetlands|Forest|HUNTING|Open Space", USECLASS1, ignore.case = TRUE) ~ "no_building",
      grepl("Vacant|Wetlands|Common", USECLASS2, ignore.case = TRUE) ~ "no_building",
      grepl("street|park|dnr|cemetary", XUSECLASS1, ignore.case = TRUE) ~ "no_building",
      TRUE ~ mc_classification
    )
  ) %>%
  # remaining properties are EXEMPT — implying non-profit status, assign to commercial
  mutate(mc_classification = if_else(
    is.na(mc_classification) & FIN_SQ_FT > 0,
    "commercial",
    mc_classification
  )) %>%
  filter(!is.na(mc_classification))


# --- impute missing sqft/emv/year using CTU then county medians ---

mn_parcel <- mn_parcel %>%
  group_by(mc_classification, CTU_ID_TXT) %>%
  mutate(
    median_ctu_sqft = if_else(all(FIN_SQ_FT == 0 | is.na(FIN_SQ_FT)), NA_real_,
                              median(FIN_SQ_FT[FIN_SQ_FT > 0], na.rm = TRUE)
    ),
    median_ctu_emv = if_else(all(EMV_BLDG == 0 | is.na(EMV_BLDG)), NA_real_,
                             median(EMV_BLDG[EMV_BLDG > 0], na.rm = TRUE)
    ),
    median_ctu_year = if_else(all(YEAR_BUILT == 0 | is.na(YEAR_BUILT)), NA_real_,
                              median(YEAR_BUILT[YEAR_BUILT > 0], na.rm = TRUE)
    )
  ) %>%
  ungroup() %>%
  group_by(mc_classification, CO_NAME) %>%
  mutate(
    median_co_sqft = if_else(all(FIN_SQ_FT == 0 | is.na(FIN_SQ_FT)), NA_real_,
                             median(FIN_SQ_FT[FIN_SQ_FT > 0], na.rm = TRUE)
    ),
    median_co_emv = if_else(all(EMV_BLDG == 0 | is.na(EMV_BLDG)), NA_real_,
                            median(EMV_BLDG[EMV_BLDG > 0], na.rm = TRUE)
    ),
    median_co_year = if_else(all(YEAR_BUILT == 0 | is.na(YEAR_BUILT)), NA_real_,
                             median(YEAR_BUILT[YEAR_BUILT > 0], na.rm = TRUE)
    )
  ) %>%
  ungroup() %>%
  mutate(
    FIN_SQ_FT = if_else(
      is.na(FIN_SQ_FT) | FIN_SQ_FT == 0,
      coalesce(median_ctu_sqft, median_co_sqft, FIN_SQ_FT),
      FIN_SQ_FT
    ),
    EMV_BLDG = if_else(
      is.na(EMV_BLDG) | EMV_BLDG == 0,
      coalesce(median_ctu_emv, median_co_emv, EMV_BLDG),
      EMV_BLDG
    ),
    YEAR_BUILT = if_else(
      is.na(YEAR_BUILT) | YEAR_BUILT == 0,
      coalesce(median_ctu_year, median_co_year, YEAR_BUILT),
      YEAR_BUILT
    )
  ) %>%
  select(CO_NAME, CTU_NAME, CTU_ID_TXT, FIN_SQ_FT, EMV_BLDG, YEAR_BUILT, mc_classification)


# --- summarize to CTU level ---

residential_categories <- c(
  "single_family_detached",
  "single_family_attached",
  "multifamily_units",
  "manufactured_home"
)

mn_parcel_map <- mn_parcel %>%
  group_by(CO_NAME, CTU_NAME, CTU_ID_TXT, mc_classification) %>%
  summarize(
    median_sq_ft = median(FIN_SQ_FT),
    total_sq_ft = sum(FIN_SQ_FT),
    median_emv = median(EMV_BLDG),
    total_emv = sum(EMV_BLDG),
    median_year = median(YEAR_BUILT),
    .groups = "drop"
  ) %>%
  mutate(ctu_id = case_when(
    CTU_NAME == "Credit River" ~
      ccap_ctu$ctu_id_gnis[ccap_ctu$geog_name == "Credit River"],
    CTU_NAME == "Empire Township" ~
      ccap_ctu$ctu_id_gnis[ccap_ctu$geog_name == "Empire"],
    TRUE ~ CTU_ID_TXT
  )) %>%
  left_join(ccap_ctu, by = c(
    "ctu_id" = "ctu_id_gnis",
    "CO_NAME" = "county_name"
  )) %>%
  sf::st_drop_geometry()


# --- predict Hennepin sqft from EMV (Hennepin doesn't report sqft) ---

# extract observed residential parcels at CTU level
# keep median_emv through this step for the Hennepin prediction
ctu_parcel_obs <- mn_parcel_map %>%
  filter(mc_classification %in% residential_categories) %>%
  select(
    county_name = CO_NAME, geog_name, geog_id = ctu_id,
    mc_classification, sq_ft_use = median_sq_ft, median_emv, median_year
  )

# linear models: sqft ~ emv for non-Hennepin CTUs
sqft_lm_sfd <- lm(
  sq_ft_use ~ median_emv,
  ctu_parcel_obs %>%
    filter(mc_classification == "single_family_detached", county_name != "Hennepin")
)

sqft_lm_sfa <- lm(
  sq_ft_use ~ median_emv,
  ctu_parcel_obs %>%
    filter(mc_classification == "single_family_attached", county_name != "Hennepin")
)

# apply predictions to zero-sqft rows (Hennepin)
ctu_parcel_obs <- ctu_parcel_obs %>%
  mutate(
    sq_ft_use = case_when(
      sq_ft_use > 0 ~ sq_ft_use,
      mc_classification == "single_family_detached" ~
        predict(sqft_lm_sfd, data.frame(median_emv = median_emv)),
      mc_classification == "single_family_attached" ~
        predict(sqft_lm_sfa, data.frame(median_emv = median_emv)),
      TRUE ~ sq_ft_use
    )
  ) %>%
  select(-median_emv)


# --- fill in missing CTU-category combos using county averages ---
# Use ccap_ctu as the canonical CTU list so every CTU gets a row for
# every residential category, even if it has zero parcels of that type
# (e.g. Landfall has no residential parcels at all).

# county-level fallback values
county_medians <- ctu_parcel_obs %>%
  filter(
    !is.na(sq_ft_use), sq_ft_use > 0,
    !is.na(median_year), median_year > 0
  ) %>%
  group_by(county_name, mc_classification) %>%
  summarize(
    county_sq_ft = median(sq_ft_use, na.rm = TRUE),
    county_year = median(median_year, na.rm = TRUE),
    .groups = "drop"
  )

# complete CTU x category grid from ccap_ctu
ctu_reference <- ccap_ctu %>%
  sf::st_drop_geometry() %>%
  transmute(
    county_name,
    geog_name = if_else(
      ctu_class == "TOWNSHIP",
      paste(geog_name, "Twp."),
      geog_name
    ),
    geog_id = ctu_id_gnis
  ) %>%
  tidyr::crossing(mc_classification = residential_categories)

# join observed CTU data first, then county fallbacks for gaps
parcel_ctu <- ctu_reference %>%
  left_join(
    ctu_parcel_obs %>% select(geog_id, mc_classification, sq_ft_use, median_year),
    by = c("geog_id", "mc_classification")
  ) %>%
  left_join(
    county_medians,
    by = c("county_name", "mc_classification")
  ) %>%
  mutate(
    sq_ft_use = coalesce(sq_ft_use, county_sq_ft),
    median_year = coalesce(median_year, county_year)
  ) %>%
  select(county_name, geog_id, geog_name, mc_classification, sq_ft_use, median_year)


# --- add county-level weighted averages ---

housing_data <- ghg.ccap::demographic_data %>%
  filter(
    sp_categories %in% residential_categories,
    inventory_year == 2021
  )

housing_join <- parcel_ctu %>%
  left_join(
    housing_data %>% select(geog_id, sp_categories, units = value),
    by = c("geog_id", "mc_classification" = "sp_categories")
  )

county_weighted <- housing_join %>%
  filter(!is.na(units), units > 0) %>%
  group_by(county_name, mc_classification) %>%
  summarise(
    sq_ft_use = weighted.mean(sq_ft_use, w = units, na.rm = TRUE),
    median_year = weighted.mean(median_year, w = units, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(geog_name = paste(county_name, "County")) %>%
  left_join(
    ghg.ccap::geog_index %>% select(geog_name, geog_id),
    by = "geog_name"
  )

parcel_ctu <- bind_rows(parcel_ctu, county_weighted)

# --- clean up and save ---

rm(mn_parcel)
gc()

usethis::use_data(parcel_ctu, overwrite = TRUE)
