# Script to import ancillary housing data from MN geospatial commons

library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)
library(readr)

ccap_ctu <- readRDS(file.path(here::here(), "data-raw/meta/ccap_ctu.RDS"))

# fetch parcel data from MN Geospatial Commons
# need to modify import code from councilR to get multiple layers
import_from_gpkg_all_layers <- function(link, save_file = FALSE, save_path = getwd(), .crs = 4326,
                                        keep_temp = FALSE, .quiet = TRUE) {
  requireNamespace("rlang", quietly = TRUE)
  requireNamespace("sf", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)

  purrr::map(c(link), rlang:::check_string)
  purrr::map(c(save_file, keep_temp, .quiet), rlang:::check_bool)
  rlang:::check_string(save_path)
  rlang:::check_number_whole(.crs)

  temp <- tempfile()
  download.file(link, temp, quiet = .quiet)
  file_names <- strsplit(link, split = "/")
  file_name <- tail(file_names[[1]], 1) %>%
    gsub(pattern = "gpkg_", replacement = "") %>%
    gsub(pattern = ".zip", replacement = "")

  # Unzip the GeoPackage file
  gpkg_path <- unzip(temp, paste0(file_name, ".gpkg"))

  # Get a list of all layer names
  layers <- sf::st_layers(gpkg_path)$name

  # Read each layer and transform CRS
  layer_data <- purrr::map(layers, function(layer) {
    sf::read_sf(gpkg_path, layer = layer, quiet = .quiet) %>%
      sf::st_transform(crs = .crs)
  })

  # Combine all layers into one data frame
  out_sf <- dplyr::bind_rows(layer_data, .id = "layer_name")

  if (keep_temp == FALSE) {
    fs::file_delete(gpkg_path)
  }

  if (save_file == TRUE) {
    saveRDS(out_sf, paste0(save_path, "/", file_name, ".RDS"))
  }

  return(out_sf)
}

mn_parcel <- import_from_gpkg_all_layers(
  "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metrogis/plan_regonal_parcels_2021/gpkg_plan_regonal_parcels_2021.zip"
) %>%
  sf::st_drop_geometry()

### Parcel data is extremely messy and incomplete. The following code attempts
### to distill multiple columns with 100s of variable names into sensible classification

### after testing, this column appears to offer the largest percentage of
### first pass classification
mn_parcel %>%
  distinct(DWELL_TYPE) %>%
  dplyr::arrange() %>%
  print(n = 200)

# large possibility for error in classification. Attempting to structure so overriding of earlier
# cases happens as necessary
mn_parcel <- mn_parcel %>%
  mutate(
    mc_classification = case_when(
      ## RESIDENTIAL
      # Multifamily
      grepl("apartment|condo|apt|nursing|astd|eldry|fraternity|sorority", DWELL_TYPE, ignore.case = TRUE) ~ "multifamily",
      grepl("Apartment|APARMENT|Apt|Elderly Liv Fac|Housing - Low Income > 3 Units|HRA|Nursing|Sr Citizens", USECLASS1, ignore.case = TRUE) ~ "multifamily",
      grepl("APT|condo", HOME_STYLE, ignore.case = TRUE) ~ "multifamily",
      grepl("Apartment|APARMENT|Apt|Elderly Liv Fac|Housing - Low Income > 3 Units|HRA|Nursing|Sr Citizens", USECLASS2, ignore.case = TRUE) ~ "multifamily",


      # Mobile Homes
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
  ### COMM/IND/PUBLIC/AG
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

      ## public buildings
      grepl("School|Public|Municipal|County|State Property|FEDERAL|state", USECLASS1, ignore.case = TRUE) ~ "public_building",
      grepl("Cities|state|hwy dept|county|waste control|township|federal property|public schools|Commission", XUSECLASS1, ignore.case = TRUE) ~ "public_building",


      # agricultural
      grepl("Agricultural|Farm|AG|Green Acres|Preserve|Managed Forrest", USECLASS1, ignore.case = TRUE) ~ "agriculture",
      grepl("Ag", USECLASS2, ignore.case = TRUE) ~ "agriculture",
      grepl("farm", XUSECLASS1, ignore.case = TRUE) ~ "agriculture",
      TRUE ~ mc_classification
    )
  ) %>%
  ### EMPTY LAND
  mutate(
    mc_classification = case_when(
      # Vacant/Exempt
      grepl("vacant|forfeit", DWELL_TYPE, ignore.case = TRUE) ~ "vacant",

      # open space
      grepl("Vacant|Res V Land|VAC LAND|Unimproved|Wetlands|Forest|HUNTING|Open Space", USECLASS1, ignore.case = TRUE) ~ "no_building",
      grepl("Vacant|Wetlands|Common", USECLASS2, ignore.case = TRUE) ~ "no_building",
      grepl("street|park|dnr|cemetary", XUSECLASS1, ignore.case = TRUE) ~ "no_building",
      TRUE ~ mc_classification
    )
  )


mn_parcel_assigned <- mn_parcel %>%
  # remaining properties are EXEMPT - implying non-profit status, putting in commercial
  mutate(mc_classification = ifelse(is.na(mc_classification) & FIN_SQ_FT > 0,
    "commercial",
    mc_classification
  )) %>%
  filter(!is.na(mc_classification))


# quick check
tapply(mn_parcel_assigned$FIN_SQ_FT, mn_parcel_assigned$mc_classification, "median")
### zero is often the median value :/

big_building <- mn_parcel_assigned %>% filter(FIN_SQ_FT > 50000, mc_classification == "single_family_home")
test <- mn_parcel_assigned %>% filter(grepl("park", XUSECLASS1, ignore.case = TRUE), mc_classification != "no_building")

mn_parcel_assigned %>%
  filter(FIN_SQ_FT != 0) %>%
  count(mc_classification)

data_status <- mn_parcel_assigned %>%
  group_by(CO_NAME, mc_classification) %>%
  summarise(
    total_buildings = n(),
    zero_sq_ft = sum(EMV_BLDG == 0, na.rm = TRUE),
    non_zero_sq_ft = sum(EMV_BLDG != 0, na.rm = TRUE),
    pct_zero_sq_ft = (zero_sq_ft / total_buildings) * 100
  )


### fill in 0 data for mc_classification - first based on ctu_name when avaiable, and county_name where ctu is also blank

mn_parcel_predict <- mn_parcel_assigned %>%
  # Calculate mc_classification averages by CTU_NAME and CO_NAME
  group_by(mc_classification, CTU_ID_TXT) %>%
  mutate(
    mean_ctu_sqft = if_else(all(FIN_SQ_FT == 0 | is.na(FIN_SQ_FT)), NA_real_,
                            mean(FIN_SQ_FT[FIN_SQ_FT > 0], na.rm = TRUE)),
    mean_ctu_emv = if_else(all(EMV_BLDG == 0 | is.na(EMV_BLDG)), NA_real_,
                           mean(EMV_BLDG[EMV_BLDG > 0], na.rm = TRUE)),
    mean_ctu_year = if_else(all(YEAR_BUILT == 0 | is.na(YEAR_BUILT)), NA_real_,
                            mean(YEAR_BUILT[YEAR_BUILT > 0], na.rm = TRUE)),
    median_ctu_sqft = if_else(all(FIN_SQ_FT == 0 | is.na(FIN_SQ_FT)), NA_real_,
                              median(FIN_SQ_FT[FIN_SQ_FT > 0], na.rm = TRUE)),
    median_ctu_emv = if_else(all(EMV_BLDG == 0 | is.na(EMV_BLDG)), NA_real_,
                             median(EMV_BLDG[EMV_BLDG > 0], na.rm = TRUE)),
    median_ctu_year = if_else(all(YEAR_BUILT == 0 | is.na(YEAR_BUILT)), NA_real_,
                              median(YEAR_BUILT[YEAR_BUILT > 0], na.rm = TRUE))
  ) %>%
  ungroup() %>%
  group_by(mc_classification, CO_NAME) %>%
  mutate(
    mean_co_sqft = if_else(all(FIN_SQ_FT == 0 | is.na(FIN_SQ_FT)), NA_real_,
                           mean(FIN_SQ_FT[FIN_SQ_FT > 0], na.rm = TRUE)),
    mean_co_emv = if_else(all(EMV_BLDG == 0 | is.na(EMV_BLDG)), NA_real_,
                          mean(EMV_BLDG[EMV_BLDG > 0], na.rm = TRUE)),
    mean_co_year = if_else(all(YEAR_BUILT == 0 | is.na(YEAR_BUILT)), NA_real_,
                           mean(YEAR_BUILT[YEAR_BUILT > 0], na.rm = TRUE)),
    median_co_sqft = if_else(all(FIN_SQ_FT == 0 | is.na(FIN_SQ_FT)), NA_real_,
                             median(FIN_SQ_FT[FIN_SQ_FT > 0], na.rm = TRUE)),
    median_co_emv = if_else(all(EMV_BLDG == 0 | is.na(EMV_BLDG)), NA_real_,
                            median(EMV_BLDG[EMV_BLDG > 0], na.rm = TRUE)),
    median_co_year = if_else(all(YEAR_BUILT == 0 | is.na(YEAR_BUILT)), NA_real_,
                             median(YEAR_BUILT[YEAR_BUILT > 0], na.rm = TRUE))
  ) %>%
  ungroup() %>%
  # In-fill zeros AND NAs using medians
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


mn_parcel_map <- mn_parcel_predict %>%
  group_by(CO_NAME, CTU_NAME, CTU_ID_TXT, mc_classification) %>%
  summarize(
    median_sq_ft = median(FIN_SQ_FT),
    total_sq_ft = sum(FIN_SQ_FT),
    median_emv = median(EMV_BLDG),
    total_emv = sum(EMV_BLDG),
    median_year = median(YEAR_BUILT)
  ) %>%
  mutate(ctu_id = case_when(
    CTU_NAME == "Credit River" ~ # incorporated as city in 2021
      ccap_ctu$ctu_id_gnis[ccap_ctu$geog_name == "Credit River"],
    CTU_NAME == "Empire Township" ~ # incorporated as city in 2024
      ccap_ctu$ctu_id_gnis[ccap_ctu$geog_name == "Empire"],
    TRUE ~ CTU_ID_TXT
  )) %>%
  left_join(ccap_ctu, by = c(
    "ctu_id" = "ctu_id_gnis",
    "CO_NAME" = "county_name"
  )) %>%
  sf::st_as_sf()



### ctu_parcel output

ctu_parcel <- mn_parcel_map %>%
  ungroup() %>%
  sf::st_drop_geometry() %>%
  rename(county_name = CO_NAME) %>%
  select(-c(CTU_NAME, CTU_ID_TXT, statefp, state_abb)) %>%
  mutate(inventory_year = 2021,
         geog_name = if_else(ctu_class == "TOWNSHIP",
                             paste(geog_name, "Twp."),
                             geog_name))


### go back and use simple lm to fill in 0 sq ft cities (Hennepin)

sfa_parcel <- filter(ctu_parcel, mc_classification == "single_family_attached")
sfd_parcel <- filter(ctu_parcel, mc_classification == "single_family_detached")
mfh_parcel <- filter(ctu_parcel, mc_classification == "multifamily")
mfd_parcel <- filter(ctu_parcel, mc_classification == "manufactured_home")


# which cities are missing sfa?
no_detached <- anti_join(sfd_parcel, sfa_parcel, by = "geog_name") %>%
  distinct(county_name, geog_name, ctu_class) %>%
  print(n = 50)
mn_parcel %>%
  filter(CTU_NAME %in% no_detached$geog_name) %>%
  distinct(mc_classification)
mn_parcel %>% filter(CTU_NAME %in% no_detached$geog_name, is.na(mc_classification))

# seem to legitimitely not have these housing types
# will add median value to these city-house styles in case they're predicted to build

### Hennepin does not report square footage, need to predict from other counties estimated mean value ~ square footage relationship
sqft_lm_sfd <- lm(
  median_sq_ft ~ median_emv,
  sfd_parcel %>% filter(county_name != "Hennepin")
)

ggplot(data = sfd_parcel, aes(
  y = median_sq_ft, x = median_emv,
  col = county_name
)) +
  geom_point()

### counties all have similar slope but different intercept.
### However, Ramsey, Carver, and Anoka (Hennepin neighbors) are all median,
### so not going to try to adjust slope away from prediction for Hennepin

sfd_parcel$sq_ft_pred <- predict(
  sqft_lm_sfd,
  sfd_parcel
)

ggplot(data = sfd_parcel, aes(
  x = median_sq_ft, y = sq_ft_pred,
  col = county_name
)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)


# repeat for sfd

### Hennepin does not report square footage, need to predict from other counties estimated mean value ~ square footage relationship
sqft_lm_sfa <- lm(
  median_sq_ft ~ median_emv,
  sfa_parcel %>% filter(county_name != "Hennepin")
)

ggplot(data = sfa_parcel, aes(
  y = median_sq_ft, x = median_emv,
  col = county_name
)) +
  geom_point()
### not as tight of a relationship, but potentially less important for sfa

sfa_parcel$sq_ft_pred <- predict(
  sqft_lm_sfa,
  sfa_parcel
)

ggplot(data = sfa_parcel, aes(
  x = median_sq_ft, y = sq_ft_pred,
  col = county_name
)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

sfd_out <- sfd_parcel %>%
  mutate(sq_ft_use = if_else(median_sq_ft == 0,
    sq_ft_pred, median_sq_ft
  )) %>%
  select(county_name, ctu_id, geog_name, mc_classification, inventory_year, sq_ft_use, median_year)

sfa_out <- sfa_parcel %>%
  mutate(sq_ft_use = if_else(median_sq_ft == 0,
    sq_ft_pred, median_sq_ft
  )) %>%
  select(county_name, ctu_id, geog_name, mc_classification, inventory_year, sq_ft_use, median_year)

missing_cities <- anti_join(sfd_out, sfa_out, by = "ctu_id")

county_medians <- sfa_out %>%
  group_by(county_name) %>%
  summarize(
    sq_ft_use = median(sq_ft_use, na.rm = TRUE),
    median_year = median(median_year, na.rm = TRUE),
    .groups = "drop"
  )

missing_sfa_rows <- missing_cities %>%
  select(county_name, ctu_id, geog_name, inventory_year) %>%
  left_join(county_medians, by = "county_name") %>%
  mutate(mc_classification = "single_family_attached") %>%
  select(county_name, ctu_id, geog_name, mc_classification, inventory_year, sq_ft_use, median_year)

sfa_out_completed <- bind_rows(sfa_out, missing_sfa_rows)

### input multifamily year built in similar manner

mfh_out <- mfh_parcel %>%
  select(county_name, ctu_id, geog_name, mc_classification, inventory_year, sq_ft_use = median_sq_ft, median_year)

missing_cities_mf <- anti_join(sfd_out, mfh_parcel, by = "ctu_id")

county_medians_mfh <- mfh_out %>%
  group_by(county_name) %>%
  summarize(
    sq_ft_use = median(sq_ft_use, na.rm = TRUE),
    median_year = median(median_year, na.rm = TRUE),
    .groups = "drop"
  )

missing_mfh_rows <- missing_cities_mf %>%
  select(county_name, ctu_id, geog_name, inventory_year) %>%
  left_join(county_medians_mfh, by = "county_name") %>%
  mutate(mc_classification = "multifamily") %>%
  select(county_name, ctu_id, geog_name, mc_classification, inventory_year, sq_ft_use, median_year)

mfh_out_completed <- bind_rows(mfh_out, missing_mfh_rows)%>%
  mutate(mc_classification = "multifamily_units")

### lastly repeat for manufactured homes

### input multifamily year built in similar manner

mfd_out <- mfd_parcel %>%
  select(county_name, ctu_id, geog_name, mc_classification, inventory_year, sq_ft_use = median_sq_ft, median_year) %>%
  filter(!is.na(median_year),
         median_year != 0)

missing_cities_mfd <- anti_join(sfd_out, mfd_out, by = "ctu_id")

# no counties
median_mfd <- mfd_out %>%
  summarize(
    sq_ft_use = median(sq_ft_use, na.rm = TRUE),
    median_year = median(median_year, na.rm = TRUE),
    .groups = "drop"
  )

missing_mfd_rows <- missing_cities_mfd %>%
  select(county_name, ctu_id, geog_name, inventory_year) %>%
  cross_join(median_mfd) %>%
  mutate(mc_classification = "manufactured_home") %>%
  select(county_name, ctu_id, geog_name, mc_classification, inventory_year, sq_ft_use, median_year)

mfd_out_completed <- bind_rows(mfd_out, missing_mfd_rows)

parcel_ctu <- rbind(sfd_out, sfa_out_completed, mfh_out_completed, mfd_out_completed)

rm(mn_parcel)
rm(mn_parcel_assigned)
rm(mn_parcel_predict)
gc()


usethis::use_data(parcel_ctu, overwrite = TRUE)
