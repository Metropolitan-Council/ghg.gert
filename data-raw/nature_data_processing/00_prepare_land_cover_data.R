inpath <- "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_nature/data/"

natural_systems_data <- c()

natural_systems_data$land_cover_carbon <- readr::read_rds(paste0(inpath, "land_cover_carbon.rds"))

lc_ctu <- readr::read_rds(paste0(inpath, "nlcd_ctu_landcover_allyrs.rds")) %>%
  mutate(
    geog_name = dplyr::if_else(ctu_class == "TOWNSHIP",
      paste(ctu_name, "Twp."),
      ctu_name
    ),
    geog_id = stringr::str_pad(ctu_id, width = 8, pad = "0", side = "left")
  ) %>%
  ungroup() %>%
  group_by(geog_name, ctu_class, geog_id, inventory_year, land_cover_type) %>%
  dplyr::summarize(area = sum(area), .groups = "keep") %>%
  ungroup()

lc_county <- readr::read_rds(paste0(inpath, "nlcd_county_landcover_allyrs.rds")) %>%
  mutate(
    geog_name = paste(county_name, "County"),
    geog_id = county_id,
    ctu_class = "COUNTY"
  ) %>%
  ungroup() %>%
  group_by(geog_name, ctu_class, geog_id, inventory_year, land_cover_type) %>%
  dplyr::summarize(area = sum(area), .groups = "keep") %>%
  ungroup()

lc <- bind_rows(
  lc_ctu,
  lc_county
)

natural_systems_data$ctu_lc_inventory <- lc %>%
  dplyr::select(geog_name, ctu_class, geog_id, inventory_year, land_cover_type, area) %>%
  pivot_wider(names_from = land_cover_type, values_from = area) %>%
  rowwise() %>%
  mutate(TOTAL = rowSums(across(c(
    Bare, Developed_Low, Developed_Med, Developed_High,
    Urban_Grassland, Urban_Tree,
    Cropland, Grassland, Tree, Water,
    Wetland
  )), na.rm = T)) %>%
  ungroup() %>%
  # replace NAs with 0
  mutate(across(everything(), ~ tidyr::replace_na(., 0)))


inventory_start_year <- head(sort(unique(natural_systems_data$ctu_lc_inventory$inventory_year)), 1)
inventory_end_year <- tail(sort(unique(natural_systems_data$ctu_lc_inventory$inventory_year)), 1)
future_years <- (inventory_end_year + 1):2050

natural_systems_data$ctu_lc_null <- natural_systems_data$ctu_lc_inventory %>%
  filter(inventory_year == inventory_end_year) %>%
  select(-c(inventory_year, TOTAL)) %>%
  pivot_longer(cols = -c(geog_name, geog_id, ctu_class), names_to = "land_cover_type", values_to = "area") %>%
  tidyr::crossing(inventory_year = future_years) %>%
  pivot_wider(names_from = "land_cover_type", values_from = "area") %>%
  rowwise() %>%
  mutate(TOTAL = sum(dplyr::c_across(c(
    Bare, Developed_Low, Developed_Med, Developed_High,
    Urban_Grassland, Urban_Tree,
    Cropland, Grassland, Tree, Water,
    Wetland
  )), na.rm = T)) %>%
  ungroup() %>%
  # replace NAs with 0
  mutate(across(everything(), ~ tidyr::replace_na(., 0)))

usethis::use_data(natural_systems_data, overwrite = TRUE)
