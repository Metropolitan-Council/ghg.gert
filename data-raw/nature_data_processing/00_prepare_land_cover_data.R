# inpath <- "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_nature/data/"
inpath <- "https://github.com/Metropolitan-Council/ghg-cprg/raw/236-incorporate-restorable-wetlands-into-natural-systems-projections/_nature/data/"

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
  dplyr::summarize(area = sum(area),
                   potential_wetland_area = sum(potential_wetland_area),
                   .groups = "keep") %>%
  ungroup()

lc_county <- readr::read_rds(paste0(inpath, "nlcd_county_landcover_allyrs.rds")) %>%
  mutate(
    geog_name = paste(county_name, "County"),
    geog_id = county_id,
    ctu_class = "COUNTY"
  ) %>%
  ungroup() %>%
  group_by(geog_name, ctu_class, geog_id, inventory_year, land_cover_type) %>%
  dplyr::summarize(area = sum(area),
                   potential_wetland_area = sum(potential_wetland_area),
                   .groups = "keep") %>%
  ungroup()


lc_region <- readr::read_rds(paste0(inpath, "nlcd_county_landcover_allyrs.rds")) %>%
  mutate(
    geog_name = "Regional",
    geog_id = "00000000",
    ctu_class = "REGION"
  ) %>%
  ungroup() %>%
  group_by(geog_name, ctu_class, geog_id, inventory_year, land_cover_type) %>%
  dplyr::summarize(area = sum(area),
                   potential_wetland_area = sum(potential_wetland_area),
                   .groups = "keep") %>%
  ungroup()


lc <- bind_rows(
  lc_ctu,
  lc_county,
  lc_region
)

natural_systems_data$inventory$ctu <- lc %>% filter(!(ctu_class %in% c("COUNTY","REGION")))
natural_systems_data$inventory$county <- lc %>% filter(ctu_class %in% c("COUNTY"))
natural_systems_data$inventory$region <- lc %>% filter(ctu_class %in% c("REGION"))


inventory_start_year <- head(sort(unique(natural_systems_data$inventory$region$inventory_year)), 1)
inventory_end_year <- tail(sort(unique(natural_systems_data$inventory$region$inventory_year)), 1)
future_years <- (inventory_end_year + 1):2050



natural_systems_data$projections$ctu <-
  natural_systems_data$inventory$ctu %>%
  filter(inventory_year == inventory_end_year) %>%
  select(-c(inventory_year)) %>%
  tidyr::crossing(inventory_year = future_years) %>%
  relocate(inventory_year, .after = geog_id) %>%
  arrange(geog_name, inventory_year,land_cover_type)


natural_systems_data$projections$county <-
  natural_systems_data$inventory$county %>%
  filter(inventory_year == inventory_end_year) %>%
  select(-c(inventory_year)) %>%
  tidyr::crossing(inventory_year = future_years) %>%
  relocate(inventory_year, .after = geog_id) %>%
  arrange(geog_name, inventory_year,land_cover_type)


natural_systems_data$projections$region <-
  natural_systems_data$inventory$region %>%
  filter(inventory_year == inventory_end_year) %>%
  select(-c(inventory_year)) %>%
  tidyr::crossing(inventory_year = future_years) %>%
  relocate(inventory_year, .after = geog_id) %>%
  arrange(geog_name, inventory_year,land_cover_type)



usethis::use_data(natural_systems_data, overwrite = TRUE)




