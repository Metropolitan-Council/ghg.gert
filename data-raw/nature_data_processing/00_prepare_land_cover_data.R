inpath <- "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_nature/data/"

natural_systems_data <- c()

natural_systems_data$land_cover_carbon <- readr::read_rds(paste0(inpath, "land_cover_carbon.rds"))

lc_ctu <- readr::read_rds(paste0(inpath, "nlcd_ctu_landcover_allyrs.rds"))  %>%
  mutate(ctu_name = str_to_title(ctu_name)) %>%
  mutate(
    ctu_name = case_when(
      ctu_class == "TOWNSHIP" ~ paste0(ctu_name, " Twp."),
      .default = ctu_name
    )
  ) %>%
  ungroup() %>%
  group_by(ctu_name, inventory_year, land_cover_type) %>%
  summarize(area = sum(area), .groups="keep") %>% ungroup()


natural_systems_data$ctu_lc_inventory <- lc_ctu %>%
  dplyr::select(ctu_name, inventory_year, land_cover_type,  area) %>%
  pivot_wider(names_from = land_cover_type, values_from = area) %>%
  rowwise() %>%
  mutate(TOTAL = rowSums(across(c(
    Bare, Developed_Low, Developed_Med, Developed_High,
    Urban_Grassland, Urban_Tree,
    Cropland, Grassland, Tree, Water,
    Wetland
  )),na.rm=T)) %>%
  ungroup()  %>%
  # replace NAs with 0
  mutate(across(everything(), ~replace_na(., 0)))


inventory_start_year <- head(sort(unique(natural_systems_data$ctu_lc_inventory$inventory_year)),1)
inventory_end_year <- tail(sort(unique(natural_systems_data$ctu_lc_inventory$inventory_year)),1)
future_years <- (inventory_end_year+1):2050

natural_systems_data$ctu_lc_null <- natural_systems_data$ctu_lc_inventory %>%
  filter(inventory_year == inventory_end_year) %>%
  select(-c(inventory_year, TOTAL)) %>%
  pivot_longer(cols = -ctu_name, names_to = "land_cover_type", values_to = "area") %>%
  crossing(inventory_year = future_years) %>%
  pivot_wider(names_from = "land_cover_type", values_from = "area") %>%
  rowwise() %>%
  mutate(TOTAL = sum(c_across(c(Bare, Developed_Low, Developed_Med, Developed_High,
                                Urban_Grassland, Urban_Tree,
                                Cropland, Grassland, Tree, Water,
                                Wetland)), na.rm = T)) %>%
  ungroup()  %>%
  # replace NAs with 0
  mutate(across(everything(), ~replace_na(., 0)))

usethis::use_data(natural_systems_data, overwrite = TRUE)

