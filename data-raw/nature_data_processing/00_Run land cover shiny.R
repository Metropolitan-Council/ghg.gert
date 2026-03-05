# Clear environment -------------------------------------------------------
rm(list = ls())

# Load required packages --------------------------------------------------
# List the packages you'll need
ListOfPackages <- c(
  "tidyverse", "plotly", "patchwork", "usethis", "readr", "shiny",
  "tidyr", "purrr", "scales", "shinyWidgets",
  "bslib"
)

# From this list, check any that aren't currently installed
newPackages <- ListOfPackages[!(ListOfPackages %in% installed.packages()[, "Package"])]

# If any new packages are not currently loaded, load them now
if (length(newPackages)) install.packages(newPackages)
lapply(ListOfPackages, library, character.only = TRUE)

# Install the released version of councilR from GitHub.
remotes::install_github("Metropolitan-Council/councilR")
library(councilR)


inpath <- "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_nature/data/"

lc_county <- readr::read_rds(paste0(inpath, "nlcd_county_landcover_allyrs.rds"))
lc_ctu <- readr::read_rds(paste0(inpath, "nlcd_ctu_landcover_allyrs.rds")) %>%
  mutate(ctu_name = str_to_title(ctu_name)) %>%
  ungroup() %>%
  group_by(ctu_name, inventory_year, land_cover_type) %>%
  summarize(area = sum(area), .groups = "keep") %>%
  ungroup()

land_cover_c <- readr::read_rds(paste0(inpath, "land_cover_carbon.rds"))

# point to your directory with functions
funs_path <- paste0(here::here(), "/data-raw/nature_data_processing/")

source(paste0(funs_path, "01_logisticGrowth.R"))
source(paste0(funs_path, "01_mod1_lawnsToLegumes.R"))
source(paste0(funs_path, "01_mod2_urbanTreePlanting.R"))
source(paste0(funs_path, "01_mod3_cropRestoration.R"))
source(paste0(funs_path, "01_combine_module_deltas.R"))


# Set up data ---------------------------------------------------------
# Transform data for counties
hist_data_county <- lc_county %>%
  ungroup() %>%
  dplyr::select(county_name, inventory_year, land_cover_type, area) %>%
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
  mutate(across(everything(), ~ replace_na(., 0)))

inventory_start_year <- head(sort(unique(hist_data_county$inventory_year)), 1)
inventory_end_year <- tail(sort(unique(hist_data_county$inventory_year)), 1)
future_years <- (inventory_end_year + 1):2050


null_data_county <- hist_data_county %>%
  filter(inventory_year == inventory_end_year) %>%
  select(-c(inventory_year, TOTAL)) %>%
  pivot_longer(cols = -county_name, names_to = "land_cover_type", values_to = "area") %>%
  crossing(inventory_year = future_years) %>%
  pivot_wider(names_from = land_cover_type, values_from = area) %>%
  rowwise() %>%
  mutate(TOTAL = sum(c_across(c(
    Bare, Developed_Low, Developed_Med, Developed_High,
    Urban_Grassland, Urban_Tree,
    Cropland, Grassland, Tree, Water,
    Wetland
  )), na.rm = T)) %>%
  ungroup() %>%
  # replace NAs with 0
  mutate(across(everything(), ~ replace_na(., 0)))


hist_data_ctu <- lc_ctu %>%
  dplyr::select(ctu_name, inventory_year, land_cover_type, area) %>%
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
  mutate(across(everything(), ~ replace_na(., 0)))


null_data_ctu <- hist_data_ctu %>%
  filter(inventory_year == inventory_end_year) %>%
  select(-c(inventory_year, TOTAL)) %>%
  pivot_longer(cols = -ctu_name, names_to = "land_cover_type", values_to = "area") %>%
  crossing(inventory_year = future_years) %>%
  pivot_wider(names_from = "land_cover_type", values_from = "area") %>%
  rowwise() %>%
  mutate(TOTAL = sum(c_across(c(
    Bare, Developed_Low, Developed_Med, Developed_High,
    Urban_Grassland, Urban_Tree,
    Cropland, Grassland, Tree, Water,
    Wetland
  )), na.rm = T)) %>%
  ungroup() %>%
  # replace NAs with 0
  mutate(across(everything(), ~ replace_na(., 0)))


source(paste0(funs_path, "02_ui.R"))
source(paste0(funs_path, "02_server.R"))

shinyApp(ui = ui, server = server)
