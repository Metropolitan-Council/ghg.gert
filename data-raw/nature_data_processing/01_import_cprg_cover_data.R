# Here we're going to import the area coverage data from ghg-cprg
# Goal is to have a holistic accounting of area covered by each cover type

# Clear old variables
rm(list = ls())

# Load required packages --------------------------------------------------
# List the packages you'll need
ListOfPackages <- c("tidyverse", "plotly", "patchwork", "usethis", "readr")

# From this list, check any that aren't currently installed
newPackages <- ListOfPackages[!(ListOfPackages %in% installed.packages()[,"Package"])]

# If any new packages are not currently loaded, load them now
if(length(newPackages)) install.packages(newPackages)
lapply(ListOfPackages, library, character.only=TRUE)

# Install the released version of councilR from GitHub.
remotes::install_github("Metropolitan-Council/councilR")
library(councilR)


inpath <- "https://github.com/Metropolitan-Council/ghg-cprg/raw/162-new-nlcd-data-rollout/_nature/data/"

lc_county <- readr::read_rds(paste0(inpath, "nlcd_county_landcover_allyrs.rds"))
lc_ctu <- readr::read_rds(paste0(inpath, "nlcd_ctu_landcover_allyrs.rds"))

seq_county <- readr::read_rds(paste0(inpath, "nlcd_county_landcover_sequestration_allyrs.rds"))
seq_cty <- readr::read_rds(paste0(inpath, "nlcd_ctu_landcover_sequestration_allyrs.rds"))

waterways_county <- readr::read_rds(paste0(inpath, "nhd_county_waterways_emissions_allyrs.rds"))
waterways_ctu <- readr::read_rds(paste0(inpath, "nhd_ctu_waterways_emissions_allyrs.rds"))

land_cover_c <- readr::read_rds(paste0(inpath, "land_cover_carbon.rds"))



usethis::use_data(lc_county, overwrite=T)











