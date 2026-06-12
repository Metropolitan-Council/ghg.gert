#### Compare agricultural communities by satellite and land use
library(readxl)


## read in standard council planned land use data
landuse <- readxl::read_xls("./data-raw/land_use_data_processing/Thrive_2040/PlannedLandUseData.xls") %>%
  janitor::clean_names()

ag_area <- read_rds("./data/agriculture_area.rda")
           readRDS()
