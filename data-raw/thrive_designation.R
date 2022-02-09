library(tidyverse)
library(councilR)
library(sf)
library(readxl)

thrive <- councilR::import_from_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_thrive_msp2040_com_des/gpkg_society_thrive_msp2040_com_des.zip") %>%
  st_drop_geometry() %>%
  separate(COCTU_DESC, sep = " [(]", into = c("ctu", "cty"), fill = "right") %>%
  mutate(COMDESNAME = factor(COMDESNAME,  levels = c(
    "Urban Center",
    "Urban",
    "Suburban",
    "Suburban Edge",
    "Emerging Suburban Edge",
    "Rural Center",
    "Diversified Rural",
    "Rural Residential",
    "Agricultural",
    "Non-Council Area"
    ),
    ordered = T)) %>%
  group_by(ctu, CTU_NAME, COMDESNAME) %>%
  count() %>%
  group_by(ctu, CTU_NAME) %>%
  filter(as.integer(COMDESNAME) == max(as.integer(COMDESNAME))) %>%
  select(-n,
         ctu = CTU_NAME,
         ctu_name = ctu,
         com_des = COMDESNAME)
