library(tidyverse)
library(councilR)
library(sf)
library(readxl)

thrive <- councilR::import_from_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_thrive_msp2040_com_des/gpkg_society_thrive_msp2040_com_des.zip") %>%
  st_drop_geometry() %>%
  separate(COCTU_DESC, sep = " [(]", into = c("ctu", "cty"), fill = "right") %>%
  mutate(
    COMDESNAME = factor(COMDESNAME,
      levels = c(
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
      ordered = T
    ),
    URB_RURAL = stringr::str_sub(URB_RURAL, 1, 5),
    URB_SUB_RURAL = case_when(
      COMDESNAME %in% c(
        "Suburban",
        "Suburban Edge",
        "Emerging Suburban Edge"
      ) ~ "Suburban",
      TRUE ~ URB_RURAL
    ) %>%
      factor(levels = c(
        "Urban",
        "Suburban",
        "Rural"
      ), ordered = T)
  ) %>%
  group_by(ctu, CTU_NAME, COMDESNAME, URB_RURAL, URB_SUB_RURAL) %>%
  count() %>%
  group_by(ctu, CTU_NAME) %>%
  filter(as.integer(COMDESNAME) == max(as.integer(COMDESNAME))) %>%
  select(-n,
    ctu = CTU_NAME,
    ctu_name = ctu,
    com_des = COMDESNAME,
    urban_rural = URB_RURAL,
    urban_sub_rural = URB_SUB_RURAL
  )
