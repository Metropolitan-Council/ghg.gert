#### Compare agricultural communities by satellite and land use

## read in standard council planned land use data
landuse <- read_csv("./data-raw/land_use_data_processing/gen_land_use") %>%
  janitor::clean_names()

ag_area <- agriculture_area %>%
  filter(inventory_year == 2020,
         geog_id %in% ctu_county_area$geog_id)

landuse_cat <- landuse %>% distinct(pluse_desc)

landuse_ag <- landuse %>%
  filter(grepl("agri", pluse_desc, ignore.case = TRUE) |
           grepl("rural resi", pluse_desc, ignore.case = TRUE)) %>%
  distinct(ctu_name, coctu_id) %>%
  mutate(geog_id = substr(coctu_id,4,11))

ag_no_landuse <- anti_join(ag_area,
                           landuse_ag,
                           by= "geog_id") %>%
  arrange(desc(area))

landuse %>% filter(grepl("Sand Creek Twp.",ctu_name)) %>% distinct(pluse_desc)

### NEED EXISTING LAND USE ###
