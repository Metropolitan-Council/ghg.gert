# Preliminary code for working with FAF5 to replace freight data
pkgload::load_all()
library(data.table)
library(ggplot2)
library(stringr)
library(sf)
library(janitor)
# contains only metro CTUs
ctu_population <- readRDS("../../Interdivisional/ghg-cprg/_meta/data/ctu_population.RDS")

# load geographic objects
cprg_county <- readRDS("../../Interdivisional/ghg-cprg/_meta/data/cprg_county.RDS") %>%
  mutate(county_id = str_sub(geoid, 3, 5))

# contains all CTUs in the CPRG area
# includes data from Wisconsin only-ctus
cprg_ctu <- readRDS("../../Interdivisional/ghg-cprg/_meta/data/cprg_ctu.RDS") %>%
  # dplyr::filter(state_abb == "MN") %>%
  st_transform(26915)

cprg_ctu_slim <- cprg_ctu %>%
  left_join(
    cprg_county %>% sf::st_drop_geometry(),
    join_by(county_name, state_name, statefp, state_abb, cprg_area)
  ) %>%
  mutate(gnis = stringr::str_pad(as.character(gnis),
    width = 8, side = "left",
    pad = "0"
  )) %>%
  # mutate(coctu_id = paste0(county_id, stringr::str_pad(as.character(gnis),
  #                                                      width = 8, side = "left",
  #                                                      pad = "0"))) %>%
  left_join(
    ctu_population %>%
      select(ctu_class, gnis, coctu_id_fips, coctu_id_gnis, ctu_name, ctuid, geoid) %>%
      unique(),
    join_by(ctu_name, ctu_class, gnis, geoid)
  ) %>%
  select(ctu_name, ctu_class, gnis, ctuid, county_id, coctu_id = coctu_id_gnis, geometry)


# read in FAF5 links
#
# The Freight Analysis Framework (FAF5) - Network Links dataset was created from 2017 base year data and was published on April 11, 2022 from the Bureau of Transportation Statistics (BTS) and is part of the U.S. Department of Transportation (USDOT)/Bureau of Transportation Statistics (BTS) National Transportation Atlas Database (NTAD). The FAF (Version 5) Network contains 487,384 link features. All link features are topologically connected to permit network pathbuilding and vehicle assignment using a variety of assignment algorithms. The FAF Link and the FAF Node datasets can be used together to create a network. The link features include all roads represented in prior FAF networks, and all roads in the National Highway System (NHS) and the National Highway Freight Network (NHFN) that are currently open to traffic. Other included links provide connections between intersecting routes, and to select intermodal facilities and all U.S. counties. The network consists of over 588,000 miles of equivalent road mileage. The dataset covers the 48 contiguous States plus the District of Columbia, Alaska, and Hawaii.

gdb_path <- "data-raw/transportation_data_processing/ton-miles-traveled/Networks/Geodatabase Format/FAF5Network.gdb"

faf5_links <- sf::read_sf(gdb_path, layer = "FAF5_Links") %>%
  filter(STATE == "MN") %>%
  sf::st_transform(26915)

faf5_links_ctu <- st_intersection(
  faf5_links,
  cprg_ctu_slim
) %>%
  mutate(
    ctu_segment_dist = sf::st_length(.) %>%
      units::set_units("mile") %>%
      as.numeric(),
    # create unique identifier for CTU and COCTU segments
    unique_identifier_ctu = paste0(ID, "_", ctuid),
    unique_identifier_coctu = paste0(ID, "_", coctu_id)
  ) %>%
  clean_names()


faf5_links_ctu$id %>%
  unique() %>%
  length()


# read in FAF5.7 network flows
# https://geodata.bts.gov/datasets/freight-analysis-framework-faf5-highway-network-assignments/about
faf57_network_flows_all <- list.files(
  "data-raw/transportation_data_processing/ton-miles-traveled/FAF5_Highway_Assignment_Results/",
  recursive = TRUE,
  pattern = "csv", full.names = T
) %>%
  purrr::map_dfr(function(x) {
    fread(x) %>%
      clean_names() %>%
      filter(id %in% faf5_links$ID) %>%
      select(id, starts_with("tot")) %>%
      pivot_longer(2:27, names_to = "metric_name") %>%
      mutate(
        file = x %>% stringr::str_remove("data-raw/transportation_data_processing/ton-miles-traveled/"),
        faf_year = stringr::str_extract(file, "[:digit:][:digit:][:digit:][:digit:]"),
        movement_type = stringr::str_extract(file, "\\b(Domestic|Import|Export)\\b"),
        truck_type = stringr::str_extract(file, "\\b(SU|CU|Total Truck)\\b")
      )
  })


network_tons <- faf57_network_flows_all %>%
  filter(
    stringr::str_detect(metric_name, "tot_tons_[:digit:][:digit:]_*"),
    truck_type %in% c("SU", "CU")
  )

network_trips <- faf57_network_flows_all %>%
  filter(
    stringr::str_detect(metric_name, "tot_trips_[:digit:][:digit:]_*"),
    truck_type %in% c("SU", "CU")
  )

faf57_network_flows <- data.table::fread("data-raw/transportation_data_processing/ton-miles-traveled/FAF5_Highway_Assignment_Results/FAF5_2022_Highway_Assignment_Results/CSV Format/FAF5 Total CU Truck Flows by Commodity_2022.csv") %>%
  filter(ID %in% faf5_links_ctu$ID) %>%
  janitor::clean_names()

faf_ton_miles <- faf5_links_ctu %>%
  sf::st_drop_geometry() %>%
  select(
    id,
    # class, class_description, road_name, sign_rte,
    # length, state, stfips, county_name,
    # urban_code, fafzone, facility_type, hpms_begin_point, hpms_end_point, hpms_usa_route_id,
    ctu_segment_dist,
    names(cprg_ctu_slim)[1:5]
  ) %>%
  left_join(network_tons, by = c("id")) %>%
  mutate(ton_miles = value * ctu_segment_dist)


ctu_tmt_summary <- faf_ton_miles %>%
  filter(!is.na(truck_type)) %>%
  group_by(
    gnis, ctu_name, county_id,
    faf_year, truck_type
  ) %>%
  summarize(
    ton_miles = sum(ton_miles),
    ctu_segment_dist = sum(ctu_segment_dist),
    n_segments = n()
  )


ghg.ccap::transportation_data$freight %>%
  filter(mode == "CUT", var == "TMT") %>%
  View()

nhfn <- read_sf("data-raw/transportation_data_processing/ton-miles-traveled/National_NHFN_Designated_PHFS/National_NHFN_Designated_PHFS.shp")

# faf all -----

faf57_od <- fread("data-raw/transportation_data_processing/ton-miles-traveled/FAF5.7/FAF5.7.csv") %>%
  filter(
    dms_orig == 271 | dms_dest == 271,
    dms_mode == 1
  )

faf57_od %>%
  # filter(dms_orig == 271, dms_dest == 271) %>%
  summarize(tmiles_2017 = sum(tmiles_2017))

faf5_links
