# avg minimum distance to transit stops from residential buildings

library(osmdata)
library(sf)
library(dplyr)
library(purrr)

# ctu <- councilR::import_from_gis("CountiesAndCTUs") %>%
  # st_transform(4326)



building_list <- c(
  "apartments",
  "bungalow",
  "cabin",
  "detatched",
  "dormitory",
  "farm",
  "house",
  "houseboat",
  "residential",
  "semidetatched_house",
  "static_caravan",
  "terrace")




# pull all residential building points -----

osm_buildings <- purrr::map(unique(ctu$CTU_NAME),
                            function(ctu_name){
                              ctu_geo <- ctu %>%
                                filter(CTU_NAME == ctu_name) %>%
                                sf::st_bbox()

                              res_points <- purrr::map(
                                building_list,
                                function(x,
                                         .ctu_name = ctu_name,
                                         .ctu_geo = ctu_geo){
                                  Sys.sleep(3)
                                  print(paste0(.ctu_name, " - ", x ))

                                  li <- osmdata::opq(ctu_geo) %>%
                                    osmdata::add_osm_feature(key = "building", value = x) %>%
                                    osmdata::osmdata_sf()

                                  return(li)
                                         # $osm_points
                                           # mutate(ctu_name = .ctu_name) %>%
                                           # filter(!is.na(building))
                                  # )
                                })

                              return(res_points)
                            })


# pull all transit stops -----


