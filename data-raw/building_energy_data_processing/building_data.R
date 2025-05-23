## code to prepare `building_energy_bau_data` dataset goes here
library(ghg.ccap)

# business as usual

building_data <- compile_bau_building_energy(tb = building_energy_data)

waldo::compare(
  building_data$residential %>%
    dplyr::filter(var == "multifamily_average_floor_area_sqft_county"),
  ghg.ccap::building_data$residential %>%
    dplyr::filter(var == "multifamily_average_floor_area_sqft_county")
)


waldo::compare(
  building_data$residential %>%
    dplyr::filter(var == "single_family_average_floor_area_sqft_ctu"),
  ghg.ccap::building_data$residential %>%
    dplyr::filter(var == "single_family_average_floor_area_sqft_ctu")
)



# usethis::use_data(building_energy_bau_data, overwrite = TRUE)


# building_data <- building_energy_bau_data

usethis::use_data(building_data, overwrite = TRUE)
