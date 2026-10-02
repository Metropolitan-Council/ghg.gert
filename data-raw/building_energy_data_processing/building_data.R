## code to prepare `building_energy_bau_data` dataset goes here
library(ghg.gert)

# business as usual

building_data <- compile_bau_building_energy(tb = building_energy_data)


# usethis::use_data(building_energy_bau_data, overwrite = TRUE)


# building_data <- building_energy_bau_data

usethis::use_data(building_data, overwrite = TRUE)
