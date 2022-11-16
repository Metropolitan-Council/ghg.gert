## code to prepare `building_energy_bau_data` dataset goes here
library(ghg.sp)

# business as usual

building_energy_bau_data <- ghg.sp::compile_bau_building_energy(tb = building_energy_data)

# strategies

usethis::use_data(building_energy_bau_data, overwrite = TRUE)
