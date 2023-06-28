## code to prepare `building_energy_bau_data` dataset goes here
library(ghg.sp)

# business as usual

building_energy_bau_data <- compile_bau_building_energy(tb = building_energy_data)

waldo::compare(building_energy_bau_data$residential %>%
                 dplyr::filter(var == "industrial_jobs"),
               ghg.sp::building_energy_bau_data$residential %>%
                 dplyr::filter(var == "industrial_jobs"))




usethis::use_data(building_energy_bau_data, overwrite = TRUE)


building_data <- building_energy_bau_data

usethis::use_data(building_data, overwrite = TRUE)
