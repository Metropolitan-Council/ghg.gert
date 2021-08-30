## code to prepare `all_modes` dataset goes here


mode_index <- list(
  "Freight air" = "AIR",
  "Autonomous vehicle" = "AV",
  "Bike" = "BIKE",
  "Bus rapid transit" = "BRT",
  "School bus" = "BS",
  "Transit bus" = "BU",
  "Freight combination truck" = "CUT",
  "Dynamic ride sharing" = "DRS",
  "Freight rail" = "FR",
  "Freight multimodal" = "MM",
  "Passenger vehicle" = "PLDV",
  "Passenger rail" = "RI",
  "Passenger lightrail" = "RU",
  "Freight single truck" = "SUT",
  "Walk" = "WALK",
  "Freight water" = "WAT"
)

all_stocks <- c(
  "BCIStock",
  "BEVStock",
  "HEVStock",
  "PHEVStock",
  "SIS"
)

all_mpg <- c()

all_mpe <- c(
  "BEVElec",
  "PHEVElec"
)

all_class <- c(
  "BCI", # biodiesel
  "BEV", # battery electric vehicles
  "BIKE", # bike
  "CI", # diesel
  "EV", # electric vehicles
  "HEV", # hybrid electric vehicle
  "PHEV", # plug-in hybrid vehicles
  "SI", # spark engine gasoline
  "WALK" # walk
)

# usethis::use_data(all_modes, overwrite = TRUE)
