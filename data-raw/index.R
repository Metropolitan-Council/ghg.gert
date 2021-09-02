library(tidyverse)


emission_sources <- read.csv("data-raw/sources-wDef.csv")

variables <- read.csv("data-raw/variables-wDef.csv")

modes <- read.csv("data-raw/mode-wDef.csv")

# usethis::use_data(all_modes, overwrite = TRUE)
