# Reference materials for emission sources, variables, and travel modes

library(tidyverse)


emission_sources <- read.csv("data-raw/indices/sources-wDef.csv")

variables <- read.csv("data-raw/indices/variables-wDef.csv")

modes <- read.csv("data-raw/indices/mode-wDef.csv")


transportation_index <- list(
  "emission_sources" = emission_sources,
  "variables" = variables,
  "modes" = modes
)

usethis::use_data(transportation_index, overwrite = TRUE)
