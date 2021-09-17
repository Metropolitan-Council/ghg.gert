# Reference materials for emission sources, variables, and travel modes

library(tidyverse)


emission_sources <- read_csv("data-raw/indices/sources-wDef.csv") %>%
  as_tibble()


variables <- read_csv("data-raw/indices/variables-wDef.csv") %>%
  as_tibble()


modes <- read_csv("data-raw/indices/mode-wDef.csv") %>%
  as_tibble()


transportation_index <- list(
  "emission_sources" = emission_sources,
  "variables" = variables,
  "modes" = modes
)

usethis::use_data(transportation_index, overwrite = TRUE)
