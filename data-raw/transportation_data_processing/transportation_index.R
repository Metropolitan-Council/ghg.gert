# Reference materials for emission sources, variables, and travel modes

library(tidyverse)


emission_sources <- read_csv("data-raw/transportation_data_processing/indices/sources-wDef.csv") %>%
  as_tibble()


variables <- read_csv("data-raw/transportation_data_processing/indices/variables-wDef.csv") %>%
  as_tibble() %>%
  mutate(var_name = stringr::str_replace_all(var_name, "SAV", "DRS"))


modes <- read_csv("data-raw/transportation_data_processing/indices/mode-wDef.csv") %>%
  as_tibble() %>%
  filter(!mode_abbrev %in% c("AV", "DRS"))


aeo_desc <- read_csv("data-raw/transportation_data_processing/indices/aeo_descriptions.csv") %>%
  as_tibble()

uni_sources <- read.csv("data-raw/transportation_data_processing/indices/unique_sources.csv") %>%
  as_tibble() %>%
  mutate(across(where(is.character), stringr::str_trim))





# sources_key <-



transportation_index <- list(
  "emission_sources" = emission_sources,
  "variables" = variables,
  "modes" = modes,
  "aeo" = aeo_desc,
  "data_sources" = uni_sources
)





usethis::use_data(transportation_index, overwrite = TRUE)



# transportation_index$data_sources %>%
#   left_join(transportation_index$variables, by = c("var" = "var_name")) %>%
#   unique() %>%
#   select(mode, var, var_description, source_short) %>%
#   filter(str_detect(var, '(PHEV|SI|CI|HEV)', negate = TRUE)) %>% View
