inpath <- "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_agriculture/data/"

### load in livestock count data
livestock <- agriculture_activity_data$livestock

township_livestock <- read_rds("_agriculture/data/township_usda_census_data.rds")

### load in formatted activity data connecting livestock to manure emissions
vs <- readr::read_rds(paste0(inpath, "volatile_solids.rds")) %>%
  filter(state == "Minnesota") %>%
  ungroup() %>%
  select(-state)

nex <- readr::read_rds(paste0(inpath, "nitrogen_excretion.rds"))%>%
  filter(state == "MN") %>%
  ungroup() %>%
  select(-state)

mcf <- readr::read_rds(paste0(inpath, "methane_conversion_factor_livestock.rds"))%>%
  filter(state == "Minnesota") %>%
  ungroup() %>%
  select(-state)

# formatted files
ag_constants <- readr::read_rds(paste0(inpath,"ag_constants.rds"))

## convert to named vector for easier indexing
ag_constants_vec <- ag_constants %>%
  dplyr::select(short_text, value) %>%
  tibble::deframe()

ag_manure_mgmt <- readr::read_rds(paste0(inpath, "manure_management_systems.rds")) %>%
  mutate(storage_state = case_when(
    mgmt_system %in% c(
      "Anaerobic Lagoon",
      "Liquid/Slurry",
      "Liquid/ Slurry",
      "Deep Pit"
    ) ~ "Liquid",
    TRUE ~ "Solid"
  )
  )%>%
  filter(state == "MN") %>%
  ungroup() %>%
  select(-state)
# add missing lifestyle types, copying near matches

ag_manure_mgmt_complete <- ag_manure_mgmt %>%
  bind_rows(ag_manure_mgmt %>%
              filter(livestock_type == "Layers") %>%
              mutate(livestock_type = "Broilers")
  ) %>%
  bind_rows(ag_manure_mgmt %>%
              filter(livestock_type == "Layers") %>%
              mutate(livestock_type = "Pullets")) %>%
  bind_rows(ag_manure_mgmt %>%
              filter(livestock_type == "Dairy Cows") %>%
              mutate(livestock_type = "Calves"))


animal_manure_complete <- ag_manure_mgmt_complete %>%
  group_by(year, livestock_type, storage_state) %>%
  summarize(percentage = sum(percentage)) %>%
  ungroup()%>% # need to make solid and liquid sum to 1
  # ensure both Liquid and Solid exist for each group
  complete(year, livestock_type,
           storage_state = c("Liquid", "Solid")) %>%
  # missing percentages become 0
  mutate(percentage = replace_na(percentage, 0)) %>%
  group_by(year, livestock_type) %>%
  # normalize
  mutate(percentage = percentage / sum(percentage)) %>%
  ungroup()


### pull out and format Bo (max potential emissions (ch4/ kg vs))

Bo <- ag_constants %>%
  filter(grepl("Bo", description)) %>%
  mutate(
    livestock_type = case_when(
      grepl("swine", short_text) ~ "Swine",
      grepl("Feedlot", short_text) ~ "Feedlot Cattle",
      grepl("pullets", short_text) ~ "Pullets",
      grepl("broilers", short_text) ~ "Broilers",
      grepl("hens", short_text) ~ "Layers",
      grepl("sheep", short_text) ~ "Sheep",
      grepl("turkeys", short_text) ~ "Turkeys",
      grepl("goats", short_text) ~ "Goats",
      TRUE ~ short_text
    )
  ) %>%
  group_by(livestock_type) %>%
  summarize(Bo = mean(as.numeric(value)))

agriculture_variables <- list(
  mcf = mcf,
  vs = vs,
  manure_state = animal_manure_complete,
  nex = nex,
  Bo = Bo
)

usethis::use_data(agriculture_variables, overwrite=T)
