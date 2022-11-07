# Replace bus average vehicle occupancy (AVO) with TMA specific values

devtools::load_all()

avo <- read.csv("data-raw/transportation_data_processing/ctu_tma/avo.csv") %>%
  select(-N)

load("data-raw/transportation_data_processing/ctu_tma/ctu_tma.rda")

unique_ctu_year <- transportation_data$passenger %>%
  select(ctu, year) %>%
  unique()

bus_avo <- ctu_tma %>%
  left_join(avo, by = c("max_tma" = "MarketArea")) %>%
  mutate(avo = round(avo, digits = 2),
         ctu = CTU_NAME,
         value = avo,
         var = "AVO",
         aeo_mode = "BUS",
         mode = "BU",
         type = "P") %>%
  select(mode, var, ctu, value, aeo_mode, type) %>%
  mutate(ctu = case_when(ctu == "Credit River" ~ "Credit River Twp.",
                         ctu == "Fort Snelling (unorg.)" ~ "Fort Snelling UT",
                         TRUE ~ ctu)) %>%
  right_join(unique_ctu_year) %>%
  select(names(transportation_data$passenger))


if(nrow(filter(bus_avo, is.na(value)))  != 0){
  cli::cli_abort(c(
    "x" = "Some CTUs have NA bus AVO",
    "*" = "Check CTU name joining"
  ))
}

transportation_data$passenger <- transportation_data$passenger %>%
  anti_join(bus_avo, by = c("mode", "var", "ctu", "year", "aeo_mode", "type")) %>%
  bind_rows(bus_avo)

usethis::use_data(transportation_data, overwrite = TRUE)
