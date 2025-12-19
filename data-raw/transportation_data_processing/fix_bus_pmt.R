# fix bus PMT forecasts----
# Re-assign BRT PMT to BU PMT

# double check that all transit PMT == sum of transit PMTS
all_transit <- transportation_data$passenger %>%
  filter(
    var == "PMT",
    mode == "AT" # all transit
  ) %>%
  group_by(year, ctu) %>%
  summarize(total_vmt = sum(value), .groups = "keep")

man_all_transit <- transportation_data$passenger %>%
  filter(
    var == "PMT",
    mode %in% c("BU", "BRT", "RI", "RU")
  ) %>%
  group_by(year, ctu) %>%
  summarize(total_vmt = sum(value), .groups = "keep")

waldo::compare(all_transit, man_all_transit,
  tolerance = 0.1
)

# re-assign BRT PMT to BU ----
# NOT "BS" (school buses!!!!)
total_bus_pmt <- transportation_data$passenger %>%
  filter(
    var == "PMT",
    mode %in% c("BRT", "BU")
  ) %>%
  group_by(year, ctu) %>%
  summarize(total_pmt = sum(value), .groups = "keep")

bus_all_pmt <- transportation_data$passenger %>%
  filter(
    var == "PMT",
    mode %in% c("BRT", "BU")
  ) %>%
  group_by(year, ctu, var, aeo_mode, type) %>%
  summarize(value = sum(value), .groups = "keep") %>%
  mutate(mode = "BU") %>%
  select(names(transportation_data$passenger))


waldo::compare(
  total_bus_pmt,
  bus_all_pmt %>%
    group_by(year, ctu) %>%
    summarize(total_pmt = sum(value), .groups = "keep")
)

transportation_data$passenger <- transportation_data$passenger %>%
  filter(!(mode %in% c("BRT", "BU") & var == "PMT")) %>%
  bind_rows(bus_all_pmt)

usethis::use_data(transportation_data, overwrite = TRUE)
