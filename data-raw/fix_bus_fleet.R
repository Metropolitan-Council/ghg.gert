pkgload::load_all()



bus_year_estimate <- tibble(year = unique(transportation_data$passenger$year),
                            n_bus = seq(865, by = 10, length = 9))

new_stock <- transportation_data$passenger %>%
  filter(var == "PMT",
         mode == "AT") %>%
  group_by(year) %>%
  mutate(region_pmt = sum(value)) %>%
  rowwise() %>%
  mutate(pct_of_total = value/region_pmt) %>%
  left_join(bus_year_estimate) %>%
  mutate(bus = n_bus * pct_of_total,
         value = ifelse(bus < 1 & !year %in% c("2015",
                                               "2018",
                                               "2020"), 1, bus),
         mode = "BU",
         var = "BCIStock",
         aeo_mode = "BUS") %>%
  select(mode, var, ctu, year, value, aeo_mode, type)

new_stock_all <- new_stock %>%
  bind_rows(new_stock %>%
              mutate(var = "TotStock"))


new_pass <- transportation_data$passenger %>%
  anti_join(new_stock_all, by = c("mode", "var", "ctu", "year", "aeo_mode", "type")) %>%
  bind_rows(new_stock_all)

transportation_data$passenger <- new_pass

usethis::use_data(transportation_data, overwrite = T)


