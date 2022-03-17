bus_stock <- transportation_data$passenger %>%
  filter(
    str_detect(var, "Stock"),
    mode == "BU",
    var != "TotStock"
  ) %>%
  group_by(year, mode, var) %>%
  summarize(value = sum(value)) %>%
  ungroup()

bus_stock %>%
  tidyr::pivot_wider(
    values_from = value,
    names_from = var
  ) %>%
  rowwise() %>%
  mutate(total = sum(c(
    BEVStock,
    BCIStock,
    HEVStock
  )))


transit_pmt <- transportation_data$passenger %>%
  filter(mode == "AT") %>%
  group_by(year) %>%
  mutate(total_transit_region = sum(value)) %>%
  group_by(ctu, year) %>%
  mutate(pct_of_all = value / total_transit_region)
