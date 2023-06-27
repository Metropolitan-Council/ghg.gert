# Re-assign all bus powertrains to diesel -----

pkgload::load_all()
library(councilR)
library(dplyr)
library(ggplot2)
ggplot2::theme_set(
  if (testthat:::on_ci() == TRUE) {
    theme_minimal()
  } else {
    councilR::theme_council(
      use_showtext = T,
      use_manual_font_sizes = T
    )
  }
)

bus_stock_old <- transportation_data$passenger %>%
  filter(
    str_detect(var, "Stock"),
    mode %in% c("BU", "BRT"),
    var != "TotStock"
  ) %>%
  group_by(year, var) %>%
  summarize(n_bus = sum(value))


n_bus_per_year <- 2 # avg two new buses each year

bus_year_estimate <- tibble(
  year = seq(from = 2015, to = 2050, by = 1) %>% as.character(),
  n_bus = seq(from = 865, to = 1250, length = 36)
) %>%
  filter(year %in% transportation_data$passenger$year) %>%
  mutate(
    bus_sales = n_bus - lag(n_bus, 1),
    bus_sales = replace_na(bus_sales, 0),
    bus_exist = n_bus - bus_sales,
    bus_stock = bus_exist + bus_sales
  )

bus_year_estimate

# bus_year_estimate %>%
#   mutate(
#     var = "All",
#     version = "Update"
#   ) %>%
#   bind_rows(bus_stock_old %>%
#               mutate(version = "Original")) %>%
#   ggplot(aes(
#     x = as.numeric(year),
#     y = n_bus,
#     group = var,
#     color = var,
#     fill = var,
#     label = round(n_bus)
#   )) +
#   geom_point() +
#   geom_line() +
#   geom_text(
#     nudge_y = 100,
#     size = 4.5,
#     check_overlap = T
#   ) +
#   # geom_area(position = "stack") +
#   facet_wrap(~version,
#              nrow = 2
#   ) +
#   scale_y_continuous(labels = scales::comma) +
#   scale_x_continuous(n.breaks = 7) +
#   labs(
#     title = "Regional bus fleet",
#     y = "Buses",
#     x = "Year",
#     caption = paste0("BRT included. ", Sys.Date())
#   ) +
#   theme(legend.position = "bottom")
#
# ggsave("data-raw/peer_review/figs/corrected_bus_stock.png",
#        width = 10,
#        height = 8
# )

# apply changes to transportation_data -----

region_pct_of_total <- transportation_data$passenger %>%
  filter(
    var == "PMT",
    mode == "AT" # all transit
  ) %>%
  group_by(year) %>%
  mutate(region_pmt = sum(value)) %>%
  rowwise() %>%
  mutate(pct_of_total = value / region_pmt)


new_stock <- region_pct_of_total %>%
  left_join(bus_year_estimate) %>%
  mutate(
    bus = n_bus * pct_of_total,
    value = ifelse(bus < 1 & !year %in% c(
      "2015",
      "2018",
      "2020"
    ), 1, bus),
    mode = "BU",
    var = "BCIStock",
    aeo_mode = "BUS"
  ) %>%
  select(mode, var, ctu, year, value, aeo_mode, type)

new_sales <- region_pct_of_total %>%
  left_join(bus_year_estimate) %>%
  mutate(
    bus = bus_sales * pct_of_total,
    value = ifelse(bus < 1 & !year %in% c(
      "2015",
      "2018",
      "2020"
    ), 0, bus),
    mode = "BU",
    var = "BCISales",
    aeo_mode = "BUS"
  ) %>%
  select(mode, var, ctu, year, value, aeo_mode, type)

new_exist <- region_pct_of_total %>%
  left_join(bus_year_estimate) %>%
  mutate(
    bus = bus_exist * pct_of_total,
    value = ifelse(bus < 1 & !year %in% c(
      "2015",
      "2018",
      "2020"
    ), 0, bus),
    mode = "BU",
    var = "BCIExist",
    aeo_mode = "BUS"
  ) %>%
  select(mode, var, ctu, year, value, aeo_mode, type)


# generate 0s for all other bus fuel types

blank_alt_stock <- purrr::map_dfr(c("HEVStock", "BEVStock"), function(x) {
  new_stock %>%
    mutate(
      value = 0,
      var = x
    ) %>%
    unique()
})

blank_alt_sales <- purrr::map_dfr(c("HEVSales", "BEVSales"), function(x) {
  new_stock %>%
    mutate(
      value = 0,
      var = x
    ) %>%
    unique()
})

blank_alt_exist <- purrr::map_dfr(c("HEVExist", "BEVExist"), function(x) {
  new_stock %>%
    mutate(
      value = 0,
      var = x
    ) %>%
    unique()
})




# new stock, new_sales  join back to passenger -----
new_bus_fleet <- new_stock %>%
  # stock
  bind_rows(new_stock %>% # reassign TotStock to BCIStock
    mutate(var = "TotStock")) %>%
  bind_rows(blank_alt_stock) %>%
  # sales
  bind_rows(new_sales) %>%
  bind_rows(new_sales %>% # reassign TotSales
    mutate(var = "TotSales")) %>%
  bind_rows(blank_alt_sales) %>%
  # exist
  bind_rows(new_exist) %>%
  bind_rows(new_exist %>% # reassign TotStock to BCIStock
    mutate(var = "TotExist")) %>%
  bind_rows(blank_alt_exist) %>%
  mutate(value = round(value))



new_pass <- transportation_data$passenger %>%
  anti_join(new_bus_fleet,
    by = c("mode", "var", "ctu", "year", "aeo_mode", "type")
  ) %>%
  bind_rows(new_bus_fleet)

transportation_data$passenger <- new_pass

usethis::use_data(transportation_data, overwrite = TRUE)
