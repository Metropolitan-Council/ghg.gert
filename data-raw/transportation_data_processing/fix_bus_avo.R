# Replace bus average vehicle occupancy (AVO) with TMA specific values

devtools::load_all()

avo <- read.csv("data-raw/transportation_data_processing/ctu_tma/avo.csv") %>%
  select(-N)

load("data-raw/transportation_data_processing/ctu_tma/ctu_tma.rda")

ctu_tma <- ctu_tma %>%
  mutate(CTU_NAME = case_when(
    CTU_NAME == "Credit River" ~ "Credit River Twp.",
    # CTU_NAME == "Fort Snelling (unorg.)" ~ "Fort Snelling UT",
    TRUE ~ CTU_NAME
  ))


unique_ctu_year <- transportation_data$passenger %>%
  select(ctu, year) %>%
  unique()

bus_avo <- ctu_tma %>%
  left_join(avo, by = c("max_tma" = "MarketArea")) %>%
  mutate(
    avo = round(avo, digits = 2),
    ctu = CTU_NAME,
    value = avo,
    var = "AVO",
    aeo_mode = "BUS",
    mode = "BU",
    type = "P"
  ) %>%
  select(mode, var, ctu, value, aeo_mode, type) %>%
  right_join(unique_ctu_year) %>%
  select(names(transportation_data$passenger))


if (nrow(filter(bus_avo, is.na(value))) != 0) {
  cli::cli_abort(c(
    "x" = "Some CTUs have NA bus AVO",
    "*" = "Check CTU name joining"
  ))
}


if (interactive()) {
  comp <- bus_avo %>%
    mutate(vers = "new") %>%
    bind_rows(
      transportation_data$passenger %>%
        filter(
          mode == "BU",
          var == "AVO"
        ) %>%
        mutate(vers = "orig"),
    ) %>%
    select(ctu, var, value, vers) %>%
    unique() %>%
    pivot_wider(
      names_from = vers,
      values_from = value
    ) %>%
    mutate(diff = new - orig) %>%
    left_join(ctu_tma,
      by = c("ctu" = "CTU_NAME")
    )

  summary(comp$diff)

  ggplot(comp) +
    aes(
      x = ctu,
      y = diff,
      fill = factor(max_tma)
    ) +
    geom_col() +
    labs(
      y = "new - orig",
      title = "Most CTUs increase bus AVO"
    )

  # all CTU's increase, except Bloomington and St. Paul
  # which both decrease by less than 4 occupants

  # compare
  waldo::compare(
    bus_avo,
    transportation_data$passenger %>%
      filter(
        mode == "BU",
        var == "AVO"
      )
  )
}


transportation_data$passenger <- transportation_data$passenger %>%
  anti_join(bus_avo, by = c("mode", "var", "ctu", "year", "aeo_mode", "type")) %>%
  bind_rows(bus_avo)

usethis::use_data(transportation_data, overwrite = TRUE)
