# fix bus fuel efficiency
library(councilR)
pkgload::load_all()
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
bus_fuel_economy <- tibble::tribble(
  ~Year, ~`Fuel.Usage.(Gallons)`, ~Hybrid.MPG, ~Hybrid.Mileage, ~Standard.MPG, ~Standard.Mileage, ~Total.Mileage, ~`Hybrid.%`, ~`Standard.%`, ~Total.Fleet.MPG,
  2009L,                 7265286,         5.4,         2655900,             4,          27130225,       29786125,      0.0892,        0.9108,              4.1,
  2010L,                 7165863,        5.16,         3082557,          4.06,          26696161,       29778718,      0.1035,        0.8965,             4.16,
  2011L,              6907458.65,        5.29,         3688972,          4.15,          26062342,       29751314,       0.124,         0.876,             4.31,
  2012L,              6824686.42,        5.27,         3845756,          4.28,          26127864,       29973620,      0.1283,        0.8717,             4.39,
  2013L,                 6899169,        5.35,         5167992,          4.33,          25701499,       30869491,      0.1674,        0.8326,             4.47,
  2014L,              6977846.97,         5.1,         5188796,           4.6,          26978525,       32167321,      0.1613,        0.8387,             4.61,
  2015L,              6899772.79,        5.46,         5231997,          4.59,          27270651,       32502648,       0.161,         0.839,             4.71,
  2016L,              6988674.84,        5.45,         5518283,          4.61,          27520716,       33038999,       0.167,         0.833,             4.73,
  2017L,               6868211.7,        5.46,         5453649,          4.66,          27330742,       32784391,      0.1663,        0.8337,             4.77,
  2018L,              6891797.97,        5.42,         5146831,          4.61,          27395057,       32541888,      0.1582,        0.8418,             4.72,
  2019L,              6818375.52,        5.27,         4860728,           4.6,          27113530,       31974258,       0.152,         0.848,             4.69,
  2020L,              5145303.34,        5.67,         3647144,          4.59,          20637100,       24284244,      0.1502,        0.8498,             4.72,
  2021L,              5081704.29,        5.72,         3591180,          4.65,          20712369,       24303549,      0.1478,        0.8522,             4.78
) %>%
  janitor::clean_names()


linear_mod <- lm(formula = total_fleet_mpg ~ year, data = bus_fuel_economy)

fuel_eco_predict <- tibble(year = seq(max(bus_fuel_economy$year), 2050)) %>%
  bind_rows(
    bus_fuel_economy %>%
      filter(year < 2021) %>%
      select(year, total_fleet_mpg)
  ) %>%
  arrange(year)




new_bus_mpg <- fuel_eco_predict %>%
  mutate(
    fleet_mpg =
      c(predict(linear_mod, fuel_eco_predict))
  ) %>%
  filter(year %in% transportation_data$passenger$year) %>%
  mutate(new_fleet_mpg = ifelse(is.na(total_fleet_mpg), fleet_mpg, total_fleet_mpg))

existing_bus_mpg <- transportation_data$passenger %>%
  filter(
    var == "BCIMPG",
    mode == "BU"
  ) %>%
  select(var, year, value) %>%
  unique() %>%
  arrange(year)

bind_rows(
  new_bus_mpg %>%
    mutate(
      year = as.numeric(year),
      fleet_mpg = new_fleet_mpg,
      version = "Predicted, fleet mpg ~ year"
    ) %>%
    select(year, fleet_mpg, version),
  bus_fuel_economy %>%
    mutate(
      year = as.numeric(year),
      fleet_mpg = total_fleet_mpg,
      version = "Observed"
    ) %>%
    select(year, fleet_mpg, version),
  existing_bus_mpg %>%
    mutate(
      year = as.numeric(year),
      fleet_mpg = value,
      version = "SHCN, diesel"
    ) %>%
    select(year, fleet_mpg, version)
) %>%
  ggplot(aes(
    x = year,
    y = fleet_mpg,
    group = version,
    color = version,
    # fill = var,
    label = round(fleet_mpg, 1)
  )) +
  geom_point() +
  geom_line() +
  geom_text(
    nudge_y = 0.1,
    size = 4.6,
    check_overlap = T
  ) +
  theme(legend.position = "bottom") +
  labs(
    title = "Bus fuel economy",
    y = stringr::str_wrap("Fleet, miles per gallon", 10)
  )
# geom_text(nudge_y = 100) +
# geom_area(position = "stack") +
# facet_wrap(~version,
#            nrow = 2)


# ggsave("data-raw/peer_review/figs/corrected_bus_fuel.png",
#   width = 11,
#   height = 6
# )


