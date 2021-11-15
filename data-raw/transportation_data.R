## code to prepare `transportation` dataset goes here
library(tidyverse)

pass_transpo <- read_csv("data-raw/pass_transpo_dat.csv") %>%
  unique() %>%
  arrange(ctu)

# make DRS and AV shares relative to 2050
# pass_transpo <- pass_transpo %>%
#   mutate(across(
#     all_of(4:12),
#     ~ case_when(
#       (var == "DRSShare") ~ .x /
#         pass_transpo %>%
#         filter(var == "DRSShare") %>%
#         select(`2050`) %>%
#         as.numeric(),
#       (var == "AVShare") ~ .x / pass_transpo %>%
#         filter(var == "AVShare") %>%
#         select(`2050`) %>%
#         as.numeric(),
#       TRUE ~ .x
#     )
#   ))

freight_transpo <- read_csv("data-raw/freight_transpo_dat.csv") %>%
  unique()


# passenger data -----
pass_transpo_long <- pass_transpo %>%
  group_by(mode, var, ctu) %>%
  mutate_at(4:12, as.numeric) %>%
  # mutate_at(4:12, replace_na, 0)
  pivot_longer(cols = c(
    `2015`, `2018`, `2020`,
    `2025`, `2030`, `2035`,
    `2040`, `2045`, `2050`
  ), names_to = "year") %>%
  mutate(
    aeo_mode = case_when(
      mode == "PLDV" ~ "LDV",
      mode == "BU" ~ "BUS",
      mode == "BRT" ~ "BUS",
      mode == "RU" ~ "RAIL",
      mode == "RI" ~ "RAIL",
      mode == "SUT" ~ "MDT",
      mode == "CUT" ~ "HDT",
      mode == "FR" ~ "FRAIL",
      mode == "BS" ~ "BUS",
      mode == "AV" ~ "LDV",
      mode == "DRS" ~ "LDV",
      mode %in% c("MM", "AIR", "WAT") ~ "FSHIP"
    ),
    type = "P",
    value = case_when(var == "PARK" ~ value/100,
                      TRUE ~ value)
  ) %>%
  group_by(mode, var, ctu, year, aeo_mode, type) %>%
  # selects highest value in case of duplicate entries
  top_n(1, value) %>%
  ungroup() %>%
  mutate(var = stringr::str_replace_all(var, "SAV", "DRS"))

ctu_year_unique <- pass_transpo_long %>%
  select(year, ctu) %>%
  filter(ctu != "All") %>%
  unique()

passenger_transpo_all <- pass_transpo_long %>%
  filter(ctu == "All") %>%
  select(-ctu) %>%
  right_join(ctu_year_unique) %>%
  select(names(pass_transpo_long))


# freight data -----
freight_transpo_long <- freight_transpo %>%
  group_by(mode, var, ctu) %>%
  mutate_at(4:12, as.numeric) %>%
  pivot_longer(cols = c(
    `2015`, `2018`, `2020`,
    `2025`, `2030`, `2035`,
    `2040`, `2045`, `2050`
  ), names_to = "year") %>%
  mutate(
    aeo_mode = case_when(
      mode == "PLDV" ~ "LDV",
      mode == "SUT" ~ "MDT",
      mode == "CUT" ~ "HDT",
      mode == "FR" ~ "FRAIL",
      mode == "AV" ~ "LDV",
      mode %in% c("MM", "AIR", "WAT") ~ "FSHIP"
    ),
    type = "F"
  ) %>%
  ungroup()



freight_transpo_all <- freight_transpo_long %>%
  filter(ctu == "All") %>%
  select(-ctu) %>%
  unique() %>%
  right_join(ctu_year_unique) %>%
  select(names(freight_transpo_long))



transportation_data <- list(
  passenger = rbind(
    pass_transpo_long %>%
      filter(ctu != "All"),
    passenger_transpo_all
  ) %>%
    unique(),
  freight = rbind(
    freight_transpo_long %>%
      filter(ctu != "All"),
    freight_transpo_all
  ) %>%
    unique()
)


testthat::expect_false("All" %in% transportation_data$passenger$ctu)
testthat::expect_false("All" %in% transportation_data$freight$ctu)

testthat::expect_equal(nrow(transportation_data$passenger), 154008)
testthat::expect_equal(nrow(transportation_data$freight), 71982)


usethis::use_data(transportation_data, overwrite = TRUE)


## value comparisons ------
orig_pass_transpo <- transportation_data$passenger %>%
  filter(
    ctu == "St. Paul",
    var == "TotStock",
    mode == "PLDV"
  )

testthat::expect_equal(
  orig_pass_transpo$value[1:7],

  # values from dataset as processed in ghg.sp.tool.model
  c(
    154401.15,
    161234.14,
    165789.47,
    166936.74,
    170939.95,
    173314.49,
    175867.3
  )
)
