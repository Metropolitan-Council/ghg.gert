library(tidyverse)

pass_transpo <- read_csv("data-raw/transportation_data_processing/pass_transpo_dat.csv") %>%
  unique() %>%
  arrange(ctu) %>%
  mutate_at(4:12, as.numeric) %>%
  mutate_at(4:12, round, digits = 8)

pass_transpo_new <- read_csv("data-raw/transportation_data_processing/pass_transpo_dat_new.csv") %>%
  unique() %>%
  arrange(ctu) %>%
  mutate_at(4:12, as.numeric) %>%
  mutate_at(4:12, round, digits = 8)


new_sales_ex <- pass_transpo_new %>%
  filter(
    mode == "PLDV",
    stringr::str_detect(var, "Sales") | stringr::str_detect(var, "Existing")
  )

old_sales_ex <- pass_transpo %>%
  filter(
    mode == "PLDV",
    stringr::str_detect(var, "Sales") | stringr::str_detect(var, "Existing")
  )


old_sales_ex_long <- old_sales_ex %>%
  pivot_longer(
    cols = 4:12,
    names_to = "year",
    values_to = "value"
  )

new_sales_ex_long <- new_sales_ex %>%
  pivot_longer(
    cols = 4:12,
    names_to = "year",
    values_to = "value"
  )



full_join(old_sales_ex_long,
  new_sales_ex_long,
  by = c("mode", "var", "ctu", "year"),
  suffix = c("_old", "_new")
) %>%
  mutate(diff = value_new - value_old) %>%
  write.csv("data-raw/pass_old_new_compare.csv",
    row.names = FALSE
  )
