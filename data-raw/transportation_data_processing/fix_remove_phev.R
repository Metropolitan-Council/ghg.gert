# remove all autonomous vehicle references
# convert all PHEV to SI
pldv_stocks_new <- transportation_data$passenger %>%
  filter(
    mode == "PLDV",
    stringr::str_detect(var, "(Sales|Stock|Exist)"),
    stringr::str_detect(var, "Tot", negate = TRUE)
  ) %>%
  rowwise() %>%
  mutate(var = case_when(
    var == "PHEVStock" ~ "SIStock",
    var == "PHEVExist" ~ "SIExist",
    var == "PHEVSales" ~ "SISales",
    TRUE ~ var
  )) %>%
  group_by(mode, var, geog_name, geog_id, year, aeo_mode, type) %>%
  summarize(value = sum(value))


pldv_stocks_exist <- transportation_data$passenger %>%
  filter(
    mode == "PLDV",
    stringr::str_detect(var, "(Sales|Stock|Exist)"),
    stringr::str_detect(var, "(PHEV|SI)"),
    stringr::str_detect(var, "Tot", negate = TRUE)
  ) %>%
  pivot_wider(
    names_from = var,
    values_from = value
  ) %>%
  rowwise() %>%
  mutate(
    SISales = SISales + PHEVSales,
    SIStock = SIStock + PHEVStock,
    SIExist = SIExist + PHEVExist
  ) %>%
  select(-starts_with("PHEV")) %>%
  pivot_longer(starts_with("SI"),
    names_to = "var",
    values_to = "value"
  ) %>%
  arrange(geog_name, year, var)


# test to make sure that our new totals are as expected
pldv_stock_test <- pldv_stocks_new %>%
  filter(
    mode == "PLDV",
    stringr::str_detect(var, "(Sales|Stock|Exist)"),
    stringr::str_detect(var, "SI")
  ) %>%
  pivot_wider(
    names_from = var,
    values_from = value
  ) %>%
  pivot_longer(starts_with("SI"),
    names_to = "var",
    values_to = "value"
  ) %>%
  ungroup() %>%
  arrange(geog_name, year, var)

testthat::expect_equal(pldv_stocks_exist, pldv_stock_test)

# create new totals
pldv_stocks_new_tot <- pldv_stocks_new %>%
  filter(
    mode == "PLDV",
    stringr::str_detect(var, "(Sales|Stock|Exist)"),
    stringr::str_detect(var, "Tot", negate = TRUE)
  ) %>%
  pivot_wider(
    names_from = var,
    values_from = value
  ) %>%
  rowwise() %>%
  mutate(
    TotSales = SISales + CISales + HEVSales + BEVSales,
    TotStock = SIStock + CIStock + HEVStock + BEVStock,
    TotExist = SIExist + CIExist + HEVExist + BEVExist
  ) %>%
  pivot_longer(starts_with("Tot"),
    names_to = "var",
    values_to = "value"
  ) %>%
  arrange(geog_name, year, var) %>%
  select(names(pldv_stocks_new))


pldv_stocks_total_new <- bind_rows(
  pldv_stocks_new,
  pldv_stocks_new_tot
) %>%
  mutate(value = round(value, digits = 2))


transportation_data$passenger <- transportation_data$passenger %>%
  filter(!(mode == "PLDV" & var %in% unique(pldv_stocks_total_new$var))) %>%
  bind_rows(pldv_stocks_total_new) %>%
  filter(
    var != "PHEVPr",
    stringr::str_detect(var, "PHEV", negate = TRUE)
  )

usethis::use_data(transportation_data, overwrite = TRUE)
