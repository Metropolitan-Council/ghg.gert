#### load in past and future grid scenarios
install.packages("imputeTS")
library(imputeTS)

gwp <-
  list(
    "co2" = 1,
    "ch4" = 27.9,
    "n2o" = 273,
    "cf4" = 7380,
    "HFC-152a" = 164
  )

egrid <- readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/cd0fcbd022c0c35937bc10e1f9bcb23a3eacc239/_meta/data/epa_ghg_factor_hub.RDS") %>%
  pluck("egridTimeSeries") %>%
  mutate(emissions_year = as.numeric(Year),
         factor_source = Source,
         mt_per_mwh = as.numeric(value *
                                     units::as_units("pound") %>%
                                     units::set_units("metric_ton")),
         co2e = case_when(
           grepl("CH4", emission) ~ mt_per_mwh * gwp$ch4,
           grepl("N2O", emission) ~ mt_per_mwh * gwp$n2o,
           grepl("CO2", emission) ~ mt_per_mwh * gwp$co2
         )) %>%
  group_by(emissions_year, factor_source) %>%
  summarize(mt_co2e_per_mwh = sum(co2e)) %>%
  ungroup()

miso <- readr::read_csv("data-raw/building_energy_data_processing/grid/generated_emissions_MISO_LRZ_1_2023_2043_co2_for_electricity_1y.csv")

miso_ef <- miso %>%
  mutate(emissions_year = as.numeric(substr(start_date_utc,1,4)),
         mt_co2e_per_mwh = as.numeric(total_co2_intensity_lbs_per_mwh *
           units::as_units("pound") %>%
           units::set_units("metric_ton")),
         factor_source = "MISO regional projections")%>%
  select(emissions_year, mt_co2e_per_mwh,factor_source)

#bind these, removing 2024 as it seems low with surrounding year

grid_emissions  <- bind_rows(
  egrid, miso_ef %>% filter(!emissions_year == 2024)
) %>%
  right_join(
    data.frame(emissions_year = seq(from= 2005, to = 2050))
  ) %>%
  mutate(
    factor_source = if_else(
      is.na(mt_co2e_per_mwh),
      "Modeled",
      factor_source),
    log_co2e = log(mt_co2e_per_mwh) #prevent emissions from going negative while extrapolating
  ) %>%
  arrange(emissions_year) %>%
  # extrapolate
  mutate(
    log_co2e = imputeTS::na_kalman(log_co2e),
    mt_co2e_per_mwh = exp(log_co2e)
  ) %>%
  select(-log_co2e)

usethis::use_data(grid_emissions, overwrite = TRUE)
