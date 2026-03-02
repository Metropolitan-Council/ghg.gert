# Pull data from EIA
pkgload::load_all()
library(dplyr)
# install.packages("eia")
# you will also need to set up an EIA API key
# see https://docs.ropensci.org/eia/
library(eia)


# Pull annual gas and diesel prices ------
# Minnesota specific gas
gas_prices <- eia_data("petroleum/pri/gnd",
  freq = "annual",
  data = "value",
  facets = list(series = "EMM_EPM0_PTE_SMN_DPG")
)

# Midwest specific diesel
diesel_prices <- eia_data("petroleum/pri/gnd",
  freq = "annual",
  data = "value",
  facets = list(series = "EMD_EPD2D_PTE_R20_DPG")
)

si_fuel_cost <- gas_prices %>%
  filter(period == max(period)) %>%
  dplyr::pull(value)

ci_fuel_cost <- diesel_prices %>%
  filter(period == max(period)) %>%
  dplyr::pull(value)

# pull annual energy outlook  ----
aeo_year <- "2025"
aeo_scenarios <- c(
  paste0("ref", aeo_year),
  paste0("hm", aeo_year),
  paste0("lm", aeo_year),
  "highprice",
  "lowprice",
  "highogs",
  "lowogs"
)

eia_scenario_codes <- transportation_index$aeo %>%
  mutate(eia_code = aeo_scenarios) %>%
  select(-description)


aeo_mpg <- eia_data(
  paste0("aeo/", aeo_year),
  freq = "annual",
  facets = list(
    history = list(
      "PROJECTION",
      "HISTORIC"
    ),
    scenario = list(aeo_scenarios),
    seriesId = list(
      "efi_ldv_stk_tot_NA_NA_NA_mpggaseq",
      "efi_ldv_stk_car_NA_NA_NA_mpggaseq",
      "efi_ldv_trn_nlc_egh_NA_NA_mpggaseq",
      "efi_ldv_trn_nlc_gsl_NA_NA_mpggaseq",
      # "efi_ldv_trn_nlc_pi20gh_NA_NA_mpggaseq",
      # "efi_ldv2_new_NA_NA_NA_NA_mpggaseq",
      "efi_ldv_trn_nlc_pi50gh_NA_NA_mpggaseq",
      "efi_ldv_trn_nlt_tds_NA_NA_mpggaseq",
      "efi_NA_fght_rads_NA_NA_NA_NA",
      "efi_NA_fght_sosos_NA_NA_NA_NA",
      "efi_NA_trn_rail_NA_NA_NA_tonmlpthbtu",
      "efi_NA_trn_dmt_NA_NA_NA_tonmlpthbtu",
      "efi_ldv_trn_nlc_200miev_NA_NA_mpggaseq"
    )
  ),
  data = "value"
) %>%
  left_join(eia_scenario_codes, by = join_by(scenario == eia_code))


aeo_fuel_economy <- aeo_mpg %>%
  dplyr::arrange(seriesId, period) %>%
  mutate(
    aeo_mode = case_when(
      seriesName %in% c(
        "Light-Duty Fuel Economy : Conventional Cars : Gasoline",
        "Light-Duty Fuel Economy : Conventional Light Trucks : TDI Diesel",
        "Light-Duty Fuel Economy : Alternative-Fuel Cars : Electric-Gasoline Hybrid",
        "Light-Duty Fuel Economy : Alternative-Fuel Cars : Plug-in 50 Gasoline Hybrid",
        "Light-Duty Fuel Economy : Alternative-Fuel Cars : 200-Mile Electric Vehicle",
        "Light-Duty Fuel Economy : Stock Average",
        "Light-Duty Fuel Economy : Cars : Stock Average"
      ) ~ "LDV",
      seriesName %in% c("Freight : Railroads : Fuel Efficiency") ~ "FRAIL",
      seriesName %in% c("Freight : Domestic Shipping : Fuel Efficiency") ~ "FSHIP",
      seriesName %in% c("Freight : Truck Stock : Fuel Efficiency : Heavy : Average") ~ "HDT",
      seriesName %in% c("Freight : Truck Stock : Fuel Efficiency : Medium : Average") ~ "MDT"
    ),
    var = case_when(
      seriesName %in% c("Light-Duty Fuel Economy : Stock Average") ~ "SIMPG",
      seriesName %in% c("Light-Duty Fuel Economy : Alternative-Fuel Cars : 200-Mile Electric Vehicle") ~ "BEVElec",
      # seriesName == "Light-Duty Fuel Economy : Stock Average" ~ "Light-duty MPG",
      seriesName %in% c("Light-Duty Fuel Economy : Conventional Light Trucks : TDI Diesel") ~ "CIMPG",
      seriesName %in% c("Light-Duty Fuel Economy : Alternative-Fuel Cars : Electric-Gasoline Hybrid") ~ "HEVMPG",
      seriesName %in% c("Light-Duty Fuel Economy : Alternative-Fuel Cars : Plug-in 50 Gasoline Hybrid") ~ "PHEVMPG",
      aeo_mode %in% c("FRAIL", "FSHIP", "HDT", "MDT") ~ "MPG"
    ),
    value = as.numeric(value)
  ) %>%
  filter(
    period %in% unique(factor_values$aeo$year),
    !is.na(var)
  ) %>%
  unique()


# what data exist from AEO datasets in transportation_data

# transportation_data$passenger %>%
#   filter(stringr::str_detect(var, "MPG"),
#          mode == "PLDV") %>%
#   # are all the individual city values the same? Yes
#   select(mode, var, year, value) %>%
#   unique()


mpg_ref <- aeo_fuel_economy %>%
  filter(
    aeo_scen == "REF"
  ) %>%
  rename(value.ref = value) %>%
  select(period, var, aeo_mode, value.ref)

mpg_change <- aeo_fuel_economy %>%
  # filter(var == "SIMPG") %>%
  group_by(scenario, name, aeo_scen, aeo_mode, var) %>%
  arrange(scenario, name, aeo_scen, aeo_mode, var, period) %>%
  left_join(mpg_ref) %>%
  mutate(
    ref_pct_change = (value - value.ref) / value.ref,
    one_min_ref = 1 + ref_pct_change
  )
# mutate(var == "MPG")


# VMT  -----
# we need VMT for PLDV, medium, heavy duty trucks , freight rail
# freight shipping, buses, and rail
# To double check values, I compared with the original
# Transportation_Tool_Input_Development_2021.xlsx, aeo_scenario tab
aeo_vmt <- eia_data(paste0("aeo/", aeo_year),
  freq = "annual",
  facets = list(
    history = list(
      "PROJECTION",
      "HISTORIC"
    ),
    scenario = aeo_scenarios,
    seriesId = list(
      "eci_vmt_NA_flc_NA_NA_NA_blnmls",
      "kei_trv_trn_NA_bst_NA_NA_bpm",
      "eci_ftm_trn_dmt_NA_NA_NA_bln",
      "eci_ftm_trn_rail_NA_NA_NA_bln",
      "eci_vmt_fght_rads_NA_NA_NA_blnmls",
      "eci_vmt_fght_sosos_NA_NA_NA_blnmls",
      "kei_trv_trn_NA_rlp_NA_NA_bpm"
    )
  ),
  data = "value"
) %>%
  left_join(eia_scenario_codes, by = join_by(scenario == eia_code)) %>%
  mutate(
    var = "VMT",
    value = as.numeric(value),
    aeo_mode = case_when(
      seriesName == "Freight : Truck Stock : Vehicle Miles Traveled : Medium" ~ "MDT",
      seriesName == "Freight : Truck Stock : Vehicle Miles Traveled : Heavy" ~ "HDT",
      seriesName == "Fleet Vehicle Miles Traveled : Cars : Total" ~ "LDV",
      seriesName == "Transportation : Travel Indicators : Passenger Rail" ~ "RAIL",
      seriesName == "Freight : Railroads : Ton Miles by Rail" ~ "FRAIL",
      seriesName == "Freight : Domestic Shipping : Ton Miles Shipping" ~ "FSHIP",
      seriesName == "Transportation : Travel Indicators : Bus" ~ "BUS"
    )
  )


aeo_vmt_ref <- aeo_vmt %>%
  filter(aeo_scen == "REF") %>%
  select(period, aeo_scen, scenario, value.ref = value, var, aeo_mode, unit, seriesName) %>%
  filter(period %in% unique(factor_values$aeo$year))


vmt_change <- aeo_vmt %>%
  # group_by(scenario, name, aeo_scen, var) %>%
  arrange(scenario, name, var, aeo_mode, period) %>%
  filter(period %in% unique(factor_values$aeo$year)) %>%
  left_join(aeo_vmt_ref %>%
    select(period, var, aeo_mode, value.ref)) %>%
  mutate(
    ref_pct_change = (value - value.ref) / value.ref,
    one_min_ref = 1 + ref_pct_change
  )
