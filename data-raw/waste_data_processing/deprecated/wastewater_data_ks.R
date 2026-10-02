rm(list = ls())


# 1. Load CTU and county boundaries ---------------------------------------

# Set your reference year
# This is the last year of inventory data, and the year before projections
ref_year <- 2022

# Load geographic index data and demographic_data, both part of ghg.ccap
# (only contains 7 counties)
lookup_ctu_county <- rbind(
  geog_index %>%
    dplyr::select(geog_name, geog_level, geog_id, geog_id_type),
  demographic_data %>% filter(geog_level == "COUNTY") %>%
    filter(inventory_year == ref_year & sp_categories == "population") %>%
    dplyr::select(geog_name, geog_level, geog_id, geog_id_type)
) %>% arrange(geog_name)



# Return CTU population percentages by county (for dealing with CTUs that cross county boundaries)
ctu_pop_by_county <-
  ctu_county %>% # Load ctu_county - includes information about CTUs that span multiple counties
  dplyr::select(-pct_land_area) %>%
  mutate(
    ctu_id = stringr::str_pad(ctu_id, width = 8, pad = "0"),
    co_code = paste0("27", stringr::str_pad(co_code, width = 3, pad = "0"))
  ) %>%
  # NOTE: Empire (ctu_id = "02831011") was not in the ctu_county dataframe,
  # so it needs to be added here. Not sure why it was missing...
  bind_rows(tibble(ctu_id = "02831011", co_code = "27037", n_counties = 1, pct_population = 1)) %>%
  # add county metadata
  left_join(lookup_ctu_county %>%
    dplyr::select(geog_id, "co_name" = geog_name), by = join_by(co_code == geog_id)) %>%
  # add ctu metadata
  left_join(lookup_ctu_county %>%
    dplyr::select(geog_id, "ctu_name" = geog_name), by = join_by(ctu_id == geog_id)) %>%
  dplyr::select(ctu_id, co_code, ctu_name, co_name, everything()) %>%
  filter(pct_population != 0) %>% # remove rows where the population is zero
  # after removing the rows with zero population, re-classify the CTUs that span
  # multiple counties geographically but have 100% of their population within a
  # single county's boundary.
  mutate(n_counties = case_when(
    pct_population == 1 & n_counties > 1 ~ 1,
    .default = n_counties
  ))


# 2. Return CTUs with multiple counties ------------------------------------

# In some cases, a CTU will span two counties in terms of geographic area, but
# 100% of the population will be localized in one county and not the other.
# Return a dataframe of only the CTUs that both (1) span two counties and (2)
# contain population in both counties.
duplicate_ctus <- ctu_pop_by_county %>% filter(n_counties > 1)



# 3. Load demographic data for 2005 to 2050 -------------------------------

# Next we need to bring in population data
# these are population estimates for counties and CTUs from 2005 to 2050.
# - by county
county_population_data <- demographic_data %>%
  dplyr::filter(sp_categories == "population" & geog_level == "COUNTY") %>%
  rename(geog_pop = value)

# - by CTU
ctu_population_data <- demographic_data %>%
  dplyr::filter(sp_categories == "population" & geog_level != "COUNTY") %>%
  rename(geog_pop = value) %>%
  # Tag your ctu dataframe for cases where the population spans more
  # than one county. This will affect which per capita estimates we use for solid
  # waste source.
  mutate(flag = case_when(
    geog_id %in% duplicate_ctus$ctu_id ~ "duplicate",
    .default = NA
  ))





# 4. Load wastewater functions and constants ----------------------
epa_wastewater_constants <- readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_waste/data-raw/wastewater/epa/epa_wastewater_constants.rds")
epa_protein_consumption <- readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_waste/data-raw/wastewater/epa/epa_protein_consumption.rds")


# Vectorized municipal wastewater methane emissions
calculate_mww_ch4_emissions <- function(population, years) {
  # Pre-calculate days per year for all years
  days_per_year <- ifelse(lubridate::leap_year(years), 366, 365)

  get_epa_wastewater_constant <- function(variable_name) {
    epa_wastewater_constants %>%
      filter(short_text == variable_name) %>%
      pull(value)
  }

  # Grab constants (these are the same for all years)
  per_capita_BOD5 <- get_epa_wastewater_constant("Per_capita_BOD5")
  MT_per_kg <- get_epa_wastewater_constant("MT_per_kg")
  Emission_Factor_CH4_BOD5 <- get_epa_wastewater_constant("Emission_Factor_CH4_BOD5")
  Fraction_BOD5_anaerobically_digested <- get_epa_wastewater_constant("Fraction_BOD5_anaerobically_digested")
  CH4_GWP <- get_epa_wastewater_constant("CH4_GWP")
  MMT_per_MT <- get_epa_wastewater_constant("MMT_per_MT")

  # Handle different input scenarios
  if (length(population) == 1 && length(years) > 1) {
    # Single population, multiple years
    pop_vec <- rep(population, length(years))
    years_vec <- years
    days_vec <- days_per_year
  } else if (length(population) > 1 && length(years) == 1) {
    # Multiple populations, single year
    pop_vec <- population
    years_vec <- rep(years, length(population))
    days_vec <- rep(days_per_year, length(population))
  } else if (length(population) == length(years)) {
    # Equal length vectors
    pop_vec <- population
    years_vec <- years
    days_vec <- days_per_year
  } else {
    stop("Length of population and years must be equal, or one must be length 1")
  }

  # Vectorized calculation
  emissions_metric_tons_CH4 <- pop_vec * per_capita_BOD5 * days_vec * MT_per_kg *
    Emission_Factor_CH4_BOD5 * Fraction_BOD5_anaerobically_digested

  # Create vectorized data frame
  df <- data.frame(
    sector = "Waste",
    category = "Wastewater",
    source = "Municipal_CH4",
    data_source = "EPA State Inventory Tool - Wastewater Module",
    population = pop_vec,
    inventory_year = years_vec,
    value_emissions = emissions_metric_tons_CH4,
    units_emissions = "Metric tons CH4",
    stringsAsFactors = FALSE
  )

  return(df)
}

# Vectorized municipal wastewater nitrous oxide direct emissions
calculate_mww_n2o_direct_emissions <- function(population, years) {
  # Pre-calculate days per year for all years
  days_per_year <- ifelse(lubridate::leap_year(years), 366, 365)

  get_epa_wastewater_constant <- function(variable_name) {
    epa_wastewater_constants %>%
      filter(short_text == variable_name) %>%
      pull(value)
  }

  # Grab constants
  Fraction_population_not_on_septic <- get_epa_wastewater_constant("Fraction_population_not_on_septic")
  Direct_wwtp_emissions <- get_epa_wastewater_constant("Direct_wwtp_emissions")
  g_per_MT <- get_epa_wastewater_constant("g_per_MT")
  N2O_GWP <- get_epa_wastewater_constant("N2O_GWP")
  MMT_per_MT <- get_epa_wastewater_constant("MMT_per_MT")

  # Handle different input scenarios
  if (length(population) == 1 && length(years) > 1) {
    pop_vec <- rep(population, length(years))
    years_vec <- years
  } else if (length(population) > 1 && length(years) == 1) {
    pop_vec <- population
    years_vec <- rep(years, length(population))
  } else if (length(population) == length(years)) {
    pop_vec <- population
    years_vec <- years
  } else {
    stop("Length of population and years must be equal, or one must be length 1")
  }

  # Vectorized calculation
  emissions_metric_tons_N2O <- pop_vec * Fraction_population_not_on_septic *
    Direct_wwtp_emissions * g_per_MT

  # Create vectorized data frame
  df <- data.frame(
    sector = "Waste",
    category = "Wastewater",
    source = "Municipal_N2O_direct",
    data_source = "EPA State Inventory Tool - Wastewater Module",
    population = pop_vec,
    inventory_year = years_vec,
    value_emissions = emissions_metric_tons_N2O,
    units_emissions = "Metric tons N2O",
    stringsAsFactors = FALSE
  )

  return(df)
}

# Vectorized municipal wastewater nitrous oxide effluent emissions
calculate_mww_n2o_effluent_emissions <- function(population, years) {
  get_epa_wastewater_constant <- function(variable_name) {
    epa_wastewater_constants %>%
      filter(short_text == variable_name) %>%
      pull(value)
  }

  # Handle different input scenarios
  if (length(population) == 1 && length(years) > 1) {
    pop_vec <- rep(population, length(years))
    years_vec <- years
  } else if (length(population) > 1 && length(years) == 1) {
    pop_vec <- population
    years_vec <- rep(years, length(population))
  } else if (length(population) == length(years)) {
    pop_vec <- population
    years_vec <- years
  } else {
    stop("Length of population and years must be equal, or one must be length 1")
  }

  # Vectorized lookup for protein consumption and biosolids percentage
  protein_consumption_vec <- numeric(length(years_vec))
  biosolids_pct_vec <- numeric(length(years_vec))

  # Get the range of available years
  available_years <- as.numeric(epa_protein_consumption$year)
  min_year <- min(available_years)
  max_year <- max(available_years)

  for (i in seq_along(years_vec)) {
    year <- years_vec[i]

    if (year %in% available_years) {
      # Year is available in data
      protein_consumption_vec[i] <- epa_protein_consumption %>%
        filter(year == !!year) %>%
        pull(Protein_kg_per_person_per_year)

      biosolids_pct_vec[i] <- epa_protein_consumption %>%
        filter(year == !!year) %>%
        pull(pct_of_biosolids_as_fertilizer)
    } else if (year > max_year) {
      # Use the most recent year's data for extrapolation
      protein_consumption_vec[i] <- epa_protein_consumption %>%
        filter(year == max_year) %>%
        pull(Protein_kg_per_person_per_year)

      biosolids_pct_vec[i] <- epa_protein_consumption %>%
        filter(year == max_year) %>%
        pull(pct_of_biosolids_as_fertilizer)
    } else {
      # Use the earliest year's data for extrapolation
      protein_consumption_vec[i] <- epa_protein_consumption %>%
        filter(year == min_year) %>%
        pull(Protein_kg_per_person_per_year)

      biosolids_pct_vec[i] <- epa_protein_consumption %>%
        filter(year == min_year) %>%
        pull(pct_of_biosolids_as_fertilizer)
    }
  }

  # Get constants
  Fraction_nitrogen_in_protein <- get_epa_wastewater_constant("Fraction_nitrogen_in_protein")
  Factor_non_consumption_nitrogen <- get_epa_wastewater_constant("Factor_non_consumption_nitrogen")
  MT_per_kg <- get_epa_wastewater_constant("MT_per_kg")
  N2O_N_MWR <- get_epa_wastewater_constant("N2O_N_MWR")
  N2O_GWP <- get_epa_wastewater_constant("N2O_GWP")
  Emission_Factor_N2O_N <- get_epa_wastewater_constant("Emission_Factor_N2O_N")
  MMT_per_MT <- get_epa_wastewater_constant("MMT_per_MT")

  # Vectorized calculations
  N_in_domestic_wastewater <- pop_vec * protein_consumption_vec *
    Fraction_nitrogen_in_protein * Factor_non_consumption_nitrogen * MT_per_kg

  # Calculate direct N2O emissions vectorized
  N2O_direct_emissions_df <- calculate_mww_n2o_direct_emissions(pop_vec, years_vec)
  N2O_direct_emissions <- N2O_direct_emissions_df$value_emissions * (1 / N2O_N_MWR)

  Biosolids_avail_N_MT <- N_in_domestic_wastewater - N2O_direct_emissions

  emissions_metric_tons_N2O <- Biosolids_avail_N_MT * (1 - biosolids_pct_vec) *
    Emission_Factor_N2O_N * N2O_N_MWR

  # Create vectorized data frame
  df <- data.frame(
    sector = "Waste",
    category = "Wastewater",
    source = "Municipal_N2O_effluent",
    data_source = "EPA State Inventory Tool - Wastewater Module",
    population = pop_vec,
    inventory_year = years_vec,
    value_emissions = emissions_metric_tons_N2O,
    units_emissions = "Metric tons N2O",
    stringsAsFactors = FALSE
  )

  return(df)
}


# ## Example usage:
# population <- 1000000
# years <- 2022
# emissions_CH4 <- calculate_mww_ch4_emissions(population, years=c(2022,2023,2024, 2025, 2027:2030))
# emissions_N2O_direct <- calculate_mww_n2o_direct_emissions(population, years=years)
# emissions_N2O_effluent <- calculate_mww_n2o_effluent_emissions(population, years=years)

gwp <-
  list(
    "co2" = 1,
    "ch4" = 27.9,
    "n2o" = 273,
    "cf4" = 7380,
    "HFC-152a" = 164
  )



# 5. County level projections (2023 to 2050) ------------------------------
wastewater_proj_county <- county_population_data %>%
  dplyr::filter(inventory_year > ref_year) %>%
  group_by(geog_id) %>%
  mutate(
    MWW_CH4 = calculate_mww_ch4_emissions(population = geog_pop, years = inventory_year)$value_emissions,
    MWW_N20_direct = calculate_mww_n2o_direct_emissions(population = geog_pop, years = inventory_year)$value_emissions,
    MWW_N20_effluent = calculate_mww_n2o_effluent_emissions(population = geog_pop, years = inventory_year)$value_emissions
  )

wastewater_proj_county <- wastewater_proj_county %>%
  pivot_longer(
    cols = c(MWW_CH4, MWW_N20_direct, MWW_N20_effluent),
    names_to = "source",
    values_to = "value_emissions"
  ) %>%
  mutate(
    units_emissions = case_when(
      source == "MWW_CH4" ~ "Metric tons CH4",
      source == "MWW_N20_direct" ~ "Metric tons N2O",
      source == "MWW_N20_effluent" ~ "Metric tons N2O"
    ),
    mt_co2e =
      case_when(
        units_emissions == "Metric tons CH4" ~ value_emissions * gwp$ch4,
        units_emissions == "Metric tons N2O" ~ value_emissions * gwp$n2o
      )
  ) %>%
  group_by(
    inventory_year, geog_id, geog_name, geog_level, geog_pop
  ) %>%
  summarize(
    value_emissions = sum(mt_co2e),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  mutate(
    source = "Wastewater",
    units_emissions = "metric tons CO2e",
    data_type = "forecast using future population growth"
  ) %>%
  relocate(source, .before = "value_emissions")


# 6. County level baseline (2005 to 2022) ------------------------------
wastewater_baseline_county <- county_population_data %>%
  dplyr::filter(inventory_year >= 2005 & inventory_year <= ref_year) %>%
  group_by(geog_id) %>%
  mutate(
    MWW_CH4 = calculate_mww_ch4_emissions(population = geog_pop, years = inventory_year)$value_emissions,
    MWW_N20_direct = calculate_mww_n2o_direct_emissions(population = geog_pop, years = inventory_year)$value_emissions,
    MWW_N20_effluent = calculate_mww_n2o_effluent_emissions(population = geog_pop, years = inventory_year)$value_emissions
  )


wastewater_baseline_county <- wastewater_baseline_county %>%
  pivot_longer(
    cols = c(MWW_CH4, MWW_N20_direct, MWW_N20_effluent),
    names_to = "source",
    values_to = "value_emissions"
  ) %>%
  mutate(
    units_emissions = case_when(
      source == "MWW_CH4" ~ "Metric tons CH4",
      source == "MWW_N20_direct" ~ "Metric tons N2O",
      source == "MWW_N20_effluent" ~ "Metric tons N2O"
    ),
    mt_co2e =
      case_when(
        units_emissions == "Metric tons CH4" ~ value_emissions * gwp$ch4,
        units_emissions == "Metric tons N2O" ~ value_emissions * gwp$n2o
      )
  ) %>%
  group_by(
    inventory_year, geog_id, geog_name, geog_level, geog_pop
  ) %>%
  summarize(
    value_emissions = sum(mt_co2e),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  mutate(
    source = "Wastewater",
    units_emissions = "metric tons CO2e",
    data_type = "estimated using historical population data"
  ) %>%
  relocate(source, .before = "value_emissions")


# rbind(
#   wastewater_baseline_county,
#   wastewater_proj_county
# ) %>%
#   ggplot() +
#   geom_line(aes(x=inventory_year, y=value_emissions, color=source)) +
#   facet_wrap(~geog_name)


# 7. CTU level projections (2023 to 2050) ------------------------------
wastewater_proj_ctu <- ctu_population_data %>%
  dplyr::filter(inventory_year > ref_year) %>%
  group_by(geog_id) %>%
  mutate(
    MWW_CH4 = calculate_mww_ch4_emissions(population = geog_pop, years = inventory_year)$value_emissions,
    MWW_N20_direct = calculate_mww_n2o_direct_emissions(population = geog_pop, years = inventory_year)$value_emissions,
    MWW_N20_effluent = calculate_mww_n2o_effluent_emissions(population = geog_pop, years = inventory_year)$value_emissions
  )

wastewater_proj_ctu <- wastewater_proj_ctu %>%
  pivot_longer(
    cols = c(MWW_CH4, MWW_N20_direct, MWW_N20_effluent),
    names_to = "source",
    values_to = "value_emissions"
  ) %>%
  mutate(
    units_emissions = case_when(
      source == "MWW_CH4" ~ "Metric tons CH4",
      source == "MWW_N20_direct" ~ "Metric tons N2O",
      source == "MWW_N20_effluent" ~ "Metric tons N2O"
    ),
    mt_co2e =
      case_when(
        units_emissions == "Metric tons CH4" ~ value_emissions * gwp$ch4,
        units_emissions == "Metric tons N2O" ~ value_emissions * gwp$n2o
      )
  ) %>%
  group_by(
    inventory_year, geog_id, geog_name, geog_level, geog_pop
  ) %>%
  summarize(
    value_emissions = sum(mt_co2e),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  mutate(
    source = "Wastewater",
    units_emissions = "metric tons CO2e",
    data_type = "forecast using future population growth"
  ) %>%
  relocate(source, .before = "value_emissions") %>%
  arrange(geog_name, inventory_year)


# 8. CTU level baseline (2005 to 2022) ------------------------------
wastewater_baseline_ctu <- ctu_population_data %>%
  dplyr::filter(inventory_year >= 2005 & inventory_year <= ref_year) %>%
  group_by(geog_id) %>%
  mutate(
    MWW_CH4 = calculate_mww_ch4_emissions(population = geog_pop, years = inventory_year)$value_emissions,
    MWW_N20_direct = calculate_mww_n2o_direct_emissions(population = geog_pop, years = inventory_year)$value_emissions,
    MWW_N20_effluent = calculate_mww_n2o_effluent_emissions(population = geog_pop, years = inventory_year)$value_emissions
  )


wastewater_baseline_ctu <- wastewater_baseline_ctu %>%
  pivot_longer(
    cols = c(MWW_CH4, MWW_N20_direct, MWW_N20_effluent),
    names_to = "source",
    values_to = "value_emissions"
  ) %>%
  mutate(
    units_emissions = case_when(
      source == "MWW_CH4" ~ "Metric tons CH4",
      source == "MWW_N20_direct" ~ "Metric tons N2O",
      source == "MWW_N20_effluent" ~ "Metric tons N2O"
    ),
    mt_co2e =
      case_when(
        units_emissions == "Metric tons CH4" ~ value_emissions * gwp$ch4,
        units_emissions == "Metric tons N2O" ~ value_emissions * gwp$n2o
      )
  ) %>%
  group_by(
    inventory_year, geog_id, geog_name, geog_level, geog_pop
  ) %>%
  summarize(
    value_emissions = sum(mt_co2e),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  mutate(
    source = "Wastewater",
    units_emissions = "metric tons CO2e",
    data_type = "estimated using historical population data"
  ) %>%
  relocate(source, .before = "value_emissions")


# ?. Compile wastewater data ---------------------------------------------------
## Store data in a list
wastewater_data <- list()

# add inventory data
wastewater_data$inventory <- rbind(
  wastewater_baseline_ctu,
  wastewater_baseline_county
)

# add projections
wastewater_data$projections <- rbind(
  wastewater_proj_ctu,
  wastewater_proj_county
)


usethis::use_data(wastewater_data, overwrite = TRUE)
