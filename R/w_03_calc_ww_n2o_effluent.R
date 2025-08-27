#' Calculate effluent nitrous oxide emissions from municipal wastewater
#'
#' @param population single value or multiple, values of population from which to scale emissions
#' @param years single value or multiple, years by which to look up EPA constants
#' @param lookup data frame, lookup table for EPA wastewater constants (default: waste_data$epa$wastewater_constants)
#' @param epa_protein_consumption data frame, EPA protein consumption data (default: waste_data$epa$protein_consumption)
#'
#'
#'
#' @export
#'
#'
calculate_mww_n2o_effluent_emissions <- function(population, years,
                                                 lookup = NULL,
                                                 epa_protein_consumption = NULL) {



  if (is.null(lookup)) {
    lookup <- ghg.ccap::waste_data$epa$wastewater_constants
  }

  if (is.null(epa_protein_consumption)) {
    epa_protein_consumption <- ghg.ccap::waste_data$epa$protein_consumption
  }


  get_epa_wastewater_constant <- function(variable_name) {
    lookup %>%
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
