#' Calculate methane emissions from municipal wastewater
#'
#' @param population single value or multiple, values of population from which to scale emissions
#' @param years single value or multiple, years by which to look up EPA constants
#' @param lookup data frame, lookup table for EPA wastewater constants (default: waste_data$epa$wastewater_constants)
#'
#'
#'
#' @export
#' @importFrom lubridate leap_year
#'
#'
calculate_mww_ch4_emissions <- function(population, years, lookup = NULL) {
  if (is.null(lookup)) {
    lookup <- ghg.gert::waste_data$epa$wastewater_constants
  }

  # Pre-calculate days per year for all years
  days_per_year <- ifelse(lubridate::leap_year(years), 366, 365)

  get_epa_wastewater_constant <- function(variable_name) {
    lookup %>%
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
