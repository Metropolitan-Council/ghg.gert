#' Calculate direct nitrous oxide emissions from municipal wastewater
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
calculate_mww_n2o_direct_emissions <- function(population, years, lookup = waste_data$epa$wastewater_constants) {

  get_epa_wastewater_constant <- function(variable_name) {
    lookup %>%
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
