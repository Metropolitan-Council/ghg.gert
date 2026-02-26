#' Calculate Manure Management Emissions
#'
#' @param livestock_df Data frame with columns: county_name, year, livestock_type, head_count
#' @param agriculture_variables List containing: mcf, vs, manure_state, nex, Bo
#' @param ag_constants_vec Named vector of constants
#' @param gwp_list List with ch4 and n2o global warming potentials
#'
#' @return Data frame with emissions by county, year, storage_state, and gas type
#'
#'
calculate_manure_emissions <- function(manure_caf = ghg.ccap::agriculture_manure_caf,
                                       .selected_ctu = .selected_ctu) {

  #browser()
  emissions_output <- filter_ctu(manure_caf, .selected_ctu = .selected_ctu) %>%
    group_by(inventory_year, geog_name, county_name, scenario) %>%
    summarize(value_emissions = sum(value_emissions)) %>%
    ungroup() %>%
    mutate(category = "Livestock",
           sector = "Agriculture",
           source = "Manure")

  return(emissions_output)
}


