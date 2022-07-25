#' @title Run land use scenario
#' @family land use
#'
#' @inheritParams scen_green_infrastructure
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' run_scenario_land_use(
#'   tb = land_use_data,
#'   .urban_form_scenario = "bau",
#'   .conservation_tillage_intervention = "current_conservation_tillage",
#'   .tree_planting_intervention = "tree_planting_on_all_pervious",
#'   .w2w_diesel_emission_factor_kg_co2e_per_gal = 12.50,
#'   .avoided_emissions_tractor_use_mg_co2e_per_hectare = 0.0102,
#'   .agricultural_land_carbon_stock_mg_c_per_hectare = 3,
#'   .maximum_soc_accumation_under_reduced_or_no_till_ag = 1.54,
#'   .tree_planting_per_capita = 0.26,
#'   .tree_planting_per_hectare = 247,
#'   .parking_lot_reduction_percentage = 0.8,
#'   detail = FALSE
#' )
#' }
run_scenario_land_use <- function(tb = land_use_data,
                                  .urban_form_scenario = "bau",
                                  .conservation_tillage_intervention = "current_conservation_tillage",
                                  .tree_planting_intervention = "tree_planting_on_all_pervious",
                                  .w2w_diesel_emission_factor_kg_co2e_per_gal = 12.50,
                                  .avoided_emissions_tractor_use_mg_co2e_per_hectare = 0.0102,
                                  .agricultural_land_carbon_stock_mg_c_per_hectare = 3,
                                  .maximum_soc_accumation_under_reduced_or_no_till_ag = 1.54,
                                  .tree_planting_per_capita = 0.26,
                                  .tree_planting_per_hectare = 247,
                                  .parking_lot_reduction_percentage = 0.8,
                                  detail = FALSE) {
  check_inputs("parking_lot_reduction_percentage", .parking_lot_reduction_percentage)

  browser()

  land_use <- scen_green_infrastructure(
    tb = tb,
    detail = detail,
    .urban_form_scenario = .urban_form_scenario,
    .conservation_tillage_intervention = .conservation_tillage_intervention,
    .agricultural_land_carbon_stock_mg_c_per_hectare = .agricultural_land_carbon_stock_mg_c_per_hectare,
    .w2w_diesel_emission_factor_kg_co2e_per_gal = .w2w_diesel_emission_factor_kg_co2e_per_gal,
    .avoided_emissions_tractor_use_mg_co2e_per_hectare = .avoided_emissions_tractor_use_mg_co2e_per_hectare,
    .maximum_soc_accumation_under_reduced_or_no_till_ag = .maximum_soc_accumation_under_reduced_or_no_till_ag,
    .tree_planting_intervention = .tree_planting_intervention,
    .tree_planting_per_capita = .tree_planting_per_capita,
    .tree_planting_per_hectare = .tree_planting_per_hectare,
    .parking_lot_reduction_percentage = .parking_lot_reduction_percentage
  )

  land_use_module_output <-
    land_use

  return(land_use_module_output)
}
