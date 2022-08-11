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
#'   .avoided_emissions_tractor_use_mg_co2e_per_hectare = 0.0102,
#'   .tree_planting_per_capita = 0.26,
#'   .tree_planting_per_hectare = 247,
#'   .parking_lot_reduction_percentage = 0.8,
#'   detail = FALSE
#' )
#' }
run_scenario_land_use <- function(tb = land_use_data,
                                  .urban_form_scenario = "bau",
                                  .conservation_tillage_intervention = "current_conservation_tillage",
                                  .tree_planting_intervention = "match_la_million_trees_goal",
                                  .tree_planting_per_capita = 0.26,
                                  .tree_planting_per_hectare = 247,
                                  .parking_lot_reduction_percentage = 0.8,
                                  detail = FALSE) {
  check_inputs("parking_lot_reduction_percentage",
               .parking_lot_reduction_percentage)

  land_use <- scen_green_infrastructure(
    tb = tb,
    detail = detail,
    .urban_form_scenario = .urban_form_scenario,
    .conservation_tillage_intervention = .conservation_tillage_intervention,
    .tree_planting_intervention = .tree_planting_intervention,
    .tree_planting_per_capita = .tree_planting_per_capita,
    .tree_planting_per_hectare = .tree_planting_per_hectare,
    .parking_lot_reduction_percentage = .parking_lot_reduction_percentage
  )

  land_use_module_output <-
    land_use %>%
    dplyr::group_by(ctu_name,
                    year,
                    detail,
                    urban_form_scenario,
                    tree_planting_intervention,
                    conservation_tillage_intervention,
                    parking_lot_reduction_percentage,
    ) %>%
    dplyr::summarise(value = sum(value), .groups = 'drop')

  return(land_use_module_output)
}
