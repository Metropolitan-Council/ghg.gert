#' Title
#'
#' @return
#' @export
#'
#' @examples
calc_tree_planting_scenario <- function() {
  tree_planting_scenario <-
    calc_tree_planting_factors() %>%
    group_by(ctu_name) %>%
    transmute(
      match_los_angeles_million_trees_plan_percent =
        (
          LA_goal_hectares + total_tree_canopy_hectares
        ) /
        total_tree_canopy_hectares,
      tree_planting_on_all_pervious_sufaces =
        (
          pervious_surface_hectares + total_tree_canopy_hectares
        ) /
        total_tree_canopy_hectares,
    )

}
