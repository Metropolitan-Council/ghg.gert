#' Calculate Tree Planting Land Cover by City/Township
#'
#' @family land_use_module
#'
#' @description Recalculates the hectares land by land cover type
#' by community under a tree planting scenario
#'
#' @param tree_panting_intervention this argument specificies the type of tree planting
#' intervention to be explored under the current scenario.
#'     The options are 'tree_planting_on_all_pervious', 'double', or 'match_la_million_trees_goal'
#'     The default is 'tree_planting_on_all_pervious'
#'
#' @return a table with hectares of land by land cover type after a tree planting scenario
#' for each city/township
#'
#' @export
#'
#' @examples calc_tree_planting_land_cover()
calc_tree_planting_land_cover <-
  function(tree_planting_intervention = "tree_planting_on_all_pervious") {

    scaling <- function(.data, group_col) {
      .data %>%
        dplyr::mutate(.,  group_col = dplyr::if_else(
                           year == 2016,
                           {{ group_col }},
                           dplyr::if_else(
                             {{ group_col }} * scaling_factor < 1,
                             0,
                             {{ group_col }} * scaling_factor)
                         ))
    }

    calc_tree_planting_land_cover <-
      calc_total_plantable_area() %>%
      right_join(.,
                 calc_tree_planting_scenario(),
                 by = "ctu_name") %>%
      mutate(
        max_trees = total_area_hectares - woody_wetland - forest - impervious - wetland,
        trees =
          dplyr::if_else(year == 2016,
                         trees,
                         (if (tree_planting_intervention == "tree_planting_on_all_pervious") {
                           dplyr::if_else(
                             #need to add choice of main parameter
                             trees * tree_planting_on_all_pervious_sufaces_percent < max_trees,
                             trees * tree_planting_on_all_pervious_sufaces_percent,
                             max_trees
                           )
                         } else if (tree_planting_intervention == "match_la_million_trees_goal") {
                           dplyr::if_else(
                             #need to add choice of main parameter
                             trees * match_los_angeles_million_trees_plan_percent < max_trees,
                             trees * match_los_angeles_million_trees_plan_percent,
                             max_trees
                           )
                         } else if (tree_planting_intervention == "double") {
                           dplyr::if_else(
                             trees * match_los_angeles_million_trees_plan_percent < max_trees,
                             trees * match_los_angeles_million_trees_plan_percent,
                             max_trees
                           )
                         }))
      ) %>%
      dplyr::mutate(total_scenario_tree = trees + forest + woody_wetland) %>%
      dplyr::mutate(total_bau_tree = total_trees_hectares) %>%
      dplyr::mutate(increased_tree = total_scenario_tree - total_bau_tree) %>%
      dplyr::mutate(scaling_factor =
                      dplyr::if_else(
                        increased_tree > 0,
                        (plantable_area_hectares  - increased_tree) / plantable_area_hectares ,
                        1
                      )) %>%
      dplyr::mutate(dplyr::across(
        .cols = c(grass, water, barren, shrub, grassland, agriculture),
        ~ if_else(
          year == 2016,
          .x,
          if_else(.x * scaling_factor < 1, 0, .x * scaling_factor)
        )
      )) %>%

      # dplyr::mutate(grass = if_else(
      #   year == 2016,
      #   grass,
      #   if_else(grass * scaling_factor < 1, 0, grass * scaling_factor)
      # )) %>%
      # dplyr::mutate(water = if_else(
      #   year == 2016,
      #   water,
      #   if_else(water * scaling_factor < 1, 0, water * scaling_factor)
      # )) %>%
      # dplyr::mutate(barren = if_else(
      #   year == 2016,
      #   barren,
      #   if_else(barren * scaling_factor < 1, 0, barren * scaling_factor)
      # )) %>%
      # dplyr::mutate(shrub = if_else(
      #   year == 2016,
      #   shrub,
      #   if_else(shrub * scaling_factor < 1, 0, shrub * scaling_factor)
      # )) %>%
      # dplyr::mutate(grassland = if_else(
      #   year == 2016,
      #   grassland,
      #   if_else(grassland * scaling_factor < 1, 0, grassland * scaling_factor)
      # )) %>%
      # dplyr::mutate(agriculture = if_else(
      #   year == 2016,
      #   agriculture,
      #   if_else(
      #     agriculture * scaling_factor < 1,
      #     0,
      #     agriculture * scaling_factor
      #   )
      # )) %>%
      select(
        ctu_name,
        year,
        agriculture,
        barren,
        forest,
        grass,
        grassland,
        impervious,
        parking_lot,
        shrub,
        trees,
        water,
        wetland,
        woody_wetland,
        total_area_hectares
      )

    return(calc_tree_planting_land_cover)

  }
