#' Calculate Tree Planting Land Cover by City/Township
#'
#' @family land_use_module
#'
#' @description `calc_tree_planting_land_cover()` recalculates the hectares of land by
#' land cover type by city/township under a tree planting scenario.
#'
#' @details
#'
#' @param tree_panting_intervention String. Specifies the type of tree planting
#' intervention to be explored under the current scenario. The options are:
#' * `tree_planting_on_all_pervious` Assumes that all pervious surfaces are converted to tree canopy.
#' * `double` Assumes double the tree canopy relative.
#'    to the baseline year.
#' * `match_la_million_trees_goal` Matches the equivalent tree canopy to Los Angeles Million Tree Goal.
#'
#' @param detail Default is `FALSE`.
#' If true, returns a table with more detailed fields. Recommended
#' for debugging.
#'
#' @return Returns a `tibble`. A table with hectares of land by land cover type after a tree planting scenario
#' for each city/township. Set argumnet `detail` to `TRUE` for a more detailed table.
#'
#' @export
#'
#' @examples calc_tree_planting_land_cover()

calc_tree_planting_land_cover <-
  function(tb = land_use_data,
           tree_planting_intervention = "tree_planting_on_all_pervious",
           detail = FALSE,
           tree_planting_per_capita = 0.26,
           tree_planting_per_hectare = 247) {
    land_cover_by_city <- calc_land_cover_by_city()

    total_plantable_area <-
      land_cover_by_city %>%
      dplyr::group_by(ctu_name, year) %>%
      tidyr::pivot_wider(names_from = land_cover_description_2, values_from = land_cover_hectares) %>%
      dplyr::mutate(
        plantable_area_hectares = (grass +  barren + shrub + grassland + agriculture),
        total_trees_hectares = (trees + forest + woody_wetland),
        total_area_hectares = (
          grass +  barren + shrub + grassland
          + agriculture + trees + forest + woody_wetland
          + impervious + water + wetland
        )
      )

    tree_planting_factors <-
      tb$ctu_forecast %>%
      dplyr::filter(metric == "population") %>%
      dplyr::filter(year == 2040) %>%
      dplyr::select(-c(year, metric)) %>%
      dplyr::rename(population = value) %>%
      dplyr::full_join(
        land_cover_by_city %>%
          dplyr::filter(land_cover_description_2 == "trees",
                        year == 2040) %>%
          # to check: are you aware that Brooklyn Center has NAs for tree cover?
          # land_cover_by_city %>% filter(ctu_name == "Brooklyn Center", land_cover_description_2 == "trees" )
          dplyr::select(-c(year, land_cover_description_2))
        ,
        by = "ctu_name"
      ) %>%
      # total tree canopy hectares
      dplyr::rename(total_tree_canopy_hectares = land_cover_hectares) %>%
      # LA goal hectares
      dplyr::mutate(
        LA_goal_hectares = (population * tree_planting_per_capita)
        / tree_planting_per_hectare
      ) %>%
      # pervious surface hectares
      dplyr::right_join(
        (
          total_plantable_area %>%
            dplyr::filter(year == 2040) %>%
            dplyr::select(-c(year)) %>%
            dplyr::rename(pervious_surface_hectares =
                            plantable_area_hectares)
        ),
        by = "ctu_name"
      )

    tree_planting_scenario <-
      tree_planting_factors %>%
      dplyr::group_by(ctu_name) %>%
      dplyr::transmute(
        match_los_angeles_million_trees_plan_percent =
          (LA_goal_hectares +
             total_tree_canopy_hectares) /
          total_tree_canopy_hectares,
        tree_planting_on_all_pervious_sufaces_percent =
          (pervious_surface_hectares + total_tree_canopy_hectares) /
          total_tree_canopy_hectares,
      )

    tree_planting_land_cover <-
      total_plantable_area %>%
      dplyr::right_join(.,
                 tree_planting_scenario,
                 by = "ctu_name") %>%
      dplyr::mutate(
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
        ~ dplyr::if_else(
          year == 2016,
          .x,
          dplyr::if_else(.x * scaling_factor < 1, 0, .x * scaling_factor)
        )
      ))

    tree_planting_land_cover_short <-
      tree_planting_land_cover %>%
      dplyr::select(
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

    return(if (detail == TRUE) {
      tree_planting_land_cover
    }
    else{
      tree_planting_land_cover_short
    })

  }
