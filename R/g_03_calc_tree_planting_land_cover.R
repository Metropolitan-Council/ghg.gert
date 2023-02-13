#' @title Calculate tree planting land cover by city/township
#' @family land use
#'
#' @description Recalculates the hectares of land by
#'     land cover type by city/township under a tree planting scenario.
#'
#' @param .tree_panting_intervention character, specifies the type of tree planting.
#'     intervention to be explored under the current scenario. options are:
#'     * `"tree_planting_on_all_pervious"`: Assumes that all pervious surfaces are converted to tree canopy.
#'     * `"double"`: Assumes double the tree canopy relative.
#'         to the baseline year.
#'     * `"match_la_million_trees_goal"`: Matches the equivalent tree canopy to
#'         Los Angeles Million Tree Goal.
#'     Default is `tree_planting_on_all_pervious`.
#' @param .tree_planting_per_capita numeric,
#'      Tree planting per capita factor from the "Los Angeles 1,000,000 Trees" scenario.
#'      Default is `0.26`.
#' @param .tree_planting_per_hectare numeric,
#'      Tree planting per hectare factor from the "Los Angeles 1,000,000 Trees" scenario.
#'      Default is `247`.
#' @param detail logical,
#'      If `TRUE`, returns a table with more detailed fields. Recommended
#'      for debugging.
#'      Default is `FALSE`.
#'
#' @return [tibble::tibble()].
#'      `calc_tree_planting_land_cover()` returns table with hectares of land by land cover type after a tree planting scenario
#'      for each city/township. Set argument `detail` to `TRUE` for a more detailed table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' calc_tree_planting_land_cover(
#'   tb = land_use_data,
#'   detail = FALSE,
#'   .selected_ctu = "all",
#'   .urban_form_scenario = "bau",
#'   .tree_planting_intervention = "match_la_million_trees_goal",
#'   .tree_planting_per_capita = 0.26,
#'   .tree_planting_per_hectare = 247
#' )
#' }
#'
calc_tree_planting_land_cover <- function(tb,
                                          detail,
                                          .selected_ctu = .selected_ctu,
                                          .urban_form_scenario,
                                          .tree_planting_intervention,
                                          .tree_planting_per_capita,
                                          .tree_planting_per_hectare) {
  cli::cli_progress_message("*** calculating tree planting strategy \n")

  # -------------------------------------------------------------------------
  land_cover_by_city <- calc_land_cover_by_land_use(
    tb = tb,
    .selected_ctu = .selected_ctu,
    .urban_form_scenario = .urban_form_scenario
  ) %>%
    group_by(ctu_name, year, land_cover_type) %>%
    summarise(land_cover_hectares = sum(land_cover_land_use_hectares))


  # -------------------------------------------------------------------------
  match.arg(
    arg = .tree_planting_intervention,
    choices = c(
      "tree_planting_on_all_pervious",
      "double",
      "match_la_million_trees_goal"
    )
  )

  # -------------------------------------------------------------------------
  total_plantable_area <-
    land_cover_by_city %>%
    dplyr::group_by(ctu_name, year) %>%
    tidyr::pivot_wider(names_from = land_cover_type, values_from = land_cover_hectares) %>%
    dplyr::mutate(
      plantable_area_hectares = (grass + barren + shrub + grassland + agriculture),
      total_trees_hectares = (trees + forest + woody_wetland),
      total_area_hectares = (
        grass + barren + shrub + grassland
          + agriculture + trees + forest + woody_wetland
          + impervious + water + wetland
      )
    ) %>%
    dplyr::ungroup()

  # -------------------------------------------------------------------------
  tree_planting_factors <-
    tb$ctu_forecast %>%
    dplyr::filter(var == "population") %>%
    dplyr::filter(year == 2040) %>%
    dplyr::select(-c(year, var)) %>%
    dplyr::rename(population = value) %>%
    dplyr::full_join(
      land_cover_by_city %>%
        dplyr::filter(
          land_cover_type == "trees",
          year == 2040
        ) %>%
        # to check: are you aware that Brooklyn Center has NAs for tree cover?
        # land_cover_by_city %>% filter(ctu_name == "Brooklyn Center", land_cover_type == "trees" )
        dplyr::select(-c(land_cover_type)),
      by = "ctu_name"
    ) %>%
    # total tree canopy hectares
    dplyr::rename(total_tree_canopy_hectares = land_cover_hectares) %>%
    # LA goal hectares
    dplyr::mutate(
      LA_goal_hectares = (population * .tree_planting_per_capita)
      / .tree_planting_per_hectare
    ) %>%
    # pervious surface hectares
    dplyr::right_join(
      (
        total_plantable_area %>%
          dplyr::filter(year == 2040) %>%
          dplyr::select(-c(year)) %>%
          dplyr::rename(
            pervious_surface_hectares =
              plantable_area_hectares
          )
      ),
      by = "ctu_name"
    ) %>%
    dplyr::ungroup()

  # -------------------------------------------------------------------------
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
    ) %>%
    dplyr::ungroup()

  # -------------------------------------------------------------------------
  tree_planting_land_cover <-
    total_plantable_area %>%
    dplyr::right_join(.,
      tree_planting_scenario,
      by = "ctu_name"
    ) %>%
    dplyr::mutate(
      max_trees = total_area_hectares - woody_wetland - forest - impervious - wetland,
      trees =
        dplyr::if_else(year == 2016,
          trees,
          (if (.tree_planting_intervention == "tree_planting_on_all_pervious") {
            dplyr::if_else(
              # need to add choice of main parameter
              trees * tree_planting_on_all_pervious_sufaces_percent < max_trees,
              trees * tree_planting_on_all_pervious_sufaces_percent,
              max_trees
            )
          } else if (.tree_planting_intervention == "match_la_million_trees_goal") {
            dplyr::if_else(
              # need to add choice of main parameter
              trees * match_los_angeles_million_trees_plan_percent < max_trees,
              trees * match_los_angeles_million_trees_plan_percent,
              max_trees
            )
          } else if (.tree_planting_intervention == "double") {
            dplyr::if_else(
              trees * match_los_angeles_million_trees_plan_percent < max_trees,
              trees * match_los_angeles_million_trees_plan_percent,
              max_trees
            )
          })
        )
    ) %>%
    dplyr::mutate(total_scenario_tree = trees + forest + woody_wetland) %>%
    dplyr::mutate(total_bau_tree = total_trees_hectares) %>%
    dplyr::mutate(increased_tree = total_scenario_tree - total_bau_tree) %>%
    dplyr::mutate(
      scaling_factor =
        dplyr::if_else(
          increased_tree > 0,
          (plantable_area_hectares - increased_tree) / plantable_area_hectares,
          1
        )
    ) %>%
    dplyr::mutate(dplyr::across(
      .cols = c(grass, water, barren, shrub, grassland, agriculture),
      ~ dplyr::if_else(
        year == 2016,
        .x,
        dplyr::if_else(.x * scaling_factor < 1, 0, .x * scaling_factor)
      )
    )) %>%
    dplyr::ungroup()


  # -------------------------------------------------------------------------
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
    ) %>%
    dplyr::ungroup()

  # -------------------------------------------------------------------------
  return(if (detail == TRUE) {
    tree_planting_land_cover
  } else {
    tree_planting_land_cover_short
  })
}
