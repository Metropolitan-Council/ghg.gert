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
    dplyr::group_by(ctu_name, year, land_cover_type) %>%
    dplyr::summarise(land_cover_hectares = sum(land_cover_land_use_hectares))


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
  # Calculate the total plantable area based on land cover by city
  total_plantable_area <-
    # Start with the land_cover_by_city dataset
    land_cover_by_city %>%
    # Group the dataset by ctu_name (city) and year
    dplyr::group_by(ctu_name, year) %>%
    # Pivot the dataset to make land cover types as columns with their corresponding hectare values
    tidyr::pivot_wider(names_from = land_cover_type, values_from = land_cover_hectares) %>%
    # Create new variables for the dataset
    dplyr::mutate(
      # Calculate the plantable area in hectares by summing grass, barren, shrub, grassland, and agriculture
      plantable_area_hectares = (grass + barren + shrub + grassland + agriculture),
      # Calculate the total tree area in hectares by summing trees, forest, and woody_wetland
      total_trees_hectares = (trees + forest + woody_wetland),
      # Calculate the total area in hectares by summing all land cover types
      total_area_hectares = (
        grass + barren + shrub + grassland
        + agriculture + trees + forest + woody_wetland
        + impervious + water + wetland
      )
    ) %>%
    # Ungroup the dataset to remove the grouping by ctu_name and year
    dplyr::ungroup()

  # -------------------------------------------------------------------------
  # Calculate tree planting factors
  tree_planting_factors <-
    # Start with the ctu_forecast dataset within tb object
    tb$ctu_forecast %>%
    # Filter rows where the variable is "population" AND the year 2040
    dplyr::filter(var == "population",
                  year == 2040) %>%
    # Remove 'year' and 'var' columns
    dplyr::select(-c(year, var)) %>%
    # Rename the 'value' column to 'population'
    dplyr::rename(population = value) %>%
    # Perform a full join with the land_cover_by_city dataset
    dplyr::full_join(
      land_cover_by_city %>%
        dplyr::filter(
          land_cover_type == "trees",
          year == 2040
        ) %>%
        # Select rows with land_cover_type as "trees" and year as 2040, and remove the 'land_cover_type' column
        dplyr::select(-c(land_cover_type)),
      by = "ctu_name"
    ) %>%
    # Rename the 'land_cover_hectares' column to 'total_tree_canopy_hectares'
    dplyr::rename(total_tree_canopy_hectares = land_cover_hectares) %>%
    # Calculate LA goal hectares
    dplyr::mutate(
      LA_goal_hectares = (population * .tree_planting_per_capita)
      / .tree_planting_per_hectare
    ) %>%
    # Perform a right join with the total_plantable_area dataset
    dplyr::right_join(
      (
        total_plantable_area %>%
          # Filter rows for the year 2040
          dplyr::filter(year == 2040) %>%
          # Remove the 'year' column
          dplyr::select(-c(year)) %>%
          # Rename the 'plantable_area_hectares' column to 'pervious_surface_hectares'
          dplyr::rename(
            pervious_surface_hectares =
              plantable_area_hectares
          )
      ),
      by = "ctu_name"
    ) %>%
    # Ungroup the dataset
    dplyr::ungroup()

  # -------------------------------------------------------------------------
  # Calculate tree planting scenario based on tree_planting_factors
  tree_planting_scenario <-
    # Start with the tree_planting_factors dataset
    tree_planting_factors %>%
    # Group the dataset by ctu_name (city)
    dplyr::group_by(ctu_name) %>%
    # Create new variables for the dataset while keeping only the new variables and grouping variable (ctu_name)
    dplyr::transmute(
      match_los_angeles_million_trees_plan_percent =
        # Calculate the percentage of matching Los Angeles Million Trees Plan by dividing the sum of
        # LA_goal_hectares and total_tree_canopy_hectares by total_tree_canopy_hectares
        (LA_goal_hectares + total_tree_canopy_hectares) /
        total_tree_canopy_hectares,
      tree_planting_on_all_pervious_sufaces_percent =
        # Calculate the percentage of tree planting on all pervious surfaces by dividing the
        # sum of pervious_surface_hectares and total_tree_canopy_hectares by total_tree_canopy_hectares
        (pervious_surface_hectares + total_tree_canopy_hectares) /
        total_tree_canopy_hectares,
    ) %>%
    # Ungroup the dataset to remove the grouping by ctu_name
    dplyr::ungroup()


  # -------------------------------------------------------------------------
  # Calculate tree planting land cover based on total_plantable_area and tree_planting_scenario
  tree_planting_land_cover <-
    # Start with the total_plantable_area dataset
    total_plantable_area %>%
    # Perform a right join with the tree_planting_scenario dataset
    dplyr::right_join(.,
                      tree_planting_scenario,
                      by = "ctu_name"
    ) %>%
    # Create new variables and modify existing ones
    dplyr::mutate(
      # Calculate the maximum number of trees
      max_trees = total_area_hectares - woody_wetland - forest - impervious - wetland,
      # Calculate the number of trees based on the selected tree_planting_intervention
      trees =
        dplyr::if_else(year == 2016,
                       trees,
                       (if (.tree_planting_intervention == "tree_planting_on_all_pervious") {
                         dplyr::if_else(
                           trees * tree_planting_on_all_pervious_sufaces_percent < max_trees,
                           trees * tree_planting_on_all_pervious_sufaces_percent,
                           max_trees
                         )
                       } else if (.tree_planting_intervention == "match_la_million_trees_goal") {
                         dplyr::if_else(
                           trees * match_los_angeles_million_trees_plan_percent < max_trees,
                           trees * match_los_angeles_million_trees_plan_percent,
                           max_trees
                         )
                       } else if (.tree_planting_intervention == "double") {
                         dplyr::if_else(
                           (trees *  2) < max_trees,
                           (trees *  2),
                           max_trees
                         )
                       })
        )
    ) %>%
    # Calculate the total tree count in the scenario
    dplyr::mutate(total_scenario_tree = trees + forest + woody_wetland) %>%
    # Calculate the total tree count in the business-as-usual case
    dplyr::mutate(total_bau_tree = total_trees_hectares) %>%
    # Calculate the increased number of trees in the scenario
    dplyr::mutate(increased_tree = total_scenario_tree - total_bau_tree) %>%
    # Calculate the scaling factor
    dplyr::mutate(
      scaling_factor =
        dplyr::if_else(
          increased_tree > 0,
          (plantable_area_hectares - increased_tree) / plantable_area_hectares,
          1
        )
    ) %>%
    # Apply a function to selected columns
    dplyr::mutate(dplyr::across(
      # Select the columns to apply the function to
      .cols = c(grass, water, barren, shrub, grassland, agriculture),
      # Modify the values in the selected columns based on the year and scaling_factor
      ~ dplyr::if_else(
        year == 2016,
        .x,
        dplyr::if_else(.x * scaling_factor < 1, 0, .x * scaling_factor)
      )
    )) %>%
    # Ungroup the dataset
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
