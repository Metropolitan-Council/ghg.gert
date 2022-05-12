#' Calculate Tree Planting Factors
#'
#' @param tree_planting_factor_per_capita the default tree planting per capita factor
#' is '0.26' from the Los Angeles 1,000,000 trees scenario
#'
#' @param tree_planting_per_hectares the default tree planting per hectares factor is
#' '247'
#'
#' @return
#' @export
#'
#' @examples
calc_tree_planting_factors <-
  function(tree_planting_per_capita = 0.26,
           tree_planting_per_hectare = 247) {
    tree_planting_factors <-
      tb$ctu_forecast %>%
      dplyr::filter(metric == "population") %>%
      dplyr::filter(year == 2040) %>%
      dplyr::select(-c(year, metric)) %>%
      dplyr::rename(population = value) %>%
      dplyr::full_join(
        calc_land_cover_by_city() %>%
          dplyr::filter(land_cover_description_2 == "trees",
                        year == 2040) %>%
          # to check: are you aware that Brooklyn Center has NAs for tree cover?
          # land_cover_by_city %>% filter(ctu_name == "Brooklyn Center", land_cover_description_2 == "trees" )
          select(-c(year, land_cover_description_2))
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
          calc_total_plantable_area() %>%
            dplyr::filter(year == 2040) %>%
            dplyr::select(-c(year)) %>%
            dplyr::rename(pervious_surface_hectares =
                            plantable_area_hectares)
        ),
        by = "ctu_name"
      )

    return(tree_planting_factors)
  }
