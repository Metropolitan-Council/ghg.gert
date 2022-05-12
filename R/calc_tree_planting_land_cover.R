#' Title
#'
#' @return
#' @export
#'
#' @examples
calc_tree_planting_land_cover <- function() {
  calc_land_cover_by_city()  %>%
    tidyr::pivot_wider(names_from = land_cover_description_2,
                       values_from = land_cover_hectares) %>%
    dplyr::full_join(tree_planting_factors, by = c("ctu_name")) %>%
    base::list(
      land_cover_by_city_total_plantable,
      land_cover_by_city_total_area,
      total_tree_canopy_hectares
    ) %>%
    purrr::reduce(full_join, by = c("ctu_name", "year")) %>%
    dplyr::filter(ctu_name != c("Fort Snelling (unorg.)",
                                "Hilltop",
                                "Rogers")) %>% #to check: these 3 not included originally
    #for liz; do you see the quick purrr fix here? probably a group_by(ctu_name, year) and maybe then purrring on just the 2040 data?
    mutate(
      max_trees = total_area - (impervious + forest + woody_wetland + wetland),
      trees =
        dplyr::if_else(
          year == 2016,
          trees,
          dplyr::if_else(
            #need to add choice of main parameter
            trees * tree_planting_on_all_pervious < max_trees,
            trees * tree_planting_on_all_pervious,
            max_trees
          )
        )
    ) %>%
    dplyr::mutate(total_scenario_tree = trees + forest + woody_wetland) %>%
    dplyr::mutate(total_bau_tree = total_trees) %>%
    dplyr::mutate(increased_tree = total_scenario_tree - total_bau_tree) %>%
    dplyr::mutate(scaling_factor =
                    dplyr::if_else(
                      increased_tree > 0,
                      (total_plantable - increased_tree) / total_plantable,
                      1
                    )) %>%
    dplyr::mutate(grass =
                    dplyr::if_else(year == 2016,
                                   grass,
                                   grass * scaling_factor)) %>%
    dplyr::mutate(water =
                    dplyr::if_else(year == 2016,
                                   water,
                                   water * scaling_factor)) %>%
    dplyr::mutate(barren =
                    dplyr::if_else(year == 2016,
                                   barren,
                                   barren * scaling_factor)) %>%
    dplyr::mutate(shrub =
                    dplyr::if_else(year == 2016,
                                   shrub,
                                   shrub * scaling_factor)) %>%
    dplyr::mutate(grassland =
                    dplyr::if_else(year == 2016,
                                   grassland,
                                   grassland * scaling_factor)) %>%
    dplyr::mutate(agriculture =
                    dplyr::if_else(year == 2016,
                                   agriculture,
                                   agriculture * scaling_factor))
}
