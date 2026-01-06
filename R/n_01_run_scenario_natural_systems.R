#' @title Run natural systems scenario
#' @family natural systems
#'
#' @description This function simulates the impact of various land use scenarios on carbon
#'    sequestration and carbon stock in cities or townships. It considers urban tree planting and
#'    restoration of abandoned agriculture.
#'
#'
#' @export
#' @import dplyr
#' @import tidyr
#'
run_scenario_natural_systems <- function(tb_inv = natural_systems_data$inventory,
                                         tb_future = natural_systems_data$projections,
                                         tb_seq = natural_systems_data$land_cover_carbon,
                                         .selected_ctu = "all",

                                         # module 1 - restore wetlands
                                         .wetland_restore_start = 2025,
                                         .wetland_restore_end = 2050,
                                         .wetland_restore_fromGrass_perc = 0, # percent of grassland to convert to wetland
                                         .wetland_restore_fromBare_perc = 0,  # percent of bare land to convert to wetland
                                         .wetland_restore_fromCrop_perc = 0,  # percent of cropland to convert to wetland
                                         .wetland_restore_fromTree_perc = 0,  # percent of forest to convert to wetland

                                         # module 2 - restore forests
                                         .forest_restore_start = 2025,
                                         .forest_restore_end = 2050,
                                         .forest_restore_fromGrass_perc = 0, # percent of grassland to convert to forest
                                         .forest_restore_fromBare_perc = 0,  # percent of bare land to convert to forest
                                         .forest_restore_fromCrop_perc = 0,  # percent of cropland to convert to forest

                                         # module 3 - plant community trees
                                         .community_tree_start = 2025,
                                         .community_tree_end = 2050,
                                         .community_tree_perc = 0, # percent of urban area to plant community trees

                                         # module 4 - plant pocket prairies
                                         .pocket_prairie_start = 2025,
                                         .pocket_prairie_end = 2050,
                                         .pocket_prairie_perc = 0, # percent of lawn area to plant pocket prairies

                                         .enviro_factors = ghg.ccap::enviro_factors,
                                         detail = FALSE) {
  # -------------------------------------------------------------------------

  # if .selected_ctu has the word "County" in it, we filter by county
  if (grepl("County", .selected_ctu)) {
    df_hist <- tb_inv$county %>% filter(geog_name == .selected_ctu)
    df_null <- tb_future$county %>% filter(geog_name == .selected_ctu)
  } else if (.selected_ctu == "Regional") {
    df_hist <- tb_inv$region
    df_null <- tb_future$region
  } else if (.selected_ctu == "all") {
    df_hist <- tb_inv$ctu
    df_null <- tb_future$ctu
  } else {
    df_hist <- tb_inv$ctu %>% filter(geog_name == .selected_ctu)
    df_null <- tb_future$ctu %>% filter(geog_name == .selected_ctu)
  }




  ## module 1 - restore wetlands -----
  #+ here's the rub: restoring wetlands based on potential_wetland_area incurs
  #+ a cost of removing other natural land cover to make up for it. So we should
  #+ start with this module first before urban tree planting and forest restoration.

  #+ Let's work out the details. df_hist is the inventory of land cover area over time
  #+ from 2000 to 2022. df_null is the projection of land cover area from 2023 to 2050
  #+ assuming no changes. The wetland restoration module needs to look at the last year
  #+ of df_hist to see how much wetland area exists currently, and then look at the
  #+ potential_wetland_area column to see how much more wetland area can be restored.

  if (.wetland_restore_fromGrass_perc != 0 | .wetland_restore_fromBare_perc != 0 |
      .wetland_restore_fromCrop_perc != 0 | .wetland_restore_fromTree_perc != 0) {
    tb01 <- ghg.ccap::restore_wetlands(
      df_null = df_null,
      start_yr = .wetland_restore_start,
      end_yr = .wetland_restore_end,
      tree_pct = .wetland_restore_fromTree_perc,
      grass_pct = .wetland_restore_fromGrass_perc,
      bare_pct = .wetland_restore_fromBare_perc,
      crop_pct = .wetland_restore_fromCrop_perc
    )
  } else {
    tb01 <- df_null
  }


  ## module 2 - restore forests -----
  # this one is a little challenging since it acts on the same land cover types that
  # the previous module acted on. So we gotta think carefully about how the logic will work here.
  if (.forest_restore_fromGrass_perc != 0 | .forest_restore_fromBare_perc != 0 |
      .forest_restore_fromCrop_perc != 0) {
    tb02 <- ghg.ccap::restore_forests(
      df_null = tb01,
      start_yr = .forest_restore_start,
      end_yr = .forest_restore_end,
      grass_pct = .forest_restore_fromGrass_perc,
      bare_pct = .forest_restore_fromBare_perc,
      crop_pct = .forest_restore_fromCrop_perc
    )
  } else {
    tb02 <- tb01
  }


  ## module 3 - plant community trees -----
  if (.community_tree_perc != 0) {
    tb03 <- ghg.ccap::plant_community_trees(
      df_null = tb02,
      start_yr = .community_tree_start,
      end_yr = .community_tree_end,
      area_pct = .community_tree_perc
    )
  } else {
    tb03 <- tb02
  }


  ## module 4 - plant pocket prairies -----
  if (.pocket_prairie_perc != 0) {
    tb04 <- ghg.ccap::plant_pocket_prairies(
      df_null = tb03,
      start_yr = .pocket_prairie_start,
      end_yr = .pocket_prairie_end,
      area_pct = .pocket_prairie_perc
    )
  } else {
    tb04 <- tb03
  }


  # tb04 %>%
  # ggplot() +
  #   geom_line(alpha = 0.9, linewidth=0.5,
  #             aes(x = inventory_year, y = area,
  #                 color = land_cover_type),show.legend = F) +
  #   theme(
  #     legend.position = "bottom",
  #     legend.direction = "horizontal"
  #   ) +
  #   facet_wrap(~land_cover_type, scales="free_y")
  #
  #
  # tb04 %>%
  #   # df_null %>%
  #   # filter(land_cover_type %in% c("Wetland", "Grassland", "Bare", "Cropland")) %>%
  #   # filter(land_cover_type %in% c("Urban_Tree", "Developed_Low", "Developed_Med", "Developed_High")) %>%
  #   # filter(land_cover_type %in% c("Grassland")) %>%
  #
  # ggplot(
  #   aes(x = inventory_year, y = area, fill = land_cover_type)
  # ) +
  #   # stacked area chart
  #   geom_area(alpha = 0.6, color = NA, position = "stack") +
  #   geom_line(alpha = 0.9, linewidth=0.5,
  #             aes(color = land_cover_type), position = "stack", show.legend = F) +
  #   theme(
  #     legend.position = "bottom",
  #     legend.direction = "horizontal"
  #   )

  ## calculate carbon sequestration and stock potential -----
  carbon_sequestration_out <- rbind(df_hist, tb04) %>%
    dplyr::arrange(inventory_year, land_cover_type) %>%
    dplyr::left_join(tb_seq, by = c("land_cover_type")) %>%
    dplyr::mutate(
      value_emissions = area * seq_mtco2e_sqkm,
      value_stock_potential = area * stock_mtco2e_sqkm
    )


  # carbon_sequestration_out %>%
  #   filter(!is.na(value_emissions)) %>%
  #   ggplot(
  #     aes(x = inventory_year, y = value_emissions, fill = land_cover_type)
  #   ) +
  #   # stacked area chart
  #   geom_area(alpha = 0.6, color = NA, position = "stack") +
  #   geom_line(alpha = 0.9, linewidth=0.5,
  #             aes(color = land_cover_type), position = "stack", show.legend = F) +
  #   theme(
  #     legend.position = "bottom",
  #     legend.direction = "horizontal"
  #   )



  # -------------------------------------------------------------------------
  return(carbon_sequestration_out)
}
