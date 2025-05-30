#' @title Run natural systems scenario
#' @family natural systems
#'
#' @description This function simulates the impact of various land use scenarios on carbon
#'    sequestration and carbon stock in cities or townships. It considers urban tree planting and
#'    restoration of abandoned agriculture.
#'
#' @inheritParams calc_carbon_sequestration_per_ctu
#' @inheritParams calc_carbon_stock_per_ctu
#' @inheritParams calc_parking_lot_land_cover
#' @inheritParams calc_land_cover_by_land_use
#' @inheritParams calc_tree_planting_land_cover
#' @inheritParams calc_scen_land_use
#' @inheritParams calc_land_by_development_type
#' @inheritParams calc_electrify_commercial_heating
#' @inheritParams run_scenario_building
#' @inheritParams filter_ctu
#'
#' @export
#' @import dplyr
#' @import tidyr
#'
run_scenario_natural_systems <- function(tb_inv = natural_systems_data$ctu_lc_inventory,
                                  tb_future = natural_systems_data$ctu_lc_null,
                                  tb_seq = natural_systems_data$land_cover_carbon,
                                  .selected_ctu = "all",
                                  .urban_tree_start = 2025,
                                  .urban_tree_time = 10,
                                  .urban_tree_area_perc = 0,
                                  .l2l_start = 2025,
                                  .l2l_time = 10,
                                  .l2l_area_perc = 0,
                                  .restoration_start = 2025,
                                  .restoration_time = 10,
                                  .restoration_area_perc = 0,
                                  .enviro_factors = ghg.ccap::enviro_factors,
                                  detail = FALSE) {
  # -------------------------------------------------------------------------
  # store filtered database tables into variables
  df_hist <- filter_ctu(tb_inv, .selected_ctu = .selected_ctu)
  df_null <- filter_ctu(tb_future, .selected_ctu = .selected_ctu)


  tb01 <- if(.urban_tree_area_perc == 0) {df_null} else {
    ghg.ccap::urban_tree_planting(
    df_hist = df_hist,
    df_null = df_null,
    #.selected_ctu = .selected_ctu,
    .urban_tree_start = .urban_tree_start,
    .urban_tree_time = .urban_tree_time,
    .urban_tree_area_perc = .urban_tree_area_perc
  )
}

  tb02 <- if(.restoration_area_perc == 0) {tb01} else {
    ghg.ccap::crop_restoration(
    df_hist = df_hist,
    df_null = tb01,
    #.selected_ctu = .selected_ctu,
    .restoration_start = .restoration_start,
    .restoration_time = .restoration_time,
    .restoration_area_perc = .restoration_area_perc
  )
  }

  # -------------------------------------------------------------------------
  # store carbon sequestration function output into variable
  carbon_sequestration_out <- rbind(df_hist, tb02) %>%
    dplyr::arrange(inventory_year) %>%
    tidyr::pivot_longer(
      # cols = natural_systems_data$land_cover_carbon$land_cover_type,
      cols = c("Bare", "Cropland", "Developed_Low", "Developed_Med",
               "Developed_High", "Water", "Grassland", "Tree",
               "Urban_Grassland", "Urban_Tree", "Wetland"),
      names_to = "land_cover_type",
      values_to = "area"
    ) %>%
    dplyr::left_join(
      natural_systems_data$land_cover_carbon
    ) %>%
    dplyr::mutate(value_emissions = area * seq_mtco2e_sqkm,
           value_stock_potential = area * stock_mtco2e_sqkm)


  # # -------------------------------------------------------------------------
  # check_inputs(
  #   "parking_lot_reduction_percentage",
  #   .parking_lot_re

  # -------------------------------------------------------------------------
  return(carbon_sequestration_out)
}
