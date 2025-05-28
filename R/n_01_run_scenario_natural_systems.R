#' @title Run land use scenario
#' @family land use
#'
#' @description This function simulates the impact of various land use scenarios on carbon
#'    sequestration and carbon stock in cities or townships. It considers factors such as
#'    urban form, conservation tillage intervention, tree planting intervention, tree planting
#'    per capita, tree planting per hectare, and parking lot reduction percentage.
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
#'
#' @examples
#' \dontrun{
#' library(ghg.ccap)
#'
#' run_scenario_land_use(
#'   tb = land_use_data,
#'   .selected_ctu = all,
#'   .conservation_tillage_intervention = "current_conservation_tillage",
#'   .tree_planting_intervention = "tree_planting_on_all_pervious",
#'   .tree_planting_per_capita = 0.26,
#'   .tree_planting_per_hectare = 247,
#'   .parking_lot_reduction_percentage = 0.8,
#'   detail = FALSE
#' )
#' }
run_scenario_land_use <- function(tb_inv = natural_systems_data$ctu_lc_inventory,
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

  # -------------------------------------------------------------------------
  # store carbon sequestration function output into variable
  carbon_sequestration_per_ctu <-
    calc_carbon_sequestration_per_ctu(
      tb = tb,
      .selected_ctu = .selected_ctu,
      .tree_planting_intervention = .tree_planting_intervention,
      .tree_planting_per_capita = .tree_planting_per_capita,
      .tree_planting_per_hectare = .tree_planting_per_hectare,
      .parking_lot_reduction_percentage = .parking_lot_reduction_percentage,
      detail = detail
    )

  # -------------------------------------------------------------------------
  # store carbon stock function output into variable
  carbon_stock_per_ctu <-
    calc_carbon_stock_per_ctu(
      tb = tb,
      .selected_ctu = .selected_ctu,
      .conservation_tillage_intervention = .conservation_tillage_intervention,
      .parking_lot_reduction_percentage = .parking_lot_reduction_percentage,
      .tree_planting_intervention = .tree_planting_intervention,
      .tree_planting_per_capita = .tree_planting_per_capita,
      .tree_planting_per_hectare = .tree_planting_per_hectare,
      detail = detail,
      .enviro_factors = .enviro_factors
    )

  # -------------------------------------------------------------------------
  check_inputs(
    "parking_lot_reduction_percentage",
    .parking_lot_reduction_percentage
  )

  # -------------------------------------------------------------------------

  land_cover_results <- dplyr::bind_rows(
    carbon_sequestration_per_ctu %>%
      dplyr::mutate(
        var = "sequestration_tonnes_co2e_per_year",
        year = as.numeric(year)
      ),
    carbon_stock_per_ctu %>%
      dplyr::mutate(
        var = "stock_tonnes_co2e_per_year (land conversion emissions)",
        year = as.numeric(year)
      )
  ) %>%
    dplyr::group_by(ctu_name, year, var) %>%
    tidyr::pivot_longer(names_to = "land_cover_type", cols = -c(ctu_name, year, var)) %>%
    dplyr::mutate(
      urban_form_scenario = .enviro_factors$URBAN_FORM_SCENARIO,
      tree_planting_intervention = .tree_planting_intervention,
      parking_lot_reduction_percentage = .parking_lot_reduction_percentage,
      conservation_tillage_intervention = .conservation_tillage_intervention
    )

  # -------------------------------------------------------------------------
  land_use_module_output <-
    land_cover_results %>%
    dplyr::group_by(
      ctu_name,
      year,
      var,
      urban_form_scenario,
      tree_planting_intervention,
      conservation_tillage_intervention,
      parking_lot_reduction_percentage,
    ) %>%
    dplyr::summarise(value = sum(value), .groups = "drop")


  # -------------------------------------------------------------------------
  return(land_use_module_output)
}
