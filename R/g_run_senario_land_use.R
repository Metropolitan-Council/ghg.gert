#' @title Run land use scenario
#' @family land use
#'
#' @inheritParams calc_carbon_sequestration_per_ctu
#' @inheritParams calc_carbon_stock_per_ctu
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' run_scenario_land_use(
#'   tb = land_use_data,
#'   .urban_form_scenario = "bau",
#'   .conservation_tillage_intervention = "current_conservation_tillage",
#'   .tree_planting_intervention = "tree_planting_on_all_pervious",
#'   .avoided_emissions_tractor_use_mg_co2e_per_hectare = 0.0102,
#'   .tree_planting_per_capita = 0.26,
#'   .tree_planting_per_hectare = 247,
#'   .parking_lot_reduction_percentage = 0.8,
#'   detail = FALSE
#' )
#' }
run_scenario_land_use <- function(tb = land_use_data,
                                  .urban_form_scenario = "bau",
                                  .conservation_tillage_intervention = "current_conservation_tillage",
                                  .tree_planting_intervention = "match_la_million_trees_goal",
                                  .tree_planting_per_capita = 0.26,
                                  .tree_planting_per_hectare = 247,
                                  .parking_lot_reduction_percentage = 0.8,
                                  detail = FALSE) {


# -------------------------------------------------------------------------
  carbon_sequestration_per_ctu <-
    calc_carbon_sequestration_per_ctu(
      tb = tb,
      .urban_form_scenario = .urban_form_scenario,
      .tree_planting_intervention = .tree_planting_intervention,
      .tree_planting_per_capita = .tree_planting_per_capita,
      .tree_planting_per_hectare = .tree_planting_per_hectare,
      .parking_lot_reduction_percentage = .parking_lot_reduction_percentage,
      detail = detail
    )


# -------------------------------------------------------------------------
  carbon_stock_per_ctu <-
    calc_carbon_stock_per_ctu(
      tb = tb,
      .conservation_tillage_intervention = .conservation_tillage_intervention,
      .parking_lot_reduction_percentage = .parking_lot_reduction_percentage,
      .tree_planting_intervention = .tree_planting_intervention,
      .tree_planting_per_capita = .tree_planting_per_capita,
      .tree_planting_per_hectare = .tree_planting_per_hectare,
      .urban_form_scenario = .urban_form_scenario,
      detail = detail
    )

# -------------------------------------------------------------------------
  check_inputs(
    "parking_lot_reduction_percentage",
    .parking_lot_reduction_percentage
  )


# -------------------------------------------------------------------------
  land_use <- dplyr::bind_rows(
    carbon_sequestration_per_ctu %>%
      dplyr::rename(year = year.2040) %>%
      tidyr::pivot_longer(
        cols = -c(ctu_name, year),
        names_to = "metric_detail",
        values_to = "value"
      ) %>%
      dplyr::mutate(metric = "seq_mg_c_per_year") %>%
      tidyr::unite("metric", c(metric_detail, metric), remove = FALSE) %>%
      dplyr::select(ctu_name, year, metric, value) %>%
      dplyr::mutate(detail = "sequestration"),
    carbon_stock_per_ctu %>%
      tidyr::pivot_longer(
        cols = -c(ctu_name, year),
        names_to = "metric_detail",
        values_to = "value"
      ) %>%
      dplyr::mutate(metric = "stock_mg_c_per_ha") %>%
      tidyr::unite("metric", c(metric_detail, metric), remove = FALSE) %>%
      dplyr::select(ctu_name, year, metric, value) %>%
      dplyr::mutate(detail = "stock")
  ) %>%
    dplyr::mutate(
      urban_form_scenario = .urban_form_scenario,
      tree_planting_intervention = .tree_planting_intervention,
      parking_lot_reduction_percentage = .parking_lot_reduction_percentage,
      conservation_tillage_intervention = .conservation_tillage_intervention
    )

# -------------------------------------------------------------------------
  land_use_module_output <-
    land_use %>%
    dplyr::group_by(
      ctu_name,
      year,
      detail,
      urban_form_scenario,
      tree_planting_intervention,
      conservation_tillage_intervention,
      parking_lot_reduction_percentage,
    ) %>%
    dplyr::summarise(value = sum(value), .groups = "drop")


# -------------------------------------------------------------------------
  return(land_use_module_output)

}
