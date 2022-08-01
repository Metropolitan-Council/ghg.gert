#' @title Prepare green infrastructure module
#' @family land use
#'
#' @description  Generates the outputs of the land use and green
#' infrastructure module.
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
#' scen_green_infrastructure(
#'   tb = land_use_data,
#'   detail = FALSE,
#'   .urban_form_scenario = "bau",
#'   .conservation_tillage_intervention = "current_conservation_tillage",
#'   .w2w_diesel_emission_factor_kg_co2e_per_gal = 12.50,
#'   .avoided_emissions_tractor_use_mg_co2e_per_hectare = 0.0102,
#'   .maximum_soc_accumation_under_reduced_or_no_till_ag = 1.54,
#'   .tree_planting_intervention = "tree_planting_on_all_pervious",
#'   .tree_planting_per_capita = 0.26,
#'   .tree_planting_per_hectare = 247,
#'   .parking_lot_reduction_percentage = 0.8,
#'   .agricultural_land_carbon_stock_mg_c_per_hectare = 3
#' )
#' }
scen_green_infrastructure <- function(tb,
                                      detail,
                                      .urban_form_scenario,
                                      .conservation_tillage_intervention,
                                      .w2w_diesel_emission_factor_kg_co2e_per_gal,
                                      .avoided_emissions_tractor_use_mg_co2e_per_hectare,
                                      .maximum_soc_accumation_under_reduced_or_no_till_ag,
                                      .tree_planting_intervention,
                                      .tree_planting_per_capita,
                                      .tree_planting_per_hectare,
                                      .parking_lot_reduction_percentage,
                                      .agricultural_land_carbon_stock_mg_c_per_hectare) {
  dplyr::bind_rows(
    calc_carbon_sequestration_per_ctu(
      tb = tb,
      .urban_form_scenario = .urban_form_scenario,
      .tree_planting_intervention = .tree_planting_intervention,
      .tree_planting_per_capita = .tree_planting_per_capita,
      .tree_planting_per_hectare = .tree_planting_per_hectare,
      .parking_lot_reduction_percentage = .parking_lot_reduction_percentage,
      detail = detail
    ) %>%
      dplyr::rename(year = year.2040) %>%
      tidyr::pivot_longer(
        cols = -c(ctu_name, year),
        names_to = "metric_detail",
        values_to = "value"
      ) %>%
      dplyr::mutate(metric = "seq_mg_c_per_year") %>%
      tidyr::unite('metric', c(metric_detail, metric), remove = FALSE) %>%
      dplyr::select(ctu_name, year, metric, value) %>%
      dplyr::mutate(detail = "sequestration"),

    calc_carbon_stock_per_ctu(
      tb = tb,
      .conservation_tillage_intervention = .conservation_tillage_intervention,
      .w2w_diesel_emission_factor_kg_co2e_per_gal = .w2w_diesel_emission_factor_kg_co2e_per_gal,
      .avoided_emissions_tractor_use_mg_co2e_per_hectare = .avoided_emissions_tractor_use_mg_co2e_per_hectare,
      .agricultural_land_carbon_stock_mg_c_per_hectare = .agricultural_land_carbon_stock_mg_c_per_hectare,
      .maximum_soc_accumation_under_reduced_or_no_till_ag = .maximum_soc_accumation_under_reduced_or_no_till_ag,
      .parking_lot_reduction_percentage = .parking_lot_reduction_percentage,
      .tree_planting_intervention = .tree_planting_intervention,
      .tree_planting_per_capita = .tree_planting_per_capita,
      .tree_planting_per_hectare = .tree_planting_per_hectare,
      .urban_form_scenario = .urban_form_scenario,
      detail = detail
    ) %>%
      tidyr::pivot_longer(
        cols = -c(ctu_name, year),
        names_to = "metric_detail",
        values_to = "value"
      ) %>%
      dplyr::mutate(metric = "stock_mg_c_per_ha") %>%
      tidyr::unite('metric', c(metric_detail, metric), remove = FALSE) %>%
      dplyr::select(ctu_name, year, metric, value) %>%
      dplyr::mutate(detail = "stock")

  ) %>%
    dplyr::mutate(
      urban_form_scenario = .urban_form_scenario,
      tree_planting_intervention = .tree_planting_intervention,
      parking_lot_reduction_percentage = .parking_lot_reduction_percentage,
      conservation_tillage_intervention = .conservation_tillage_intervention
    ) %>%
    dplyr::select(
      ctu_name,
      year,
      urban_form_scenario,
      tree_planting_intervention,
      parking_lot_reduction_percentage,
      conservation_tillage_intervention,
      metric,
      detail,
      metric_detail,
      value
    )
}
