#' @title Calculate Conservation Tillage by City/Township
#' @family Land Use
#'
#' @description Calculates the impact of conservation tillage scenarios on carbon
#'      stocks by city/township
#'
#' @inheritParams calc_parking_lot_land_cover
#' @param .conservation_tillage_intervention **Character**.
#'      The type of conservation tillage scenario to be explored.
#'      Default is `current_conservation_tillage`. The options are:
#'      * `"current_conservation_tillage"` it maintains the per county levels of conservation tillage
#'      relative to the baseline year.
#'      * `"double_conservation_tillage"` it doubles the per county levels of conservation tillage
#'      relative to the baseline year.
#'      * `"maximum_conservation_tillage"` it assumes that all agricultural land implements conservation
#'      tillage.
#' @param .w2w_diesel_emission_factor_kg_co2e_per_gal numeric, 
#'      Well-to-well diesel emissions factor in kg of carbon dioxide equivalent (CO2e) per gallon
#'      Default is `12.50`.
#'      * `Hilman and Ramaswami, 2009`.
#' @param .avoided_emissions_tractor_use_mg_co2e_per_hectare numeric, 
#'      Million grams of carbon dioxide equivalent (CO2e) per hectare.
#'      From `United States Department of Agriculture`
#'      Default is `0.102`.
#' @param .agricultural_land_carbon_stock_mg_c_per_hectare numeric, 
#'      Million grams of carbon (C) per hectare.
#'      From [`Tran et al., 2015`](https://doi.org/10.1073/pnas.1512542112)
#'      Default is `41`.
#' @param .maximum_soc_accumation_under_reduced_or_no_till_ag numeric, 
#'      Maximum SOC acumulation per hectare under reduced or no till agriculture.
#'      Default is `1.54`.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' calc_conservation_tillage(
#'     tb = land_use_data,
#'     .urban_form_scenario = "bau",
#'     .conservation_tillage_intervention = "current_conservation_tillage",
#'     .tree_planting_intervention = "tree_planting_on_all_pervious",
#'     .w2w_diesel_emission_factor_kg_co2e_per_gal = 12.50,
#'     .avoided_emissions_tractor_use_mg_co2e_per_hectare = 0.0102,
#'     .agricultural_land_carbon_stock_mg_c_per_hectare = 3,
#'     .maximum_soc_accumation_under_reduced_or_no_till_ag = 1.54,
#'     .tree_planting_per_capita = 0.26,
#'     .tree_planting_per_hectare = 247,
#'     .parking_lot_reduction_percentage = 0.8,
#'     detail = FALSE)
#'  }
calc_conservation_tillage <-
  function(tb,
           .conservation_tillage_intervention,
           .w2w_diesel_emission_factor_kg_co2e_per_gal,
           .avoided_emissions_tractor_use_mg_co2e_per_hectare,
           .agricultural_land_carbon_stock_mg_c_per_hectare,
           .maximum_soc_accumation_under_reduced_or_no_till_ag,
           .parking_lot_reduction_percentage,
           detail,
           .tree_planting_intervention,
           .tree_planting_per_capita,
           .tree_planting_per_hectare,
           .urban_form_scenario) {

    match.arg(
      arg = .conservation_tillage_intervention,
      choices = c(
        "current_conservation_tillage",
        "double_conservation_tillage",
        "maximum_conservation_tillage"
      )
    )

    baseline_bau <-
      calc_parking_lot_land_cover(
        tb = tb,
        detail = detail,
        .tree_planting_intervention = .tree_planting_intervention,
        .tree_planting_per_capita = .tree_planting_per_capita,
        .tree_planting_per_hectare = .tree_planting_per_hectare,
        .parking_lot_reduction_percentage = .parking_lot_reduction_percentage,
        .urban_form_scenario = .urban_form_scenario
      ) %>%
      right_join(.,
                 tb$ctu_county,
                 by = "ctu_name") %>%
      right_join(.,
                 tb$current_conservation_tillage_county,
                 by = "co_name") %>%
      dplyr::select(ctu_name, co_name, year, agriculture) %>%
      tidyr::pivot_wider(
        names_from = year,
        values_from = agriculture,
        names_prefix = 'agriculture_hectares_year_'
      ) %>%
      right_join(.,
                 tb$current_conservation_tillage_county,
                 by = "co_name") %>%
      dplyr::mutate(
        baseline_carbon_stock_mg_c_per_hectare = agriculture_hectares_year_2016 *
          .agricultural_land_carbon_stock_mg_c_per_hectare,
        bau_carbon_stock_without_conservation_tillage_mg_c_per_hectare =
          agriculture_hectares_year_2040 *
          .agricultural_land_carbon_stock_mg_c_per_hectare
      )

    conservation_tillage_carbon_stocks_mg_c <-
      if (.conservation_tillage_intervention == "current_conservation_tillage") {
        baseline_bau %>%
          dplyr::mutate(
            current_conservation_tillage_mg_c =
              (
                agriculture_hectares_year_2040 * current_conservation_tillage_percent *
                  .maximum_soc_accumation_under_reduced_or_no_till_ag *
                  .agricultural_land_carbon_stock_mg_c_per_hectare
              )
            + (
              agriculture_hectares_year_2040 * (1 - current_conservation_tillage_percent)
            )
            * .agricultural_land_carbon_stock_mg_c_per_hectare,

            carbon_stock_change_from_conservation_ag_mg_c =

              current_conservation_tillage_mg_c -
              bau_carbon_stock_without_conservation_tillage_mg_c_per_hectare,

            reduced_tractor_emissions_relative_to_current_conservation_tillage_mg_co2e_per_year =
              0
          )

      } else if (.conservation_tillage_intervention == "double_conservation_tillage") {
        baseline_bau %>%
          dplyr::mutate(
            current_conservation_tillage_mg_c =
              (
                agriculture_hectares_year_2040 * (current_conservation_tillage_percent *
                                                    2) *
                  .maximum_soc_accumation_under_reduced_or_no_till_ag *
                  .agricultural_land_carbon_stock_mg_c_per_hectare
              )
            + (agriculture_hectares_year_2040 * (
              1 - (current_conservation_tillage_percent * 2)
            ))
            * .agricultural_land_carbon_stock_mg_c_per_hectare,

            carbon_stock_change_from_conservation_ag_mg_c =

              current_conservation_tillage_mg_c -
              bau_carbon_stock_without_conservation_tillage_mg_c_per_hectare,

            reduced_tractor_emissions_relative_to_current_conservation_tillage_mg_co2e_per_year =
              (
                agriculture_hectares_year_2040  *
                  .avoided_emissions_tractor_use_mg_co2e_per_hectare *
                  (current_conservation_tillage_percent * 2) -
                  current_conservation_tillage_percent
              ) *
              -1

          )
      } else {
        baseline_bau %>%
          dplyr::mutate(
            current_conservation_tillage_mg_c =
              (
                agriculture_hectares_year_2040 * 1 *
                  .maximum_soc_accumation_under_reduced_or_no_till_ag *
                  .agricultural_land_carbon_stock_mg_c_per_hectare
              )
            + (
              agriculture_hectares_year_2040 *
                .agricultural_land_carbon_stock_mg_c_per_hectare
            ),

            carbon_stock_change_from_conservation_ag_mg_c =

              current_conservation_tillage_mg_c -
              bau_carbon_stock_without_conservation_tillage_mg_c_per_hectare,

            reduced_tractor_emissions_relative_to_current_conservation_tillage_mg_co2e_per_year =
              (
                agriculture_hectares_year_2040  *
                  .avoided_emissions_tractor_use_mg_co2e_per_hectare *   -1
              )

          )
      }

    return(conservation_tillage_carbon_stocks_mg_c)

  }
