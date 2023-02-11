#' @title Calculate conservation tillage by city/township
#' @family land use
#'
#' @description Calculates the impact of conservation tillage scenarios on carbon
#'      stocks by city/township
#'
#' @inheritParams calc_parking_lot_land_cover
#' @param .conservation_tillage_intervention character,
#'      The type of conservation tillage scenario to be explored.
#'      Default is `current_conservation_tillage`. The options are:
#'      * `"current_conservation_tillage"` it maintains the per county levels of conservation tillage
#'      relative to the baseline year.
#'      * `"double_conservation_tillage"` it doubles the per county levels of conservation tillage
#'      relative to the baseline year.
#'      * `"maximum_conservation_tillage"` it assumes that all agricultural land implements conservation
#'      tillage.
#'
#' @return [tibble::tibble()] with column names...
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' calc_conservation_tillage(
#'   tb = land_use_data,
#'   .selected_ctu = "all",
#'   .urban_form_scenario = "bau",
#'   .conservation_tillage_intervention = "current_conservation_tillage",
#'   .tree_planting_intervention = "tree_planting_on_all_pervious",
#'   .tree_planting_per_capita = 0.26,
#'   .tree_planting_per_hectare = 247,
#'   .parking_lot_reduction_percentage = 0.8,
#'   detail = FALSE
#' )
#' }
calc_conservation_tillage <- function(tb,
                                      .selected_ctu = .selected_ctu,
                                      .conservation_tillage_intervention,
                                      .parking_lot_reduction_percentage,
                                      detail,
                                      .tree_planting_intervention,
                                      .tree_planting_per_capita,
                                      .tree_planting_per_hectare,
                                      .urban_form_scenario) {
  # -------------------------------------------------------------------------
  cli::cli_progress_message("*** calculating conservation tillage strategy \n")

  tb$ctu_forecast <- filter_ctu(tb$ctu_forecast, .selected_ctu = .selected_ctu)
  tb$ctu_land_use_hectares <- filter_ctu(tb$ctu_land_use_hectares, .selected_ctu = .selected_ctu)
  tb$ctu_land_use_2016_land_cover <- filter_ctu(tb$ctu_land_use_2016_land_cover, .selected_ctu = .selected_ctu)
  tb$ctu_county <- filter_ctu(tb$ctu_county, .selected_ctu = .selected_ctu)

  match.arg(
    arg = .conservation_tillage_intervention,
    choices = c(
      "current_conservation_tillage",
      "double_conservation_tillage",
      "maximum_conservation_tillage"
    )
  )

  # -------------------------------------------------------------------------
  parking_lot_land_cover <-
    calc_parking_lot_land_cover(
      tb = tb,
      .selected_ctu = .selected_ctu,
      detail = detail,
      .tree_planting_intervention = .tree_planting_intervention,
      .tree_planting_per_capita = .tree_planting_per_capita,
      .tree_planting_per_hectare = .tree_planting_per_hectare,
      .parking_lot_reduction_percentage = .parking_lot_reduction_percentage,
      .urban_form_scenario = .urban_form_scenario
    )


  # -------------------------------------------------------------------------
  baseline_bau <-
    parking_lot_land_cover %>%
    right_join(.,
      tb$ctu_county,
      by = "ctu_name"
    ) %>%
    right_join(.,
      tb$current_conservation_tillage_county,
      by = "co_name"
    ) %>%
    dplyr::select(ctu_name, co_name, year, agriculture) %>%
    tidyr::pivot_wider(
      names_from = year,
      values_from = agriculture,
      names_prefix = "agriculture_hectares_year_"
    ) %>%
    right_join(.,
      tb$current_conservation_tillage_county,
      by = "co_name"
    ) %>%
    dplyr::mutate(
      baseline_carbon_stock_mg_c_per_hectare = agriculture_hectares_year_2016 *
        enviro_factors$AGRI_LAND_CARBON_STOCK,
      bau_carbon_stock_without_conservation_tillage_mg_c_per_hectare =
        agriculture_hectares_year_2040 *
          enviro_factors$AGRI_LAND_CARBON_STOCK
    )


  # -------------------------------------------------------------------------
  conservation_tillage_carbon_stocks_mg_c <-
    if (.conservation_tillage_intervention == "current_conservation_tillage") {
      baseline_bau %>%
        dplyr::mutate(
          conservation_tillage_mg_c =
            (
              agriculture_hectares_year_2040 * current_conservation_tillage_percent *
                enviro_factors$MAX_SOC_ACCUMULATION_UNDER_REDUCED_OR_NO_TILL_AGRI_PCT *
                enviro_factors$AGRI_LAND_CARBON_STOCK
            )
            + (
                agriculture_hectares_year_2040 * (1 - current_conservation_tillage_percent)
              )
              * enviro_factors$AGRI_LAND_CARBON_STOCK,
          carbon_stock_change_from_conservation_ag_mg_c =

            conservation_tillage_mg_c -
              bau_carbon_stock_without_conservation_tillage_mg_c_per_hectare,
          reduced_tractor_emissions_relative_to_current_conservation_tillage_mg_co2e_per_year =
            0
        )
    } else if (.conservation_tillage_intervention == "double_conservation_tillage") {
      baseline_bau %>%
        dplyr::mutate(
          conservation_tillage_mg_c =
            (
              agriculture_hectares_year_2040 * (current_conservation_tillage_percent * 2) *
                enviro_factors$MAX_SOC_ACCUMULATION_UNDER_REDUCED_OR_NO_TILL_AGRI_PCT *
                enviro_factors$AGRI_LAND_CARBON_STOCK
            )
            + (agriculture_hectares_year_2040 * (
                1 - (current_conservation_tillage_percent * 2)
              ))
              * enviro_factors$AGRI_LAND_CARBON_STOCK,
          carbon_stock_change_from_conservation_ag_mg_c =

            conservation_tillage_mg_c -
              bau_carbon_stock_without_conservation_tillage_mg_c_per_hectare,
          # tractor
          reduced_tractor_emissions_relative_to_current_conservation_tillage_mg_co2e_per_year =
            (
              agriculture_hectares_year_2040 *
                enviro_factors$AVOIDED_EMISSIONS_TRACTOR_USE *
                ((current_conservation_tillage_percent * 2) -
                  (current_conservation_tillage_percent)
                )
            ) * -1
        )
    } else if (.conservation_tillage_intervention == "maximum_conservation_tillage") {
      baseline_bau %>%
        dplyr::mutate(
          conservation_tillage_mg_c =
            (
              agriculture_hectares_year_2040 *
                enviro_factors$MAX_SOC_ACCUMULATION_UNDER_REDUCED_OR_NO_TILL_AGRI_PCT *
                enviro_factors$AGRI_LAND_CARBON_STOCK
            ),
          carbon_stock_change_from_conservation_ag_mg_c =

            conservation_tillage_mg_c -
              bau_carbon_stock_without_conservation_tillage_mg_c_per_hectare,
          # tractor
          reduced_tractor_emissions_relative_to_current_conservation_tillage_mg_co2e_per_year =
            (
              (
                agriculture_hectares_year_2040 *
                  enviro_factors$AVOIDED_EMISSIONS_TRACTOR_USE
              ) *
                (1 - current_conservation_tillage_percent) * -1
            )
        )
    }


  # -------------------------------------------------------------------------


  return(conservation_tillage_carbon_stocks_mg_c)
}
