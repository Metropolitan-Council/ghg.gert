#' @title Calculate carbon stock by city/township
#' @family land use
#' @family emissions
#'
#' @description Calculates the carbon stock per land cover type by city/township
#'      under the selected scenario parameters
#'
#' @inheritParams calc_parking_lot_land_cover
#'
#' @return [tibble::tibble()] with column names...
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' calc_carbon_stock_per_ctu(
#'   tb = land_use_data,
#'   .selected_ctu = "all",
#'   .urban_form_scenario = "bau",
#'   .tree_planting_intervention = "tree_planting_on_all_pervious",
#'   .tree_planting_per_capita = 0.26,
#'   .tree_planting_per_hectare = 247,
#'   .parking_lot_reduction_percentage = 0.8,
#'   .conservation_tillage_intervention = "current_conservation_tillage",
#'   detail = FALSE
#' )
#' }
calc_carbon_stock_per_ctu <- function(tb,
                                      .selected_ctu,
                                      .urban_form_scenario,
                                      .tree_planting_intervention,
                                      .tree_planting_per_capita,
                                      .tree_planting_per_hectare,
                                      .parking_lot_reduction_percentage,
                                      .conservation_tillage_intervention,
                                      detail) {

  cli::cli_progress_message("** calculating carbon stock \n")

  match.arg(
    arg = .conservation_tillage_intervention,
    choices = c(
      "current_conservation_tillage",
      "double_conservation_tillage",
      "maximum_conservation_tillage"
    )
  )

  tb$ctu_forecast <- filter_ctu(tb$ctu_forecast, .selected_ctu = .selected_ctu)

  tb$ctu_land_use_hectares <- filter_ctu(tb$ctu_land_use_hectares,
                                         .selected_ctu = .selected_ctu)

  tb$ctu_land_use_2016_land_cover <- filter_ctu(tb$ctu_land_use_2016_land_cover,
                                                .selected_ctu = .selected_ctu)

  tb$ctu_county <- filter_ctu(tb$ctu_county, .selected_ctu = .selected_ctu)

  csf <- carbon_stock_factors

  # -------------------------------------------------------------------------

  # conservation_tillage <-
  #   calc_conservation_tillage(
  #     tb = tb,
  #     detail = detail,
  #     .urban_form_scenario = .urban_form_scenario,
  #     .tree_planting_intervention = .tree_planting_intervention,
  #     .conservation_tillage_intervention = .conservation_tillage_intervention,
  #     .parking_lot_reduction_percentage = .parking_lot_reduction_percentage,
  #     .tree_planting_per_capita = .tree_planting_per_capita,
  #     .tree_planting_per_hectare = .tree_planting_per_hectare
  #   ) %>%
  #   dplyr::select(ctu_name,
  #                 conservation_tillage_mg_c,
  #                 carbon_stock_change_from_conservation_ag_mg_c,
  #                 reduced_tractor_emissions_relative_to_current_conservation_tillage_mg_co2e_per_year)


  # -------------------------------------------------------------------------

  parking_lot_land_cover <-
    calc_parking_lot_land_cover(
      tb = tb,
      .selected_ctu = .selected_ctu,
      .urban_form_scenario = .urban_form_scenario,
      .tree_planting_intervention = .tree_planting_intervention,
      .tree_planting_per_capita = .tree_planting_per_capita,
      .tree_planting_per_hectare = .tree_planting_per_hectare,
      .parking_lot_reduction_percentage = .parking_lot_reduction_percentage,
      detail = detail
  )

  # -------------------------------------------------------------------------

  baseline_bau <-
    parking_lot_land_cover %>%
    dplyr::left_join(.,
      tb$ctu_county,
      by = "ctu_name"
    ) %>%
    dplyr::left_join(.,
      tb$current_conservation_tillage_county,
      by = "co_name"
    )
  # %>%
    #dplyr::select(ctu_name, co_name, year, agriculture) %>%
    # tidyr::pivot_wider(
    #   names_from = year,
    #   values_from = agriculture,
    #   names_prefix = "agriculture_hectares_year_"
    # ) %>%
    # dplyr::right_join(.,
    #   tb$current_conservation_tillage_county,
    #   by = "co_name"
    # ) %>%
    # dplyr::mutate(
    #   baseline_carbon_stock_mg_c_per_hectare = agriculture_hectares_year_2016 *
    #     enviro_factors$AGRI_LAND_CARBON_STOCK,
    #   bau_carbon_stock_without_conservation_tillage_mg_c_per_hectare =
    #     agriculture_hectares_year_2040 *
    #       enviro_factors$AGRI_LAND_CARBON_STOCK
    # )

  # -------------------------------------------------------------------------

  carbon_stock_per_ctu <- baseline_bau %>%
    dplyr::mutate(
      grass = grass * csf$GRASS_STOCK_MG_C_PER_HECTARE,
      impervious = impervious * csf$IMPERVIOUS_STOCK_MG_C_PER_HECTARE,
      trees = trees * csf$TREES_STOCK_MG_C_PER_HECTARE,
      water = water * csf$WATER_STOCK_MG_C_PER_HECTARE,
      barren = barren * csf$BARREN_STOCK_MG_C_PER_HECTARE,
      forest = forest * csf$FOREST_STOCK_MG_C_PER_HECTARE,
      shrub = shrub * csf$SHRUB_STOCK_MG_C_PER_HECTARE,
      grassland = grassland * csf$GRASSLAND_STOCK_MG_C_PER_HECTARE,
      #agriculture = agriculture * csf$AGRICULTURE_STOCK_MG_C_PER_HECTARE,
      agriculture = switch(
        .conservation_tillage_intervention,
        "current_conservation_tillage" = {(
          (
            agriculture * current_conservation_tillage_percent *
              enviro_factors$MAX_SOC_ACCUMULATION_UNDER_REDUCED_OR_NO_TILL_AGRI_PCT *
              csf$AGRICULTURE_STOCK_MG_C_PER_HECTARE
          )
          + (agriculture * (1 - current_conservation_tillage_percent))
          * csf$AGRICULTURE_STOCK_MG_C_PER_HECTARE
        )},
        "double_conservation_tillage" = {(
          (
            agriculture * (current_conservation_tillage_percent * 2) *
              enviro_factors$MAX_SOC_ACCUMULATION_UNDER_REDUCED_OR_NO_TILL_AGRI_PCT *
              csf$AGRICULTURE_STOCK_MG_C_PER_HECTARE
          )
          + (agriculture * (1 - (current_conservation_tillage_percent * 2)))
          * csf$AGRICULTURE_STOCK_MG_C_PER_HECTARE
          )},
        "maximum_conservation_tillage" = {(
            agriculture_hectares_year_2040 *
              enviro_factors$MAX_SOC_ACCUMULATION_UNDER_REDUCED_OR_NO_TILL_AGRI_PCT *
              csf$AGRICULTURE_STOCK_MG_C_PER_HECTARE
          )}
      ),
      woody_wetland = woody_wetland * csf$WOODY_WETLAND_STOCK_MG_C_PER_HECTARE,
      wetland = wetland * csf$WETLAND_STOCK_MG_C_PER_HECTARE,
      parking_lot = parking_lot * csf$PARKING_LOT_STOCK_MG_C_PER_HECTARE
    ) %>%
    dplyr::mutate(var = "cumulative_stock_tonnes_c") %>%
    dplyr::select(
      ctu_name,
      year,
      var,
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
      woody_wetland
    )


  # -------------------------------------------------------------------------

  carbon_stock_per_ctu %>%
    dplyr::group_by(ctu_name) %>%
    tidyr::pivot_wider(values_from =  c(grass,
                                        impervious,
                                        trees,
                                        water,
                                        barren,
                                        forest,
                                        shrub,
                                        grassland,
                                        agriculture,
                                        woody_wetland,
                                        wetland,
                                        parking_lot),
                       names_from = "year") %>%
    dplyr::transmute(
      grass = ((grass_2016 - grass_2040) * 11/3)/24,
      impervious = ((impervious_2040 - impervious_2016) * 11/3)/24,
      trees = ((trees_2016 - trees_2040) * 11/3)/24,
      water = ((water_2016 - water_2040) * 11/3)/24,
      barren = ((barren_2016 - barren_2040) * 11/3)/24,
      forest = ((forest_2016 - forest_2040) * 11/3)/24,
      shrub = ((shrub_2016 - shrub_2040) * 11/3)/24,
      grassland = ((grassland_2016 - grassland_2040) * 11/3)/24,
      agriculture = ((agriculture_2016 - agriculture_2040) * 11/3)/24,
      woody_wetland = ((woody_wetland_2016 - woody_wetland_2040) * 11/3)/24,
      wetland = ((wetland_2016 - wetland_2040) * 11/3)/24,
      parking_lot = ((parking_lot_2016 - parking_lot_2040) * 11/3)/24
    )

  # -------------------------------------------------------------------------

  return(carbon_stock_per_ctu)

  }
