#' @title Calculate carbon stock by city/township
#' @family land use
#' @family emissions
#'
#' @description This function estimates the carbon stock per land cover type for
#'    each city or township under a given user-selected scenario. It takes into
#'    account factors such as urban form, tree planting interventions, parking
#'    lot reduction percentages, and conservation tillage interventions.
#'
#' @param .conservation_tillage_intervention character,
#'    The type of conservation tillage scenario to be explored.
#'    Default is `current_conservation_tillage`. The options are:
#'    * `"current_conservation_tillage"` it maintains the per county levels of conservation tillage
#'    relative to the baseline year.
#'    * `"double_conservation_tillage"` it doubles the per county levels of conservation tillage
#'    relative to the baseline year.
#'    * `"maximum_conservation_tillage"` it assumes that all agricultural land implements conservation
#'    tillage.
#'
#' @inheritParams calc_parking_lot_land_cover
#' @inheritParams run_all_modules
#' @inheritParams run_scenario_land_use
#' @inheritParams filter_ctu
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
                                      .tree_planting_intervention,
                                      .tree_planting_per_capita,
                                      .tree_planting_per_hectare,
                                      .parking_lot_reduction_percentage,
                                      .conservation_tillage_intervention,
                                      detail,
                                      .enviro_factors = ghg.sp::enviro_factors) {
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
    .selected_ctu = .selected_ctu
  )

  tb$ctu_land_use_2016_land_cover <- filter_ctu(tb$ctu_land_use_2016_land_cover,
    .selected_ctu = .selected_ctu
  )

  tb$ctu_county <- filter_ctu(tb$ctu_county, .selected_ctu = .selected_ctu)

  csf <- ghg.sp::carbon_stock_factors

  # -------------------------------------------------------------------------

  parking_lot_land_cover <-
    calc_parking_lot_land_cover(
      tb = tb,
      .selected_ctu = .selected_ctu,
      .tree_planting_intervention = .tree_planting_intervention,
      .tree_planting_per_capita = .tree_planting_per_capita,
      .tree_planting_per_hectare = .tree_planting_per_hectare,
      .parking_lot_reduction_percentage = .parking_lot_reduction_percentage,
      detail = detail,
      .enviro_factors = .enviro_factors
    )

  # -------------------------------------------------------------------------

  baseline_bau <-
    parking_lot_land_cover %>%
    dplyr::left_join(.,
      tb$ctu_county,
      by = "ctu_name",
      multiple = "all"
    ) %>%
    dplyr::left_join(.,
      tb$current_conservation_tillage_county,
      by = "co_name",
      multiple = "all"
    )

  # -------------------------------------------------------------------------

  calculate_stock <- function(land_use,
                              year,
                              stock_factor,
                              # column_name,
                              # agriculture = FALSE,
                              tillage_pct = 0,
                              .this_enviro_factors = .enviro_factors) {
      case_when(year < 2040 ~ ((land_use *
                                 tillage_pct *
                                 .this_enviro_factors$MAX_SOC_ACCUMULATION_UNDER_REDUCED_OR_NO_TILL_AGRI_PCT *
                                 stock_factor) +
                                 (land_use * (1 - tillage_pct) * stock_factor)),
                .conservation_tillage_intervention == "current_conservation_tillage" ~ (
                  (land_use *
                     tillage_pct *
                     .this_enviro_factors$MAX_SOC_ACCUMULATION_UNDER_REDUCED_OR_NO_TILL_AGRI_PCT *
                     stock_factor) +
                    (land_use * (1 - tillage_pct) * stock_factor)
                  ),
                .conservation_tillage_intervention == "double_conservation_tillage" ~ (
                  (land_use * dplyr::if_else((tillage_pct * 2) < 1, tillage_pct * 2, 1) *
                     .this_enviro_factors$MAX_SOC_ACCUMULATION_UNDER_REDUCED_OR_NO_TILL_AGRI_PCT * stock_factor) +
                    (land_use * (1 - dplyr::if_else((tillage_pct * 2) < 1, tillage_pct * 2, 1)) * stock_factor)
                  ),
                .conservation_tillage_intervention == "maximum_conservation_tillage" ~ (
                  land_use *
                    .this_enviro_factors$MAX_SOC_ACCUMULATION_UNDER_REDUCED_OR_NO_TILL_AGRI_PCT * stock_factor)
      )
    }

  carbon_stock_per_ctu <- baseline_bau %>%
    dplyr::mutate(dplyr::across(
      c(grass, impervious, trees, water, barren, forest, shrub, grassland, woody_wetland, wetland, parking_lot),
      ~ . * csf[[toupper(dplyr::cur_column()) %>% paste0("_STOCK_MG_C_PER_HECTARE")]], .names = "{col}")) %>%
    dplyr::mutate(agriculture = calculate_stock(
        land_use = agriculture,
        year = year,
        stock_factor = csf[["AGRICULTURE_STOCK_MG_C_PER_HECTARE"]],
        # column_name = dplyr::cur_column(),
        # agriculture = dplyr::cur_column() == "agriculture",
        tillage_pct = current_conservation_tillage_percent,
        .this_enviro_factors = .enviro_factors
    )) %>%
    dplyr::select(
      ctu_name,
      year,
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
  carbon_stock_per_ctu <-
    carbon_stock_per_ctu %>%
    dplyr::group_by(ctu_name, year) %>%
    tidyr::pivot_wider(
      values_from = c(
        grass,
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
        parking_lot
      ),
      names_from = "year",
      values_fn = sum
    ) %>%
    dplyr::transmute(
      grass = ((grass_2016 - grass_2040) * 11 / 3) / 24,
      impervious = ((impervious_2040 - impervious_2016) * 11 / 3) / 24,
      trees = ((trees_2016 - trees_2040) * 11 / 3) / 24,
      water = ((water_2016 - water_2040) * 11 / 3) / 24,
      barren = ((barren_2016 - barren_2040) * 11 / 3) / 24,
      forest = ((forest_2016 - forest_2040) * 11 / 3) / 24,
      shrub = ((shrub_2016 - shrub_2040) * 11 / 3) / 24,
      grassland = ((grassland_2016 - grassland_2040) * 11 / 3) / 24,
      agriculture = ((agriculture_2016 - agriculture_2040) * 11 / 3) / 24,
      woody_wetland = ((woody_wetland_2016 - woody_wetland_2040) * 11 /
        3) / 24,
      wetland = ((wetland_2016 - wetland_2040) * 11 / 3) / 24,
      parking_lot = ((parking_lot_2016 - parking_lot_2040) * 11 / 3) / 24
    ) %>%
    dplyr::mutate(year = 2040)

  # -------------------------------------------------------------------------

  return(carbon_stock_per_ctu)
}
