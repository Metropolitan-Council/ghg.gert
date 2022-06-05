#' @title Calculate Carbon Sequestration by City/Township
#' @family Land Use
#'
#' @description `calc_carbon_sequestration_per_ctu` calculates the total carbon sequestration
#' per hectares by land cover type by city/township.
#'
#' @inheritParams calc_parking_lot_land_cover
#' @param .impervious_sequest_mg_c_per_hectare_per_year **Numeric**.
#'      Megagrams of carbon sequestration per hectare per year of impervious land cover.
#'      Default is `0` C/ha/year.
#' @param .grass_sequest_mg_c_per_hectare_per_year **Numeric**.
#'      Megagrams of carbon sequestration per hectare per year of grass land cover.
#'      Default is `-0.42` C/ha/year.
#' @param .trees_sequest_mg_c_per_hectare_per_year **Numeric**.
#'      Megagrams of carbon sequestration per hectare per year of trees land cover.
#'      Default is `-1.27` C/ha/year
#' @param .water_sequest_mg_c_per_hectare_per_year **Numeric**.
#'      Megagrams of carbon sequestration per hectare per year of water land cover.
#'      Default is `0` C/ha/year.
#' @param .barren_sequest_mg_c_per_hectare_per_year **Numeric**.
#'      Megagrams of carbon sequestration per hectare per year of barren land cover.
#'      Default is `-0.014` Mg C/ha/year.
#' @param .forest_sequest_mg_c_per_hectare_per_year **Numeric**.
#'      Megagrams of carbon sequestration per hectare per year of forest land cover.
#'      Default is `-0.625` Mg C/ha/year.
#' @param .shrub_sequest_mg_c_per_hectare_per_year **Numeric**.
#'      Megagrams of carbon sequestration per hectare per year of shrub land cover.
#'      Default is `-0.287` Mg C/ha/year.
#' @param .grassland_sequest_mg_c_per_hectare_per_year **Numeric**.
#'      Megagrams of carbon sequestration per hectare per year of grassland land cover.
#'      Default is `-0.287` Mg C/ha/year.
#' @param .agriculture_sequest_mg_c_per_hectare_per_year **Numeric**.
#'      Megagrams of carbon sequestration per hectare per year of agriculture land cover.
#'      Default is `-0.19` Mg C/ha/year.
#' @param .woody_wetland_sequest_mg_c_per_hectare_per_year **Numeric**.
#'      Megagrams of carbon sequestration per hectare per year of woody wetland land cover.
#'      Default is `-0.625` Mg C/ha/year.
#' @param .wetland_sequest_mg_c_per_hectare_per_year **Numeric**.
#'      Megagrams of carbon sequestration per hectare per year of wetland land cover.
#'      Default is `-1.493` Mg C/ha/year.
#' @param .parking_lot_sequest_mg_c_per_hectare_per_year **Numeric**.
#'      Megagrams of carbon sequestration per hectare per year of parking lot land cover.
#'      Default is `0` Mg C/ha/year.
#'
#' @return **Tibble**
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' library(ghg.sp)
#'
#' ghg.sp::calc_carbon_sequestration_per_ctu(
#'      tb
#'      .luse_scen = "compact_dev_with_drs",
#'      .scenario = "bau",
#'      .tree_planting_intervention = "tree_planting_on_all_pervious",
#'      .tree_planting_per_capita = 0.26,
#'      .tree_planting_per_hectare = 247,
#'      .impervious_sequest_mg_c_per_hectare_per_year = 0,
#'      .grass_sequest_mg_c_per_hectare_per_year = -0.42,
#'      .trees_sequest_mg_c_per_hectare_per_year = -1.27,
#'      .water_sequest_mg_c_per_hectare_per_year = 0,
#'      .barren_sequest_mg_c_per_hectare_per_year = -0.014,
#'      .forest_sequest_mg_c_per_hectare_per_year = -0.625,
#'      .shrub_sequest_mg_c_per_hectare_per_year = -0.287,
#'      .grassland_sequest_mg_c_per_hectare_per_year = -0.287,
#'      .agriculture_sequest_mg_c_per_hectare_per_year = -0.19,
#'      .woody_wetland_sequest_mg_c_per_hectare_per_year = -0.625,
#'      .wetland_sequest_mg_c_per_hectare_per_year = -1.493,
#'      .parking_lot_sequest_mg_c_per_hectare_per_year = 0)
#' }
#'
calc_carbon_sequestration_per_ctu <-
  function(.impervious_sequest_mg_c_per_hectare_per_year,
           .grass_sequest_mg_c_per_hectare_per_year,
           .trees_sequest_mg_c_per_hectare_per_year,
           .water_sequest_mg_c_per_hectare_per_year,
           .barren_sequest_mg_c_per_hectare_per_year,
           .forest_sequest_mg_c_per_hectare_per_year,
           .shrub_sequest_mg_c_per_hectare_per_year,
           .grassland_sequest_mg_c_per_hectare_per_year,
           .agriculture_sequest_mg_c_per_hectare_per_year,
           .woody_wetland_sequest_mg_c_per_hectare_per_year,
           .wetland_sequest_mg_c_per_hectare_per_year,
           .parking_lot_sequest_mg_c_per_hectare_per_year) {
    calc_parking_lot_land_cover() %>%
      dplyr::group_by(ctu_name) %>%
      tidyr::pivot_wider(
        names_from = year,
        values_from = !ctu_name,
        names_sep = "."
      ) %>%
      dplyr::mutate(
        impervious = ((impervious.2016 + impervious.2040) / 2) *
          .impervious_sequest_mg_c_per_hectare_per_year,

        grass = ((grass.2016 + grass.2040) / 2) *
          .grass_sequest_mg_c_per_hectare_per_year,

        trees = ((trees.2016 + trees.2040) / 2) *
          .trees_sequest_mg_c_per_hectare_per_year,

        water = ((water.2016 + water.2040) / 2) *
          .water_sequest_mg_c_per_hectare_per_year,

        barren = ((barren.2016 + barren.2040) / 2) *
          .barren_sequest_mg_c_per_hectare_per_year,

        forest = ((forest.2016 + forest.2040) / 2) *
          .forest_sequest_mg_c_per_hectare_per_year,

        shrub = ((shrub.2016 + shrub.2040) / 2) *
          .shrub_sequest_mg_c_per_hectare_per_year,

        grassland = ((grassland.2016 + grassland.2040) / 2) *
          .grassland_sequest_mg_c_per_hectare_per_year,

        agriculture = ((agriculture.2016 + agriculture.2040) / 2) *
          .agriculture_sequest_mg_c_per_hectare_per_year,

        woody_wetland = ((
          woody_wetland.2016 + woody_wetland.2040
        ) / 2) *
          .woody_wetland_sequest_mg_c_per_hectare_per_year,

        wetland = ((wetland.2016 + wetland.2040) / 2) *
          .wetland_sequest_mg_c_per_hectare_per_year,

        parking_lot = ((parking_lot.2016 + parking_lot.2040) / 2) *
          .parking_lot_sequest_mg_c_per_hectare_per_year

      ) %>%
      dplyr::select(
        ctu_name,
        year.2040,
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
  }
