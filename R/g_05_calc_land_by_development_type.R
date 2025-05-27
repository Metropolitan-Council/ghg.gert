#' @title Calculate land by development type
#' @family land use
#'
#' @description This function calculates the distribution of land across different
#'      development types (urban expansion, urban infill, and exurban development) for
#'      selected land use (urban form) scenarios at the city/township scale. The function
#'      considers various urban form scenarios, such as business as usual, post-COVID sprawl,
#'      compact development beyond business as usual, and compact development with dynamic
#'      ride-sharing. The output is a tibble containing the estimated hectares for each
#'      development type under the specified land use scenario.
#'
#' @param tb [tibble::tibble()].
#' The input dataset to be used.
#'
#' @param .urban_form_scenario character,
#' The current land use scenario being explored.
#' Default is `"bau"`.
#' The options are:
#' * `"bau"`: the business as usual scenario.
#' * `"post_covid_sprawl"`: post covid sprawl scenario.
#' * `"compact_development_beyond_bau`: compact development beyond the business as usual scenario.
#' * `"compact_development_with_drs"`: compact development with dynamic ride sharing.
#'
#'
#' @inheritParams calc_parking_lot_land_cover
#' @inheritParams run_all_modules
#' @inheritParams run_scenario_land_use
#' @inheritParams filter_ctu
#'
#' @return [tibble::tibble()].
#'      A tibble containing the estimated hectares by different development
#'      types (urban expansion, urban infill, and exurban development) for different
#'      land use scenarios.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.ccap)
#'
#' calc_land_by_development_type(
#'   tb = land_use_data,
#'   .selected_ctu = "all"
#' )
#' }
calc_land_by_development_type <- function(tb,
                                          .selected_ctu,
                                          .enviro_factors = ghg.ccap::enviro_factors) {
  # -------------------------------------------------------------------------
  ctu_land_use_hectares <- filter_ctu(tb$ctu_land_use_hectares, .selected_ctu)

  # -------------------------------------------------------------------------

  # -------------------------------------------------------------------------
  luse_scenario_params <- tb$scenario_parameters %>%
    dplyr::filter(scenario_description_2 == ghg.ccap::enviro_factors$URBAN_FORM_SCENARIO)

  # -------------------------------------------------------------------------
  ctu_land_use_hectares_by_dev_type <-
    ctu_land_use_hectares %>%
    dplyr::filter(year == 2040) %>%
    dplyr::group_by(geog_name, geog_id, development_type)

  # -------------------------------------------------------------------------
  bau_total <-
    ctu_land_use_hectares_by_dev_type %>%
    dplyr::summarise(
      hectares = sum(hectares),
      .groups = "drop"
    ) %>%
    dplyr::mutate(scenario = "bau")

  # -------------------------------------------------------------------------
  scenario_mixed_use_compact_zoning_park <-
    ctu_land_use_hectares_by_dev_type %>%
    dplyr::filter(
      land_use_type %in% c(
        "park_recreational_or_preserve",
        "mixed_use_commercial",
        "mixed_use_industrial",
        "mixed_use_residential",
        "multifamily"
      )
    ) %>%
    dplyr::summarise(
      hectares = sum(hectares),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      hectares =
        dplyr::if_else(
          development_type == "urban_expansion",
          hectares * luse_scenario_params$urban_expansion_relative_to_bau,
          hectares
        ),
      scenario = "scenario_mixed_use_compact_zoning_park"
    )

  # -------------------------------------------------------------------------
  scenario_total <-
    bau_total %>%
    base::merge(
      .,
      (
        ctu_land_use_hectares %>%
          dplyr::filter(year == 2040) %>%
          dplyr::group_by(geog_name, geog_id) %>%
          dplyr::summarise(total_hectares_bau = sum(hectares)) %>%
          dplyr::ungroup()
      ),
      by = c("geog_name", "geog_id")
    ) %>%
    tidyr::pivot_wider(.,
                       names_from = development_type,
                       values_from = hectares
    ) %>%
    dplyr::mutate(
      urban_expansion =
        dplyr::if_else(
          urban_expansion *
            luse_scenario_params$urban_expansion_relative_to_bau < total_hectares_bau - urban_infill,
          urban_expansion *
            luse_scenario_params$urban_expansion_relative_to_bau,
          total_hectares_bau - urban_infill
        ),
      exurban_development = total_hectares_bau - (urban_infill) - (urban_expansion),
      scenario = "scenario_total"
    ) %>%
    dplyr::select(., -c(total_hectares_bau)) %>%
    tidyr::pivot_longer(.,
                        cols = c(
                          exurban_development,
                          urban_expansion,
                          urban_infill
                        ),
                        names_to = "development_type",
                        values_to = "hectares"
    )

  # -------------------------------------------------------------------------
  # browser()
  scenario_mixed_use_mf_new <-
    dplyr::bind_rows(
      bau_total,
      scenario_total,
      scenario_mixed_use_compact_zoning_park
    ) %>%
    tidyr::pivot_wider(
      .,
      names_from = c(
        scenario,
        development_type
      ),
      values_from = hectares,
      names_sep = "."
    ) %>%
    dplyr::mutate(
      scenario_mixed_use_mf_new.urban_expansion = (
        scenario_total.urban_expansion -
          scenario_mixed_use_compact_zoning_park.urban_expansion
      ) * luse_scenario_params$urban_expansion
    ) %>%
    dplyr::mutate(
      scenario_mixed_use_mf_new.urban_infill =
        # if (.urban_form_scenario == "compact_dev_with_drs") {
        #   dplyr::if_else(bau.urban_expansion > bau.urban_infill,
        #     bau.urban_infill,
        #     bau.urban_expansion
        #   )
        # } else {
        ((
          scenario_total.urban_infill -
            scenario_mixed_use_compact_zoning_park.urban_infill
        )
        * luse_scenario_params$urban_infill
        ),
      scenario_mixed_use_mf_new.exurban_development = 0
    ) %>%
    group_by(geog_name, geog_id) %>%
    tidyr::pivot_longer(
      .,
      cols = any_of(c("scenario_total.exurban_development", "scenario_total.urban_expansion",
                      "scenario_total.urban_infill", "scenario_mixed_use_mf_new.urban_expansion",
                      "scenario_mixed_use_mf_new.urban_infill", "scenario_mixed_use_mf_new.exurban_development",
                      "scenario_mixed_use_compact_zoning_park.exurban_development",
                      "scenario_mixed_use_compact_zoning_park.urban_expansion", "scenario_mixed_use_compact_zoning_park.urban_infill",
                      "scenario_other_zoning.urban_expansion", "scenario_other_zoning.urban_infill",
                      "scenario_other_zoning.exurban_development", "bau.urban_expansion",
                      "bau.urban_infill", "bau.exurban_development")),
      names_to = c("scenario", "development_type"),
      names_sep = "[.]",
      values_to = "hectares"
    ) %>%
    dplyr::filter(scenario == "scenario_mixed_use_mf_new")

  # -------------------------------------------------------------------------
  scenario_other_zoning <-
    dplyr::bind_rows(
      scenario_total,
      scenario_mixed_use_mf_new,
      scenario_mixed_use_compact_zoning_park
    ) %>%
    tidyr::pivot_wider(
      .,
      names_from = c(scenario, development_type),
      values_from = hectares,
      names_sep = "."
    ) %>%
    dplyr::mutate(
      scenario_other_zoning.urban_expansion =
        scenario_total.urban_expansion -
        scenario_mixed_use_mf_new.urban_expansion -
        scenario_mixed_use_compact_zoning_park.urban_expansion,
      scenario_other_zoning.urban_infill =
        scenario_total.urban_infill -
        scenario_mixed_use_mf_new.urban_infill -
        scenario_mixed_use_compact_zoning_park.urban_infill,
      scenario_other_zoning.exurban_development =
        scenario_total.exurban_development -
        scenario_mixed_use_mf_new.exurban_development
    ) %>%
    tidyr::pivot_longer(
      .,
      cols = any_of(c("scenario_total.exurban_development", "scenario_total.urban_expansion",
                      "scenario_total.urban_infill", "scenario_mixed_use_mf_new.urban_expansion",
                      "scenario_mixed_use_mf_new.urban_infill", "scenario_mixed_use_mf_new.exurban_development",
                      "scenario_mixed_use_compact_zoning_park.exurban_development",
                      "scenario_mixed_use_compact_zoning_park.urban_expansion", "scenario_mixed_use_compact_zoning_park.urban_infill",
                      "scenario_other_zoning.urban_expansion", "scenario_other_zoning.urban_infill",
                      "scenario_other_zoning.exurban_development", "bau.urban_expansion",
                      "bau.urban_infill", "bau.exurban_development")),
      names_to = c("scenario", "development_type"),
      names_sep = "[.]",
      values_to = "hectares"
    ) %>%
    dplyr::filter(scenario == "scenario_other_zoning")


  # -------------------------------------------------------------------------
  new_land_by_development_type <-
    dplyr::bind_rows(
      scenario_mixed_use_compact_zoning_park,
      scenario_mixed_use_mf_new,
      scenario_other_zoning,
      scenario_total
    )

  # -------------------------------------------------------------------------
  return(new_land_by_development_type)
}
