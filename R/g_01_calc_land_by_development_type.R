#' @title Calculate land by development type
#' @family land use
#'
#' @description Calculates the hectares of
#'      land by different development types
#'      (urban expansion, urban infill, and exurban development) for  the selected land
#'      use (urban form) scenario at the city/township scale .
#'
#' @param tb [tibble::tibble()].
#' The input dataset to be used.
#' @param .urban_form_scenario character,
#' The current land use scenario being explored.
#' Default is `"bau"`.
#' The options are:
#' * `"bau"`: the business as usual scenario.
#' * `"post_covid_sprawl"`: post covid sprawl scenario.
#' * `"compact_development_beyond_bau`: compact development beyond the business as usual scenario.
#' * `"compact_development_with_drs"`: compact development with dynamic ride sharing.
#' @return [tibble::tibble()].
#'      A list of tables with the estimated hectares by different development
#'      types (urban expansion, urban infill, and exurban development) for different
#'      land use scenarios.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' calc_land_by_development_type(
#'   tb = land_use_data,
#'   .urban_form_scenario = "bau"
#' )
#' }
calc_land_by_development_type <- function(tb,
                                          .urban_form_scenario) {
  match.arg(
    arg = .urban_form_scenario,
    choices = c(
      "bau",
      "post_covid_sprawl",
      "compact_dev_with_drs",
      "compact_dev_beyond_bau"
    )
  )

  luse_scenario_params <- tb$scenario_parameters %>%
    dplyr::filter(scenario_description_2 == .urban_form_scenario)

  p_land_by_development_type <- c()

  # BAU Total
  land_by_development_type$bau_total <-
    tb$ctu_land_use_hectares %>%
    dplyr::filter(year == 2040) %>%
    dplyr::group_by(ctu_name, development_type) %>%
    dplyr::summarise(hectares = sum(hectares), .groups = "drop") %>%
    dplyr::mutate(scenario = "bau")

  # BAU Mixed Use/Compact Zoning/Park
  land_by_development_type$bau_mixed_use_compact_zoning_park <-
    p_multifamily_mixed_area %>%
    dplyr::mutate(scenario = "bau_mixed_use_compact_zoning")

  # BAU Mixed Use Compact Zoning Park ----
  bau_mixed_use_compact_zoning_park <-
    land_by_development_type$bau_mixed_use_compact_zoning_park

  # Scenario Mixed Use Compact Zoning Park-----
  scenario_mixed_use_compact_zoning_park <-
    (tb$p_multifamily_mixed_area <-
       tb$ctu_land_use_hectares %>%
       dplyr::group_by(ctu_name, development_type) %>%
       dplyr::filter(year == 2040) %>%
       dplyr::filter(
         land_use_type %in% c(
           "park_recreational_or_preserve",
           "mixed_use_commercial",
           "mixed_use_industrial",
           "mixed_use_residential",
           "multifamily"
         )
       ) %>%
       dplyr::summarise(hectares = sum(hectares), .groups = "drop") %>%
       dplyr::ungroup())%>%
    dplyr::mutate(
      hectares =
        dplyr::if_else(
          development_type == "urban_expansion",
          hectares * luse_scenario_params$urban_expansion_relative_to_bau,
          hectares
        )
    ) %>%
    dplyr::mutate(scenario = "scenario_mixed_use_compact_zoning_park")

  # Scenario Total ----
  scenario_total <-
    land_by_development_type$bau_total %>%
    base::merge(
      .,
      (
        tb$ctu_land_use_hectares %>%
          dplyr::filter(year == 2040) %>%
          dplyr::group_by(ctu_name) %>%
          dplyr::summarise(total_hectares_bau = sum(hectares)) %>%
          dplyr::ungroup()
      ),
      by = "ctu_name"
    ) %>%
    tidyr::pivot_wider(.,
                       names_from = development_type,
                       values_from = hectares) %>%
    dplyr::mutate(
      urban_expansion =
        dplyr::if_else
      (
        urban_expansion * luse_scenario_params$urban_expansion_relative_to_bau < total_hectares_bau - urban_infill,
        urban_expansion * luse_scenario_params$urban_expansion_relative_to_bau,
        total_hectares_bau - urban_infill
      )
    ) %>%
    dplyr::mutate(exurban_development =
                    total_hectares_bau - (urban_infill) - (urban_expansion)) %>%
    dplyr::mutate(scenario = "scenario_total") %>%
    select(.,-c(total_hectares_bau)) %>%
    tidyr::pivot_longer(.,
                        cols = 3:5,
                        names_to = "development_type",
                        values_to = "hectares")

  # Scenario Mixed Use MF (new) ----
  scenario_mixed_use_mf_new_1 <-
    bind_rows(bau_total,
              scenario_total,
              scenario_mixed_use_compact_zoning_park) %>%
    tidyr::pivot_wider(
      .,
      names_from = c(scenario, development_type),
      values_from = hectares,
      names_sep = "."
    ) %>%
    dplyr::mutate(
      scenario_mixed_use_mf_new.urban_expansion = (
        scenario_total.urban_expansion - scenario_mixed_use_compact_zoning_park.urban_expansion
      ) *
        luse_scenario_params$urban_expansion
    )

  # Scenario Mixed Use Multifamily New ----
  scenario_mixed_use_mf_new <-
    (if (.urban_form_scenario == "compact_dev_with_drs") {
      scenario_mixed_use_mf_new_1 %>%
        dplyr::mutate(
          scenario_mixed_use_mf_new.urban_infill =
            dplyr::if_else(
              bau.urban_expansion > bau.urban_infill,
              bau.urban_infill,
              bau.urban_expansion
            ),
          scenario_mixed_use_mf_new.exurban_development = 0
        ) %>%
        tidyr::pivot_longer(
          .,
          cols = !ctu_name,
          names_to = c("scenario", "development_type"),
          names_sep = "[.]",
          values_to = "hectares"
        ) %>%
        filter(scenario == "scenario_mixed_use_mf_new")
    } else {
      scenario_mixed_use_mf_new_1 %>%
        dplyr::mutate(
          scenario_mixed_use_mf_new.urban_infill =
            ((
              scenario_total.urban_infill - scenario_mixed_use_compact_zoning_park.urban_infill
            )
            * luse_scenario_params$urban_infill
            ),
          scenario_mixed_use_mf_new.exurban_development = 0
        ) %>%
        tidyr::pivot_longer(
          .,
          cols = !ctu_name,
          names_to = c("scenario", "development_type"),
          names_sep = "[.]",
          values_to = "hectares"
        ) %>%
        filter(scenario == "scenario_mixed_use_mf_new")
    })

  # Scenario Other Zoning -----
  scenario_other_zoning <-
    bind_rows(
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
      cols = !ctu_name,
      names_to = c("scenario", "development_type"),
      names_sep = "[.]",
      values_to = "hectares"
    ) %>%
    filter(scenario == "scenario_other_zoning")

  # return -----
  new_land_by_development_type <-
    bind_rows(
      bau_total,
      bau_mixed_use_compact_zoning_park,
      scenario_mixed_use_compact_zoning_park,
      scenario_mixed_use_mf_new,
      scenario_other_zoning,
      scenario_total
    )
  return(new_land_by_development_type)
}
