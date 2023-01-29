#' @title Calculate scenario land use
#' @family land use
#'
#' @description Calculates the land use type in hectares
#'       for all cities/townships for the selected scenario.
#'
#' @inheritParams calc_land_by_development_type
#'
#'
#' @return [tibble::tibble()] with column names...
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' calc_scen_land_use(
#'   tb = land_use_data,
#'   .selected_ctu = "all",
#'   .urban_form_scenario = "bau"
#' )
#' }
calc_scen_land_use <- function(tb,
                               .selected_ctu,
                               .urban_form_scenario) {
  cat("**** calculating land use urban form scneario \n")
  # -------------------------------------------------------------------------

  calc_land_by_development_type <-
    calc_land_by_development_type(
      tb = tb,
      .selected_ctu = .selected_ctu,
      .urban_form_scenario = .urban_form_scenario
    )

  # -------------------------------------------------------------------------

  luse_scenario_params <- tb$scenario_parameters %>%
    dplyr::filter(scenario_description_2 == .urban_form_scenario)


  # -------------------------------------------------------------------------

  get_total_hectares_by_development_type <-
    tb$ctu_land_use_hectares %>%
    dplyr::group_by(ctu_name, development_type, year) %>%
    dplyr::summarise(
      total_hectares = sum(hectares, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup()

  # -------------------------------------------------------------------------

  merge_datasets_hecates_by_dev_type_with_land_use_land_cover <-
    (
      tb$ctu_land_use_hectares %>%
        group_by(ctu_name, development_type, year, land_use_type) %>%
        summarise(
          hectares = sum(hectares, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        base::merge(
          .,
          (get_total_hectares_by_development_type),
          by = c("ctu_name", "development_type", "year")
        ) %>%
        dplyr::mutate(
          percent_of_hectares =
            hectares /
              total_hectares
        )
    ) %>%
    dplyr::filter(year == 2040)

  # -------------------------------------------------------------------------

  add_scaling_factor <- calc_land_by_development_type %>%
    tidyr::pivot_wider(
      data = .,
      id_cols = c(ctu_name, development_type),
      names_from = c(scenario),
      values_from = c(hectares)
    ) %>%
    # scaling factor
    dplyr::mutate(
      scaling_factor =
        dplyr::if_else(
          scenario_mixed_use_mf_new > 0,
          scenario_other_zoning / scenario_total,
          1
        )
    )

  # -------------------------------------------------------------------------

  scen_land_use <-
    add_scaling_factor %>%
    base::merge(
      .,
      (
        merge_datasets_hecates_by_dev_type_with_land_use_land_cover
      ),
      by = c(
        "ctu_name",
        "development_type"
      )
    ) %>%
    dplyr::mutate(
      scenario_hectares =
        dplyr::case_when(
          (
            land_use_type %in% c(
              "multifamily",
              "mixed_use_residential",
              "mixed_use_industrial",
              "mixed_use_commercial"
            )
          ) ~ ((hectares + ((percent_of_hectares * scenario_total) - hectares
          )) * luse_scenario_params$urban_expansion_relative_to_bau
            + (scenario_mixed_use_mf_new / 4)
          ),
          (land_use_type == "park_recreational_or_preserve") ~
            ((hectares + ((percent_of_hectares * scenario_total) - hectares
            ))
            * luse_scenario_params$urban_expansion_relative_to_bau
            ),
          land_use_type %in% c(
            "agricultural",
            "airport",
            "extractive",
            "farmstead",
            "golf_course",
            "industrial_and_utility",
            "institutional",
            "major_highway",
            "major_railway",
            "manufactured_housing_park",
            "office",
            "open_water",
            "railway",
            "retail_and_other_commercial",
            "seasonal_vacation",
            "single_family_attached",
            "single_family_detached",
            "undeveloped"
          ) ~ (((hectares + ((percent_of_hectares * scenario_total) - hectares
          )) * luse_scenario_params$urban_expansion_relative_to_bau
          )
          * scaling_factor
          )
        )
    ) %>%
    dplyr::select(c(
      "ctu_name",
      "development_type",
      "land_use_type",
      "scenario_hectares"
    )) %>%
    dplyr::group_by(
      ctu_name,
      development_type,
      land_use_type
    ) %>%
    dplyr::summarise(
      scenario_hectares = sum(scenario_hectares),
      .groups = "drop"
    ) %>%
    dplyr::group_by(ctu_name)

  # -------------------------------------------------------------------------

  return(scen_land_use)
}
