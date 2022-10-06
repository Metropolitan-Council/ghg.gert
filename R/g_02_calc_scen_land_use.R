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
#'   .urban_form_scenario = "bau"
#' )
#' }
calc_scen_land_use <- function(tb,
                               .urban_form_scenario) {
  luse_scenario_params <- tb$scenario_parameters %>%
    dplyr::filter(scenario_description_2 == .urban_form_scenario)

  calc_land_by_development_type(
    tb = tb,
    .urban_form_scenario = .urban_form_scenario
  ) %>%
    # increase mixed use / residential
    tidyr::pivot_wider(
      data = .,
      id_cols = c(ctu_name, development_name),
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
    ) %>%
    base::merge(
      .,
      # tb$land_composition_ctu %>%
      #   group_by(ctu_name, development_name, land_use_description) %>%
      #   pivot_longer(cols = c("hectares", "total_hectares", "percent")) %>%
      #   pivot_wider(names_from = c("name", "year"), values_from = "value", names_sep = ".", values_fn = sum)


      (tb$land_composition_ctu %>%
        dplyr::filter(year == 2040)),
      by = c(
        "ctu_name",
        "development_name"
      )
    ) %>%
    dplyr::mutate(
      scenario_hectares =
        dplyr::case_when(
          (
            land_use_description %in% c(
              "multifamily",
              "mixed_use_residential",
              "mixed_use_industrial",
              "mixed_use_commercial"
            )
          ) ~ ((hectares + ((percent * scenario_total) - hectares
          )) * luse_scenario_params$urban_expansion_relative_to_bau
            + (scenario_mixed_use_mf_new / 4)),
          (land_use_description == "park_recreational_or_preserve") ~
          ((hectares + ((percent * scenario_total) - hectares))
          * luse_scenario_params$urban_expansion_relative_to_bau),
          land_use_description %in% c(
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
          ) ~ (((
            hectares + ((percent * scenario_total) - hectares)
          ) * luse_scenario_params$urban_expansion_relative_to_bau)
          * scaling_factor)
        )
    ) %>%
    dplyr::select(c(
      "ctu_name",
      "development_name",
      "land_use_description",
      "scenario_hectares"
    )) %>%
    dplyr::group_by(
      ctu_name,
      development_name,
      land_use_description
    ) %>%
    dplyr::summarise(
      scenario_hectares = sum(scenario_hectares),
      .groups = "drop"
    ) %>%
    dplyr::group_by(ctu_name)
}
