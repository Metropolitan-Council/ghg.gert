#' Calculate Scenario Land Use
#'
#' @description For the selected scenario, this function calculates the land use type in hectares
#' for all cities/townships
#'
#' @inheritParams calc_land_by_development_type
#'
#' @family land_use_module
#'
#' @return
#' @export
#'
#' @examples
calc_scen_land_use <- function(tb = land_use_data) {
  bind_rows(
    calc_land_by_development_type()$scenario_mixed_use_mf_new,
    calc_land_by_development_type()$scenario_other_zoning,
    calc_land_by_development_type()$scenario_total
  ) %>%
    # Increase mixed use / residential
    tidyr::pivot_wider(
      data = .,
      id_cols = c(ctu_name, development_name),
      names_from = c(scenario),
      values_from = c(hectares)
    ) %>%
    dplyr::mutate(
      scaling_factor =
        dplyr::if_else(
          scenario_mixed_use_mf_new > 0,
          scenario_other_zoning / scenario_total,
          1
        )
    ) %>%
    base::merge(.,
                (tb$land_composition_ctu %>%
                   filter(year == 2016)),
                by = c("ctu_name",
                       "development_name")) %>%
    dplyr::mutate(scenario_hectares =
                    dplyr::if_else((
                      description_2 %in% c(
                        "multifamily",
                        "mixed_use_residential",
                        "mixed_use_industrial",
                        "mixed_use_commercial"
                      )
                    ),

                    ((hectares + ((
                      percent * scenario_total
                    ) - hectares)) + (scenario_mixed_use_mf_new / 4)
                    ),

                    ((
                      hectares + ((percent * scenario_total) - hectares)
                    ) * scaling_factor))) %>%
    dplyr::select(c(
      "ctu_name",
      "development_name",
      "description_2",
      "scenario_hectares"
    )) %>%
    dplyr::group_by(ctu_name, description_2) %>%
    dplyr::summarise(scenario_hectares = sum(scenario_hectares),
                     .groups = 'drop') %>%
    dplyr::group_by(ctu_name)
}
