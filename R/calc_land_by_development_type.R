#' Title
#'
#'
#' @param .luse_scen
#'
#' @return
#' @export
#'
#' @examples
calc_land_by_development_type <-
  function(tb = land_use_data,
           .luse_scen = "compact_dev_with_drs",
           .development_name = "") {
    land_by_development_type <- tb$land_by_development_type

    # Scenario Mixed Use Compact Zoning Park
    land_by_development_type$scenario_mixed_use_compact_zoning_park <-
      p_multifamily_mixed_area %>%
      dplyr::mutate(
        hectares =
          dplyr::if_else(
            development_name == "urban_expansion",
            hectares * luse_scenario_params()$urban_expansion_relative_to_bau,
            hectares
          )
      ) %>%
      dplyr::mutate(scenario = "scenario_mixed_use_compact_zoning")

    # Scenario Total
    land_by_development_type$scenario_total <-
      land_by_development_type$bau_total %>%
      base::merge(.,
                  p_land_by_development_type_sum_bau,
                  by = "ctu_name") %>%
      tidyr::pivot_wider(.,
                         names_from = development_name,
                         values_from = hectares) %>%
      dplyr::mutate(
        urban_expansion =
          dplyr::if_else
        (
          urban_expansion * luse_scenario_params()$urban_expansion_relative_to_bau < total_hectares_bau - urban_infill,
          urban_expansion * luse_scenario_params()$urban_expansion_relative_to_bau,
          total_hectares_bau - urban_infill
        )
      ) %>%
      dplyr::mutate(exurban_development =
                      total_hectares_bau - (urban_infill) - (urban_expansion))  %>%
      dplyr::mutate(scenario = "scenario_total") %>%
      select(.,-c(total_hectares_bau)) %>%
      tidyr::pivot_longer(.,
                          cols =  3:5,
                          names_to = "development_name",
                          values_to = "hectares")

    # Scenario Mixed Use MF (new)
    land_by_development_type$scenario_mixed_use_mf_new <-
      bind_rows(
        land_by_development_type$bau_total,
        land_by_development_type$scenario_total,
        land_by_development_type$scenario_mixed_use_compact_zoning_park
      ) %>%
      tidyr::pivot_wider(., names_from = scenario, values_from = hectares) %>%
      dplyr::mutate(
        scenario_mixed_use_mf_new = case_when(
          development_name == "urban_expansion" ~ (scenario_total - scenario_mixed_use_compact_zoning) *
            luse_scenario_params()$urban_expansion_relative_to_bau,
          development_name != "urban_expansion" ~ bau
        )
      )

    land_by_development_type$scenario_mixed_use_mf_new <-
      (if (.luse_scen == "compact_dev_with_drs") {
        land_by_development_type$scenario_mixed_use_mf_new %>%
          dplyr::select(.,
                        ctu_name,
                        development_name,
                        scenario_mixed_use_mf_new) %>%
          tidyr::pivot_wider(.,
                             names_from = development_name,
                             values_from = scenario_mixed_use_mf_new) %>%
          dplyr::mutate(
            urban_infill =
              dplyr::if_else(
                urban_expansion > urban_infill,
                urban_infill,
                urban_expansion
              )
          ) %>%
          tidyr::pivot_longer(.,
                              2:4,
                              names_to = "development_name",
                              values_to = "hectares") %>%
          dplyr::mutate(scenario = "scenario_mixed_use_mf_new")
      } else{
        land_by_development_type$scenario_mixed_use_mf_new %>%
          dplyr::select(
            .,
            ctu_name,
            development_name,
            scenario_total,
            scenario_mixed_use_compact_zoning
          ) %>%
          dplyr::mutate(
            scenario_mixed_use_mf_new =
              (scenario_total - scenario_mixed_use_compact_zoning)
            * luse_scenario_params()$urban_infill
          ) %>%
          dplyr::select(ctu_name, development_name, scenario_mixed_use_mf_new) %>%
          dplyr::rename(hectares = scenario_mixed_use_mf_new) %>%
          dplyr::mutate(scenario = "scenario_mixed_use_mf_new")
      }) %>%
      dplyr::mutate(
        hectares =
          dplyr::case_when(
            development_name == "exurban_development" ~ 0,
            development_name != "exurban_development" ~ hectares
          )
      )

    # Scenario Other Zoning
    land_by_development_type$scenarion_other_zoning <-
      bind_rows(
        land_by_development_type$scenario_total,
        land_by_development_type$scenario_mixed_use_mf_new,
        land_by_development_type$scenario_mixed_use_compact_zoning
      ) %>%
      dplyr::group_by(ctu_name, development_name) %>%
      tidyr::pivot_wider(.,
                         names_from = scenario,
                         values_from = hectares) %>%
      dplyr::group_by(ctu_name, development_name) %>%
      dplyr::transmute(hectares =
                         scenario_total -
                         scenario_mixed_use_mf_new -
                         scenario_mixed_use_compact_zoning) %>%
      dplyr::mutate(scenario = "scenario_other_zoning")

    return(land_by_development_type)

  }
