#' Title
#'
#' @param .luse_scen
#'
#' @return
#' @export
#'
#' @examples

calc_land_by_development_type_1 <-
  function(tb = land_use_data,
           .luse_scen = "compact_dev_with_drs") {
    p_x <-
      tb$ctu_land_use_hectares %>%
      dplyr::group_by(ctu_name, development_name) %>%
      dplyr::filter(year == 2040) %>%
      dplyr::filter(
        description_2 %in% c(
          "park_recreational_or_preserve",
          "mixed_use_commercial",
          "mixed_use_industrial",
          "mixed_use_residential",
          "multifamily"
        )
      ) %>%
      dplyr::summarise(hectares = sum(hectares), .groups = 'drop') %>%
      dplyr::ungroup()

    # step 1 in creating the land by development type table
    p_land_by_development_type_s1 <-
      bind_rows(
        # BAU Total
        tb$ctu_land_use_hectares %>%
          dplyr::filter(year == 2040) %>%
          dplyr::group_by(ctu_name, development_name) %>%
          dplyr::summarise(hectares = sum(hectares), .groups = 'drop') %>%
          dplyr::mutate(scenario = "bau"),

        # BAU Mixed Use/Compact Zoning/Park
        p_x %>%
          dplyr::mutate(scenario = "bau_mixed_use_compact_zoning"),

        # Scenario Mixed Use Compact Zoning Park
        p_x %>%
          dplyr::mutate(
            hectares =
              dplyr::if_else(
                development_name == "urban_expansion",
                hectares * luse_scenario_params()$urban_expansion_relative_to_bau,
                hectares
              )
          ) %>%
          dplyr::mutate(scenario = "scenario_mixed_use_compact_zoning")
      )

    p_land_by_development_type_s2 <-
      dplyr::bind_rows(
        p_land_by_development_type_s1,
        # scenario total
        p_land_by_development_type_s1 %>%
          dplyr::filter(scenario == "bau") %>%
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
          tidyr::pivot_longer(
            .,
            cols =  3:5,
            names_to = "development_name",
            values_to = "hectares"
          )
      )

    p_x2 <-
      p_land_by_development_type_s2 %>%
      tidyr::pivot_wider(., names_from = scenario, values_from = hectares) %>%
      dplyr::mutate(
        scenario_mixed_use_mf_new = case_when(
          development_name == "urban_expansion" ~ (scenario_total - scenario_mixed_use_compact_zoning) *
            luse_scenario_params()$urban_expansion_relative_to_bau,
          development_name != "urban_expansion" ~ bau
        )
      )


    p_land_by_development_type_s3 <-
      dplyr::bind_rows(
        p_land_by_development_type_s2,
        (if (.luse_scen == "compact_dev_with_drs") {
          p_x2 %>%
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
          p_x2 %>%
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
      )

    return(p_land_by_development_type_s3)

  }
#' Title
#'
#' @param .development_name
#'
#' @return
#' @export
#'
#' @examples
p_x3 <- function(.development_name = "urban_expansion") {
  calc_land_by_development_type_1() %>%
    dplyr::filter(development_name == .development_name) %>%
    tidyr::pivot_wider(.,
                       names_from = scenario,
                       values_from = hectares) %>%
    dplyr::group_by(ctu_name, development_name) %>%
    dplyr::transmute(hectares =
                       scenario_total -
                       scenario_mixed_use_mf_new -
                       scenario_mixed_use_compact_zoning) %>%
    dplyr::mutate(scenario = "scenario_other_zoning") %>%
    dplyr::mutate(development_name = .development_name)
}

# create the land by development type table
#' Title
#'
#' @return
#' @export
#'
#' @examples
calc_land_by_development_type <- function() {
  calc_land_by_development_type_1() %>%
    dplyr::bind_rows(.,
                     dplyr::bind_rows(
                       p_x3("urban_expansion"),
                       p_x3("urban_infill"),
                       p_x3("exurban_development")
                     ))
}
