## ------------------------------------------------------------------------------------------------------------
p_scenario_land_use_2040_scaling_factor <-
  p_land_by_development_type %>%
  # Increase mixed use / residential
  dplyr::filter(scenario %in% c("scenario_mixed_use_mf_new",
                                "scenario_other_zoning")) %>%
  tidyr::pivot_wider(
    data = .,
    id_cols = c(ctu_name, development_name),
    names_from = c(scenario),
    values_from = c(hectares)
  ) %>%
  full_join(
    (
      p_land_by_development_type %>%
        dplyr::filter(scenario == "scenario_total") %>%
        dplyr::group_by(ctu_name, development_name) %>%
        dplyr::summarise(total_hectares = sum(hectares), .groups = 'drop')
    ),
    by = c("ctu_name", "development_name")
  ) %>%
  dplyr::group_by(ctu_name, development_name) %>%
  dplyr::mutate(
    scaling_factor =
      dplyr::if_else(
        scenario_mixed_use_mf_new > 0,
        scenario_other_zoning / total_hectares,
        1
      )
  ) %>%
  ungroup()


## ------------------------------------------------------------------------------------------------------------
p_scenario_land_use_2040 <-
  p_scenario_land_use_2040_scaling_factor %>%
  base::merge(
    .,
    (p_land_composition_ctu %>%
       filter(year == 2016)),
    by.x = c("ctu_name",
             "development_name"),
    by.y = c("ctu_name",
             "development_name"),
    all.x = TRUE,
    all.y = TRUE
  ) %>%
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
                    percent * total_hectares.x
                  ) - hectares)) + (scenario_mixed_use_mf_new / 4)
                  ),

                  ((
                    hectares + ((percent * total_hectares.x) - hectares)
                  ) * scaling_factor))) %>%
  dplyr::select(c(
    "ctu_name",
    "development_name",
    "description_2",
    "scenario_hectares"
  ))


## ------------------------------------------------------------------------------------------------------------
p_summed_land_use_2040 <-
  p_scenario_land_use_2040 %>%
  dplyr::group_by(ctu_name, description_2) %>%
  dplyr::summarise(scenario_hectares = sum(scenario_hectares),
                   .groups = 'drop') %>%
  dplyr::group_by(ctu_name) %>%
  dplyr::mutate(total_scenario_hectares = sum(scenario_hectares)) %>%
  dplyr::ungroup()
