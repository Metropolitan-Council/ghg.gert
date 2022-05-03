## ----setup, include=FALSE------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------------------------------------------
# sums the land use by development type
p_land_by_development_type_sum_bau <-
  t_ctu_land_use_hectares %>%
  dplyr::filter(year == 2040) %>%
  dplyr::group_by(ctu_name) %>%
  dplyr::summarise(total_hectares_bau = sum(hectares)) %>%
  dplyr::ungroup()


## ------------------------------------------------------------------------------------------------------------
# temporary table
# filters to specific land use related residential compact zoning
p_x <-
  t_ctu_land_use_hectares %>%
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
    t_ctu_land_use_hectares %>%
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
            hectares * v_scenario$urban_expansion_relative_to_bau,
            hectares
          )
      ) %>%
      dplyr::mutate(scenario = "scenario_mixed_use_compact_zoning")
  ) 


## ------------------------------------------------------------------------------------------------------------
# step to in creating the land by development type table
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
          urban_expansion * v_urban_expansion_relative_to_bau < total_hectares_bau - urban_infill,
          urban_expansion * v_urban_expansion_relative_to_bau,
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


## ------------------------------------------------------------------------------------------------------------
# temporary variable
p_x2 <-
  p_land_by_development_type_s2 %>%
  tidyr::pivot_wider(., names_from = scenario, values_from = hectares) %>%
  dplyr::mutate(
    scenario_mixed_use_mf_new = case_when(
      development_name == "urban_expansion" ~ (scenario_total - scenario_mixed_use_compact_zoning) *
        v_urban_expansion_relative_to_bau,
      development_name != "urban_expansion" ~ bau
    )
  )

# step 3 in creating the land by development type table
p_land_by_development_type_s3 <-
  dplyr::bind_rows(p_land_by_development_type_s2,
            (if (params$scenario == "compact_dev_with_drs") {
              p_x2 %>%
                dplyr::select(.,
                       ctu_name,
                       development_name,
                       scenario_mixed_use_mf_new) %>%
                tidyr::pivot_wider(.,
                            names_from = development_name,
                            values_from = scenario_mixed_use_mf_new) %>%
                dplyr::mutate(urban_infill =
                                dplyr::if_else(
                                  urban_expansion > urban_infill,
                                  urban_infill,
                                  urban_expansion
                                )) %>%
                tidyr::pivot_longer(.,
                             2:4,
                             names_to = "development_name",
                             values_to = "hectares") %>%
                dplyr::mutate(scenario = "scenario_mixed_use_mf_new")
            } else{
              p_x2 %>%
                dplyr::select(.,
                       development_name,
                       scenario_total,
                       scenario_mixed_use_compact_zoning) %>%
                dplyr::mutate(
                  scenario_mixed_use_mf_new =
                    (scenario_total - scenario_mixed_use_compact_zoning)
                  * var_urban_infill
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
              ))


## ------------------------------------------------------------------------------------------------------------
# temporary function 
# creates function  to filter and create different 
p_x3 <- function(z) {
  p_land_by_development_type_s3 %>%
    dplyr::filter(development_name == z) %>%
    tidyr::pivot_wider(.,
                names_from = scenario,
                values_from = hectares) %>%
    dplyr::group_by(ctu_name, development_name) %>%
    dplyr::transmute(hectares =
                scenario_total -
                scenario_mixed_use_mf_new -
                scenario_mixed_use_compact_zoning) %>%
    dplyr::mutate(scenario = "scenario_other_zoning") %>%
    dplyr::mutate(development_name = z)
}

# create the land by development type table
p_land_by_development_type <-
  p_land_by_development_type_s3 %>%
  dplyr::bind_rows(.,
                   dplyr::bind_rows(
                     p_x3("urban_expansion"),
                     p_x3("urban_infill"),
                     p_x3("exurban_development")
                   ))

