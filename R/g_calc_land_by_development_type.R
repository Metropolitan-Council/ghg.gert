#' @title Calculate Land by Development Type
#' @family Land Use
#'
#' @description ``calc_land_by_development_type()` calculates the hectares of
#'      land by different development types
#'      (urban expansion, urban infill, and exurban development) for different land
#'      use scenarios at the city/township scale
#'
#' @param tb Tibble.
#'      The input dataset to be used.
#'      Default is `land_use_data`
#' @param .scenario **Character**.
#'      Default is `"bau"`
#' @param .luse_scen **Character**.
#'      The current land use scenario being explored
#'      Default is `"compact_dev_with_drs"`
#'
#' @return **Tibble**.
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
#' ghg.sp::calc_land_by_development_type(
#'     tb = land_use_data,
#'     .scenario = "bau",
#'     .luse_scen = "compact_dev_with_drs")
#' }
calc_land_by_development_type <-
  function(tb,
           .scenario,
           .luse_scen) {

    luse_scenario_params <- tb$scenario_parameters %>%
      dplyr::filter(scenario_description_2 == .scenario)

    land_by_development_type <- tb$land_by_development_type

    # Scenario Mixed Use Compact Zoning Park
    land_by_development_type$scenario_mixed_use_compact_zoning_park <-
      tb$multifamily_mixed_area %>%
      dplyr::mutate(
        hectares =
          dplyr::if_else(
            development_name == "urban_expansion",
            hectares * luse_scenario_params$urban_expansion_relative_to_bau,
            hectares
          )
      ) %>%
      dplyr::mutate(scenario = "scenario_mixed_use_compact_zoning")

    # Scenario Total
    land_by_development_type$scenario_total <-
      land_by_development_type$bau_total %>%
      base::merge(.,
                  tb$land_by_development_type_sum_bau,
                  by = "ctu_name") %>%
      tidyr::pivot_wider(.,
                         names_from = development_name,
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
                      total_hectares_bau - (urban_infill) - (urban_expansion))  %>%
      dplyr::mutate(scenario = "scenario_total") %>%
      select(., -c(total_hectares_bau)) %>%
      tidyr::pivot_longer(.,
                          cols =  3:5,
                          names_to = "development_name",
                          values_to = "hectares")


    # Scenario Mixed Use MF (new)
    scenario_mixed_use_mf_new_1 <-
      bind_rows(
        land_by_development_type$bau_total,
        land_by_development_type$scenario_total,
        land_by_development_type$scenario_mixed_use_compact_zoning_park
      ) %>%
      tidyr::pivot_wider(
        .,
        names_from = c(scenario, development_name),
        values_from = hectares,
        names_sep = "."
      ) %>%
      dplyr::mutate(
        scenario_mixed_use_mf_new.urban_expansion = (
          scenario_total.urban_expansion - scenario_mixed_use_compact_zoning.urban_expansion
        ) *
          luse_scenario_params$urban_expansion_relative_to_bau
      )

    land_by_development_type$scenario_mixed_use_mf_new <-
      (if (.luse_scen == "compact_dev_with_drs") {
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
            names_to = c("scenario", "development_name"),
            names_sep = "[.]",
            values_to = "hectares"
          ) %>%
          filter(scenario == "scenario_mixed_use_mf_new")

      } else{
        scenario_mixed_use_mf_new_1 %>%
          dplyr::mutate(
            scenario_mixed_use_mf_new.urban_infill =
              ((
                scenario_total.urban_infill - scenario_mixed_use_compact_zoning.urban_infill
              )
              * luse_scenario_params$urban_infill
              ),
            scenario_mixed_use_mf_new.exurban_development = 0
          ) %>%
          tidyr::pivot_longer(
            .,
            cols = !ctu_name,
            names_to = c("scenario", "development_name"),
            names_sep = "[.]",
            values_to = "hectares"
          ) %>%
          filter(scenario == "scenario_mixed_use_mf_new")
      })


    # Scenario Other Zoning
    land_by_development_type$scenario_other_zoning <-
      bind_rows(
        land_by_development_type$scenario_total,
        land_by_development_type$scenario_mixed_use_mf_new,
        land_by_development_type$scenario_mixed_use_compact_zoning
      ) %>%
      tidyr::pivot_wider(
        .,
        names_from = c(scenario, development_name),
        values_from = hectares,
        names_sep = "."
      ) %>%
      dplyr::mutate(
        scenario_other_zoning.urban_expansion =
          scenario_total.urban_expansion -
          scenario_mixed_use_mf_new.urban_expansion -
          scenario_mixed_use_compact_zoning.urban_expansion,
        scenario_other_zoning.urban_infill =
          scenario_total.urban_infill -
          scenario_mixed_use_mf_new.urban_infill -
          scenario_mixed_use_compact_zoning.urban_infill,
        scenario_other_zoning.exurban_development =
          scenario_total.exurban_development -
          scenario_mixed_use_mf_new.exurban_development
      ) %>%
      tidyr::pivot_longer(
        .,
        cols = !ctu_name,
        names_to = c("scenario", "development_name"),
        names_sep = "[.]",
        values_to = "hectares"
      ) %>%
      filter(scenario == "scenario_other_zoning")

    return(land_by_development_type)
  }
