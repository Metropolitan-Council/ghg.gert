#' @title Calculate residential building emissions
#' @family buildings
#' @family residential
#' @family emissions
#'
#' @description Estimates total energy demand
#'      and emissions from the residential building sector by city/township
#'      for the user-specified scenario, and the business-as-usual scenario.
#'
#' @note `calc_ghg_residential()` estimates the building energy demand and emissions
#'      based on the floor area assumptions. For a function that compiles all
#'      residential strategies refer to [`scen_residential_building()`].
#'
#' @param res_tb [tibble::tibble()].
#'      Table, table with residential building data.
#' @inheritParams run_scenario_transportation
#' @inheritParams scen_building_residential
#'
#' @return [tibble::tibble()].
#'    A table with columns
#'    `ctu_name`,
#'    `year`,
#'    `population`,
#'    `residential_mwh`,
#'    `residential_electricity_emissions_kg_co`,
#'    `residential_therms`, and
#'    `residential_natural_gas_emissions_kg_co`
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' calc_ghg_residential(
#'   res_tb = building_data$residential,
#'   res_tb_bau = building_data$residential,
#'   .selected_ctu = "all",
#'   .grid_decarbonization_pct = 1,
#'   .enviro_factors = enviro_factors
#' )
#' }
#' @export
#'
calc_ghg_residential <- function(res_tb,
                                 res_tb_bau,
                                 .selected_ctu,
                                 .grid_decarbonization_pct,
                                 .enviro_factors = ghg.sp::enviro_factors) {
  # cli::cli_progress_message("*** calculating residential ghg emissions \n")

  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)
  res_tb_bau <- filter_ctu(res_tb_bau, .selected_ctu = .selected_ctu)

  emis <- function(tb,
                   grid_decarb,
                   .enviro_factors = ghg.sp::enviro_factors) {
    emis_tb <- tb %>%
      dplyr::filter(
        var %in% c(
          "population",
          "single_family_units",
          "multifamily_units",
          "population",
          "residential_kwh_per_floor_area",
          "residential_therms_per_floor_area",
          "single_family_average_floor_area_sqft_ctu",
          "multifamily_average_floor_area_sqft_county"
        )
      ) %>%
      dplyr::group_by(ctu_name, year, var) %>%
      tidyr::pivot_wider(names_from = "var", values_from = value, values_fn = sum) %>%
      dplyr::mutate(
        kg_per_mwh = dplyr::case_when(
          year < 2040 ~ .enviro_factors$KG_CO2E_PER_MHW_BASELINE,
          TRUE ~ .enviro_factors$KG_CO2E_PER_MHW_FORECAST
        ),
        kg_per_therm = dplyr::case_when(
          year < 2040 ~ .enviro_factors$KG_CO2E_PER_THERM_BASELINE,
          TRUE ~ .enviro_factors$KG_CO2E_PER_THERM_FORECAST
        )
      ) %>%
      dplyr::mutate(residential_floor_area_per_capita = (
        (
          single_family_average_floor_area_sqft_ctu * single_family_units
        ) +
          (
            multifamily_average_floor_area_sqft_county * multifamily_units
          )
      )
      / population) %>%
      dplyr::mutate(
        residential_mwh = population * residential_floor_area_per_capita * (residential_kwh_per_floor_area / 1000),
        residential_electricity_emissions_kg_co = residential_mwh * (kg_per_mwh * (1 -
          dplyr::if_else(year < 2040, .enviro_factors$GRID_DECARBONIZATION_BASELINE,
            grid_decarb
          )
        )
        )
      ) %>%
      dplyr::mutate(
        residential_therms = population * residential_floor_area_per_capita * residential_therms_per_floor_area,
        residential_natural_gas_emissions_kg_co =
          residential_therms * kg_per_therm
      ) %>%
      dplyr::mutate(total_residential_emissions = sum(
        c(
          residential_natural_gas_emissions_kg_co,
          residential_electricity_emissions_kg_co
        ),
        na.rm = T
      )) %>%
      unique() %>%
      dplyr::select(
        ctu_name,
        year,
        population,
        residential_mwh,
        residential_electricity_emissions_kg_co,
        residential_therms,
        residential_natural_gas_emissions_kg_co,
        total_residential_emissions
      )

    return(emis_tb)
  }

  emis_bau <-
    emis(
      tb = res_tb_bau,
      grid_decarb = 0.6,
      .enviro_factors = .enviro_factors
    )

  emis_strategy <-
    emis(
      tb = res_tb,
      grid_decarb = .grid_decarbonization_pct,
      .enviro_factors = .enviro_factors
    )

  emis_final <-
    dplyr::right_join(
      emis_bau,
      emis_strategy,
      by = c("ctu_name", "year"),
      suffix = c(".bau", ".scen")
    ) %>%
    tidyr::pivot_longer(
      names_to = "var",
      values_to = "value",
      cols = -c(ctu_name, year)
    ) %>%
    tidyr::separate(
      col = var,
      into = c("var", "scen"),
      sep = "\\."
    ) %>%
    dplyr::ungroup()

  return(emis_final)
}
