#' @title Calculate Non-Residential Building Emissions
#' @family building_energy_module
#'
#' @description `calc_ghg_non_residential()` calculate total energy demand and emissions from
#' workers for industrial and commercial sectors
#'
#' @param non_res_tb table with non-residential data.
#'      Default is `building_data$non_residential`
#'
#' @inheritParams run_scenario
#' @return
#' @export
#'
#' @family buildings
#' @example
#'
#'
#' \donotrun{
#' calc_ghg_non_residential(
#' non_res_tb = building_data$non_residential
#'      .smart_grid = FALSE,
#'      .industrial_smart_grid_pct = 1,
#'      .grid_decarbonization_pct = 1,
#'      .renewable_natural_gas = FALSE,
#'      .enviro_factors = enviro_factors
#' )
#' }
calc_ghg_non_residential_x <-
  function(non_res_tb = building_data$non_residential,
           .smart_grid = FALSE,
           .commercial_smart_grid_pct = 1,
           .industrial_smart_grid_pct = 1,
           .electrify_commercial = FALSE,
           .grid_decarbonization_pct = 1,
           .renewable_natural_gas = FALSE,
           .enviro_factors = enviro_factors) {
    emis_bau <- non_res_tb %>%
      dplyr::filter(
        var %in% c(
          "population",
          "commercial_jobs",
          "industrial_jobs",
          "commercial_therm_per_worker",
          "industrial_therm_per_worker",
          "commercial_mwh_per_worker",
          "industrial_mwh_per_worker"
        )
      ) %>%
      dplyr::group_by(ctu_name, year, var) %>%
      tidyr::pivot_wider(names_from = "var", values_from = value) %>%
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
      dplyr::mutate(
        # mw hours
        commercial_mwh = if_else(.electrify == TRUE,

                          # electrification effect
                          (commercial_jobs * commercial_mwh_per_worker) * 0.4 *
                            (1 - commercial_jobs * commercial_therm_per_worker),


                          (commercial_jobs * commercial_mwh_per_worker)),



        industrial_mwh = (industrial_jobs * industrial_mwh_per_worker),

        # therms
        commercial_therms = (commercial_jobs * commercial_therm_per_worker),
        industrial_therms = (industrial_jobs * industrial_therm_per_worker),

        smart_grid_factor = (1 - .enviro_factors$SMART_GRID_EFFICIENCY_PCT),

        # electric emissions
        commercial_electricity_emissions_kg_co =

          dplyr::if_else(

            .smart_grid == TRUE,

            .commercial_smart_grid_pct * commercial_mwh *
              (kg_per_mwh * .grid_decarbonization_pct),

            commercial_mwh *
              (kg_per_mwh * .grid_decarbonization_pct)),

        industrial_electricity_emissions_kg_co =

          dplyr::if_else(

            .smart_grid == TRUE,

            .industrial_smart_grid_pct * industrial_mwh *
              (kg_per_mwh * .grid_decarbonization_pct),

            industrial_mwh *
              (kg_per_mwh * .grid_decarbonization_pct)

          ),
        # therm emissions

        commercial_natural_gas_emissions_kg_co =

          dplyr::if_else(

            .renewable_natural_gas == TRUE,
            (commercial_therms - (population * 78)) * kg_per_therm,
            commercial_therms * kg_per_therm
          ),
        industrial_natural_gas_emissions_kg_co =
          dplyr::if_else(
            .renewable_natural_gas == TRUE,
            (industrial_therms - (population * 78)) * kg_per_therm,
            industrial_therms * kg_per_therm
          ),
        total_industrial_commercial_emissions = sum(
          c(
            commercial_electricity_emissions_kg_co,
            industrial_electricity_emissions_kg_co,
            commercial_natural_gas_emissions_kg_co,
            industrial_natural_gas_emissions_kg_co
          ),
          na.rm = T
        )
      ) %>%
      dplyr::select(
        ctu_name,
        year,
        commercial_mwh,
        industrial_mwh,
        commercial_therms,
        industrial_therms,
        commercial_electricity_emissions_kg_co,
        industrial_electricity_emissions_kg_co,
        commercial_natural_gas_emissions_kg_co,
        industrial_natural_gas_emissions_kg_co,
        total_industrial_commercial_emissions
      )
    return(emis)
  }
