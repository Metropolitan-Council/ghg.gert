#' Title
#'
#' @param tb
#' @param .enviro_factors
#'
#' @return
#' @export
#'
#' @examples
calc_residential_renewable_ng <-
  function(res_tb,
           .enviro_factors = .enviro_factors) {
    new_res_tb <-
      res_tb %>%
      dplyr::mutate(reduced_therms =
                      (residential_therms.bau.2040
                       - residential_therms.scen.2040)) %>%
      dplyr::mutate(
        residential_natural_gas_emissions_kg_co.scen.2040 =
          (residential_therms.bau.2040 -
             (
               reduced_therms - (78 * population.bau.2040)
             )) *
          .enviro_factors$KG_CO2E_PER_THERM_FORECAST
      )

    return(new_res_tb)
  }
