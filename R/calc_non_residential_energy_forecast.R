#' @title Calculate Non Residential Energy Forecast
#'
#' @return
#' @export
#'
#' @examples
calc_non_residential_energy_forecast <- function(tb = building_energy_data){

  ctu_characteristics_forecast <- calc_demographic_forecast()$ctu
  ctu_nonresidential_energy_baseline <- get_non_residential_energy_baseline()


# -------------------------------------------------------------------------

  # NREL data: used in instances where there is not enough data to calculate using employment energy intensity
  nrel_nonresidential_energy_forecast <-
    tb$nrel_energy_consumption_ctu %>%
    dplyr::mutate(unit =
                    dplyr::case_when(source == "elec" ~ "mwh_nrel",
                                     source == "ng" ~ "therms_nrel")) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        source == "elec" ~ consumption_mmbtu * 0.293071,
        source == "ng" ~ consumption_mmbtu * 10
      )
    ) %>%
    tidyr::unite("var", c(sector, unit), remove = FALSE) %>%
    dplyr::filter(year %in% c(2040)) %>%
    dplyr::select(ctu_name, year, var, value)

  # non residential forecast
  ## -------------------------------------------------------------------------------------------
  ctu_nonresidential_energy_forecast <-
    dplyr::bind_rows(
      ctu_characteristics_forecast,
      ctu_nonresidential_energy_baseline,
      nrel_nonresidential_energy_forecast
    ) %>%
    dplyr::select(-c("year")) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = "var", values_from = "value") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      commercial_mwh = dplyr::if_else(is.na(commercial_mwh_per_worker * commercial_jobs) == FALSE, commercial_mwh_per_worker * commercial_jobs, commercial_mwh_nrel),
      commercial_therms = dplyr::if_else(is.na(commercial_therm_per_worker * commercial_jobs) == FALSE, commercial_therm_per_worker * commercial_jobs, commercial_therms_nrel),
      industrial_mwh = dplyr::if_else(is.na(industrial_mwh_per_worker * industrial_jobs) == FALSE, industrial_mwh_per_worker * industrial_jobs, industrial_mwh_nrel),
      industrial_therms = dplyr::if_else(is.na(industrial_therm_per_worker * industrial_jobs) == FALSE, industrial_therm_per_worker * industrial_jobs, industrial_therms_nrel),
      year = 2040
    ) %>%
    dplyr::select(
      ctu_name,
      year,
      commercial_mwh,
      commercial_therms,
      industrial_mwh,
      industrial_therms,
      industrial_therm_per_worker,
      industrial_mwh_per_worker,
      commercial_therm_per_worker,
      commercial_mwh_per_worker
    ) %>%
    tidyr::pivot_longer(
      cols = c(
        "commercial_mwh",
        "commercial_therms",
        "industrial_mwh",
        "industrial_therms",
        "industrial_therm_per_worker",
        "industrial_mwh_per_worker",
        "commercial_therm_per_worker",
        "commercial_mwh_per_worker"
      ),
      names_to = "var"
    )

  return(ctu_nonresidential_energy_forecast)


}
