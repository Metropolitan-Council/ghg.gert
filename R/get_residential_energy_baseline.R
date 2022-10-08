#' Title
#'
#' @return
#' @export
#'
#' @examples
get_residential_energy_baseline <- function(){

  # RESIDENTIAL ENERGY BASELINE -----

  ## ----- get electricity by ctu from 'Emissions' ------
  electricity_residential_ctu <-
    tb$electricity_residential_ctu %>%
    mutate(
      residential_mwh = case_when(
        (actual_residential_mwh > 1) ~ actual_residential_mwh,
        (is.na(actual_residential_mwh)) ~ est_residential_mwh
      ),
      residential_elec_emis_t_co2e = case_when(
        (actual_residential_mwh > 1) ~ actual_residential_electricity_emis_t_co2e,
        (is.na(actual_residential_mwh)) ~ est_residential_electricity_emis_t_co2e
      )
    ) %>%
    select(ctu_name, year, residential_mwh, residential_elec_emis_t_co2e) %>%
    group_by(ctu_name, year) %>%
    pivot_longer(
      cols = c(
        "residential_mwh",
        "residential_elec_emis_t_co2e"
      ),
      names_to = "var"
    )

  ## ----- get nat gas by ctu from 'Emissions'------
  natural_gas_residential_ctu <-
    tb$natural_gas_residential_ctu %>%
    mutate(
      residential_ng_therms = case_when(
        (actual_residential_ng_therms > 1) ~ actual_residential_ng_therms,
        (is.na(actual_residential_ng_therms)) ~ est_residential_ng_therms
      ),
      residential_ng_emis_t_co2e = case_when(
        (actual_residential_ng_therms > 1) ~ actual_residential_ng_emis_t_co2e,
        (is.na(actual_residential_ng_therms)) ~ est_residential_ng_emis_t_co2e
      )
    ) %>%
    select(
      ctu_name,
      year,
      residential_ng_therms,
      residential_ng_emis_t_co2e
    ) %>%
    group_by(ctu_name, year) %>%
    pivot_longer(
      cols = c("residential_ng_therms", "residential_ng_emis_t_co2e"),
      names_to = "var"
    )

  ## ----- estimate residential kwh/sqft -----
  residential_kwh_per_sqft <-
    bind_rows(
      electricity_residential_ctu,
      ctu_characteristics %>%
        filter(var %in% c(
          "SFD_Units",
          "single_family_average_floor_area_sqft_ctu",
          "MF_Units",
          "multifamily_average_floor_area_sqft_county"
        ))
    ) %>%
    unique() %>%
    group_by(ctu_name, year) %>%
    pivot_wider(names_from = "var", values_from = "value", values_fn = mean) %>%
    mutate(kwh_per_floor_area = (residential_mwh / ((
      SFD_Units * single_family_average_floor_area_sqft_ctu) +
        (MF_Units * multifamily_average_floor_area_sqft_county))) *
        1000) %>%
    select(ctu_name, year, kwh_per_floor_area) %>%
    group_by(ctu_name, year) %>%
    pivot_longer(
      cols = c("kwh_per_floor_area"),
      names_to = "var"
    )

  ## ----- estimate residential therms/sqft ----
  residential_therms_per_sqft <-
    bind_rows(
      natural_gas_residential_ctu,
      ctu_characteristics
    ) %>%
    pivot_wider(names_from = "var", values_from = "value", values_fn = mean) %>%
    mutate(therms_per_floor_area = (residential_ng_therms / ((
      SFD_Units * single_family_average_floor_area_sqft_ctu
    ) +
      (
        MF_Units * multifamily_average_floor_area_sqft_county
      )
    ))) %>%
    select(ctu_name, year, therms_per_floor_area) %>%
    group_by(ctu_name, year) %>%
    pivot_longer(
      cols = c("therms_per_floor_area"),
      names_to = "var"
    )

  ## ---- estimate residential kwh/household -----
  residential_kwh_per_household <-
    bind_rows(
      electricity_residential_ctu,
      ctu_characteristics
    ) %>%
    pivot_wider(names_from = "var", values_from = "value", values_fn = mean) %>%
    mutate(residential_mwh_per_households = residential_mwh / households) %>%
    select(ctu_name, year, residential_mwh_per_households) %>%
    group_by(ctu_name, year) %>%
    pivot_longer(
      cols = c("residential_mwh_per_households"),
      names_to = "var"
    )

  ## ----- estimate residnetial  therms/household ----
  residential_therms_per_household <-
    bind_rows(
      natural_gas_residential_ctu,
      ctu_characteristics
    ) %>%
    pivot_wider(names_from = "var", values_from = "value", values_fn = mean) %>%
    mutate(residential_therms_per_households = residential_ng_therms / households) %>%
    group_by(ctu_name, year) %>%
    select(ctu_name, year, residential_therms_per_households) %>%
    pivot_longer(
      cols = c("residential_therms_per_households"),
      names_to = "var"
    )

  ## ---- return variables residential energy baseline -----
  ctu_residential_energy_baseline <-
    bind_rows(
      electricity_residential_ctu,
      natural_gas_residential_ctu,
      residential_kwh_per_sqft,
      residential_therms_per_sqft,
      residential_kwh_per_household,
      residential_therms_per_household
    )

  return(ctu_residential_energy_baseline)


}
