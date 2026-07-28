#' @title Calculate residential building energy
#' @family buildings
#' @family residential
#' @family emissions
#'
#' @description Estimates total energy demand (electricity, natural gas,
#'   propane, and kerosene) from the residential building sector by
#'   city/township for the user-specified scenario and BAU.
#'
#'   Baseline years use observed inventory data directly. Forecast years
#'   apply per-unit energy profiles from [calc_building_energy()] calibrated
#'   to the most recent 5 observed years. Propane and kerosene track the
#'   natural gas forecast proportionally using per-CTU ratios derived from
#'   the baseline period.
#'
#' @param res_tb [tibble::tibble()].
#'      Scenario-adjusted residential units from [calc_residential_electrification()].
#' @param res_tb_bau [tibble::tibble()].
#'      BAU residential units (zero-strategy pass).
#'
#' @inheritParams run_module_transportation
#' @inheritParams scen_building_residential
#'
#' @return [tibble::tibble()] with columns
#'   `geog_name`, `geog_id`, `emissions_year`, `scenario`,
#'   `residential_mwh`, `residential_mcf`,
#'   `residential_propane_mmbtu`, `residential_kerosene_mmbtu`.
#'
#' @export
calc_energy_residential <- function(res_tb,
                                    res_tb_bau,
                                    .baseline_year,
                                    .scenario = "alt",
                                    .selected_ctu) {
  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)
  res_tb_bau <- filter_ctu(res_tb_bau, .selected_ctu = .selected_ctu)

  # baseline observed energy ----

  baseline_elec <- filter_ctu(
    ghg.ccap::building_energy_data$electricity_inventory,
    .selected_ctu = .selected_ctu
  ) %>%
    dplyr::filter(emissions_year <= .baseline_year, sector == "Residential")

  baseline_natgas <- filter_ctu(
    ghg.ccap::building_energy_data$natgas_inventory,
    .selected_ctu = .selected_ctu
  ) %>%
    dplyr::filter(emissions_year <= .baseline_year, sector == "Residential")

  # propane_inventory columns: propane_mmbtu, fueloil_other_mmbtu, propane (hh count)
  # select and rename to the canonical names used downstream
  baseline_propane <- filter_ctu(
    ghg.ccap::building_energy_data$propane_inventory,
    .selected_ctu = .selected_ctu
  ) %>%
    dplyr::filter(emissions_year <= .baseline_year) %>%
    dplyr::select(
      geog_name, geog_id, geog_level, sector, emissions_year,
      propane_mmbtu,
      kerosene_mmbtu = fueloil_other_mmbtu
    )

  baseline_energy <- dplyr::left_join(
    baseline_elec,
    baseline_natgas,
    by = dplyr::join_by(geog_name, geog_id, geog_level, sector, emissions_year)
  ) %>%
    dplyr::left_join(
      baseline_propane,
      by = dplyr::join_by(geog_name, geog_id, geog_level, sector, emissions_year)
    ) %>%
    dplyr::mutate(
      propane_mmbtu  = tidyr::replace_na(propane_mmbtu, 0),
      kerosene_mmbtu = tidyr::replace_na(kerosene_mmbtu, 0)
    )

  # energy profiles + calibration ----

  ctu_energy_profile <- calc_building_energy(.selected_ctu = .selected_ctu)

  bau_baseline_units <- res_tb_bau %>%
    dplyr::filter(
      emissions_year >= (.baseline_year - 4),
      emissions_year <= .baseline_year
    ) %>%
    dplyr::distinct(geog_name, sp_categories, emissions_year, allocated_units) %>%
    dplyr::left_join(
      dplyr::filter(ctu_energy_profile, scenario == "baseline"),
      by = c("sp_categories" = "mc_classification")
    )

  observed_recent <- dplyr::filter(baseline_energy, emissions_year >= (.baseline_year - 4))

  mwh_adjustment <- sum(observed_recent$mwh) /
    sum(bau_baseline_units$allocated_units * bau_baseline_units$scenario_mwh)

  mcf_adjustment <- sum(observed_recent$mcf) /
    sum(bau_baseline_units$allocated_units * bau_baseline_units$scenario_mcf)

  # propane/kerosene ratios ----
  # per-CTU ratio of liquid fuel to natgas, from the recent baseline window.
  # forecast propane/kerosene will track natgas proportionally so the same

  # strategy reductions apply evenly across fuel types.

  fuel_ratios <- observed_recent %>%
    dplyr::group_by(geog_name) %>%
    dplyr::summarize(
      propane_per_mcf  = sum(propane_mmbtu)  / sum(mcf),
      kerosene_per_mcf = sum(kerosene_mmbtu) / sum(mcf),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      propane_per_mcf  = dplyr::if_else(is.finite(propane_per_mcf),  propane_per_mcf,  0),
      kerosene_per_mcf = dplyr::if_else(is.finite(kerosene_per_mcf), kerosene_per_mcf, 0)
    )

  # forecast energy ----

  compute_energy <- function(tb) {
    browser()
    tb %>%
      dplyr::filter(emissions_year > .baseline_year) %>%
      dplyr::left_join(
        ctu_energy_profile,
        by = c("sp_categories" = "mc_classification", "scenario")
      ) %>%
      dplyr::mutate(
        residential_mwh = allocated_units * scenario_mwh * mwh_adjustment,
        residential_mcf = allocated_units * scenario_mcf * mcf_adjustment
      ) %>%
      dplyr::group_by(geog_name, geog_id, emissions_year) %>%
      dplyr::summarize(
        residential_mwh = sum(residential_mwh, na.rm = TRUE),
        residential_mcf = sum(residential_mcf, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::left_join(fuel_ratios, by = "geog_name") %>%
      dplyr::mutate(
        residential_propane_mmbtu  = residential_mcf * propane_per_mcf,
        residential_kerosene_mmbtu = residential_mcf * kerosene_per_mcf
      ) %>%
      dplyr::select(-propane_per_mcf, -kerosene_per_mcf)
  }

  # combine baseline + forecast ----

  baseline_rows <- baseline_energy %>%
    dplyr::select(
      geog_name, geog_id, emissions_year,
      residential_mwh          = mwh,
      residential_mcf          = mcf,
      residential_propane_mmbtu  = propane_mmbtu,
      residential_kerosene_mmbtu = kerosene_mmbtu
    )

  energy_final <- dplyr::bind_rows(
    dplyr::bind_rows(baseline_rows, compute_energy(res_tb_bau)) %>%
      dplyr::mutate(scenario = "bau"),
    dplyr::bind_rows(baseline_rows, compute_energy(res_tb)) %>%
      dplyr::mutate(scenario = .scenario)
  )

  return(energy_final)
}
