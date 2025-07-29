#' @title Adjust residential building energy needs
#' @family residential
#' @family buildings
#'
#' @description This function adjusts city level single-family energy demand per building
#' according to parcel data building characteristics: median year of construction,
#' median square footage
#'
#' @inheritParams run_scenario_building
#' @inheritParams filter_ctu
#' @inheritParams calc_ghg_residential
#' @param .new_homes_to_multifamily_pct numeric,  a value between `0` and `1`.
#'      Percentage of new single-family homes to instead be built as multifamily homes.
#'      Default is `0.0`.
#'
#' @return [tibble::tibble()].
#'       A table with columns `geog_name`, `geog_id`, `year`, `var`, and `value`.
#'       Table contains adjusted `single_family_units` and `multifamily_units` record for column `var`
#'       relative to residential inputs table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.ccap)
#'
#' adj_unit_counts(
#'   res_tb = compile_bau_building_energy()$residential,
#'   .selected_ctu = "all",
#'   .new_homes_to_multifamily_pct = 0.50
#' )
#' }
#' @importFrom dplyr filter group_by mutate select ungroup anti_join bind_rows
#' @importFrom tidyr pivot_wider
#' @importFrom cli cli_warn
calc_building_energy <- function(
    .selected_ctu,
    parcel_data = parcel_ctu
) {
  # bin based on resstock categories
  bin_sqft <- function(sqft) {
    cut(sqft,
        breaks = c(0, 999, 1499, 1999, 2499, 2999, Inf),
        labels = c("Less than 1,000", "1,000 to 1,499", "1,500 to 1,999",
                   "2,000 to 2,499", "2,500 to 2,999", "3,000 or more"),
        right = TRUE)
  }

  bin_year <- function(year) {
    cut(year,
        breaks = c(0, 1939, 1959, 1979, 1999, 2009, 2025.1),
        labels = c("<1940", "1940-59", "1960-79", "1980-99",
                   "2000-09", "2010s"),
        right = TRUE)
  }

  # Filter and bin parcels
  ctu_binned <- parcel_data %>%
    filter(geog_name == .selected_ctu,
           mc_classification %in% c("single_family_detached", "single_family_attached")) %>%
    mutate(
      sqft_bin = bin_sqft(sq_ft_use),
      year_bin = as.character(bin_year(median_year))
    )

  ### calculate ctu single family baseline based on size and year
  ctu_sf_baseline <- left_join(ctu_binned,
                      bind_rows(resstock_summaries$sf_attached_sqft_baseline,
                                resstock_summaries$sf_detached_sqft_baseline),
                      by = c("mc_classification",
                             "sqft_bin")) %>%
    left_join(bind_rows(resstock_summaries$sf_attached_year_baseline,
                        resstock_summaries$sf_detached_year_baseline),
              by = c("mc_classification",
                     "year_bin" = "build_year")) %>%
    # take the mean mwh and mcf of the two characteristics
    mutate(scenario_mwh = (median_kwh.x + median_kwh.y) / 2 * 10e-4,
           scenario_mcf = (median_mcf.x + median_mcf.y) / 2) %>%
    select(mc_classification,
           scenario_mwh,
           scenario_mcf)

  ctu_baseline <- bind_rows(
    ctu_sf_baseline,
    resstock_summaries$mf_baseline %>%
      mutate(scenario_mwh = median_kwh / 1000,
             scenario_mcf = median_mcf) %>%
      select(mc_classification,
             scenario_mwh,
             scenario_mcf),
    resstock_summaries$manufactured_baseline %>%
      mutate(scenario_mwh = median_kwh / 1000,
             scenario_mcf = median_mcf) %>%
      select(mc_classification,
             scenario_mwh,
             scenario_mcf)
  ) %>%
    mutate(scenario = "baseline")

  ctu_sf_retrofit <- left_join(ctu_binned,
                               bind_rows(resstock_summaries$sf_attached_sqft_envelope,
                                         resstock_summaries$sf_detached_sqft_envelope),
                               by = c("mc_classification",
                                      "sqft_bin")) %>%
    left_join(bind_rows(resstock_summaries$sf_attached_year_envelope,
                        resstock_summaries$sf_detached_year_envelope),
              by = c("mc_classification",
                     "year_bin" = "build_year")) %>%
    # take the mean mwh and mcf of the two characteristics
    mutate(scenario_mwh = (median_kwh.x + median_kwh.y) / 2 * 10e-4,
           scenario_mcf = (median_mcf.x + median_mcf.y) / 2) %>%
    select(mc_classification,
           scenario_mwh,
           scenario_mcf)

  ctu_retrofit <- bind_rows(
    ctu_sf_retrofit,
    resstock_summaries$mf_envelope %>%
      mutate(scenario_mwh = median_kwh / 1000,
             scenario_mcf = median_mcf) %>%
      select(mc_classification,
             scenario_mwh,
             scenario_mcf),
    resstock_summaries$manufactured_envelope %>%
      mutate(scenario_mwh = median_kwh / 1000,
             scenario_mcf = median_mcf) %>%
      select(mc_classification,
             scenario_mwh,
             scenario_mcf)
  ) %>%
    mutate(scenario = "retrofit")

  ctu_sf_heatpump <- left_join(ctu_binned,
                               bind_rows(resstock_summaries$sf_attached_sqft_heatpump,
                                         resstock_summaries$sf_detached_sqft_heatpump),
                               by = c("mc_classification",
                                      "sqft_bin")) %>%
    left_join(bind_rows(resstock_summaries$sf_attached_year_heatpump,
                        resstock_summaries$sf_detached_year_heatpump),
              by = c("mc_classification",
                     "year_bin" = "build_year")) %>%
    # take the mean mwh and mcf of the two characteristics
    mutate(scenario_mwh = (median_kwh.x + median_kwh.y) / 2 * 10e-4,
           scenario_mcf = (median_mcf.x + median_mcf.y) / 2) %>%
    select(mc_classification,
           scenario_mwh,
           scenario_mcf)

  ctu_heatpump <- bind_rows(
    ctu_sf_heatpump,
    resstock_summaries$mf_heatpump %>%
      mutate(scenario_mwh = median_kwh / 1000,
             scenario_mcf = median_mcf) %>%
      select(mc_classification,
             scenario_mwh,
             scenario_mcf),
    resstock_summaries$manufactured_heatpump %>%
      mutate(scenario_mwh = median_kwh / 1000,
             scenario_mcf = median_mcf) %>%
      select(mc_classification,
             scenario_mwh,
             scenario_mcf)
  ) %>%
    mutate(scenario = "heatpump")

  ctu_sf_new <- left_join(ctu_binned,
                                 bind_rows(resstock_summaries$sf_attached_sqft_baseline,
                                           resstock_summaries$sf_detached_sqft_baseline),
                                 by = c("mc_classification",
                                        "sqft_bin")) %>%
    left_join(bind_rows(resstock_summaries$sf_attached_year_baseline,
                        resstock_summaries$sf_detached_year_baseline) %>%
                filter(build_year == "2010s"),
              by = c("mc_classification")) %>%
    # take the mean mwh and mcf of the two characteristics
    mutate(scenario_mwh = (median_kwh.x + median_kwh.y) / 2 * 10e-4,
           scenario_mcf = (median_mcf.x + median_mcf.y) / 2) %>%
    select(mc_classification,
           scenario_mwh,
           scenario_mcf)

  ctu_new <- bind_rows(
    ctu_sf_new,
    resstock_summaries$mf_baseline %>%
      mutate(scenario_mwh = median_kwh / 1000,
             scenario_mcf = median_mcf) %>%
      select(mc_classification,
             scenario_mwh,
             scenario_mcf),
    resstock_summaries$manufactured_envelope %>%
      mutate(scenario_mwh = median_kwh / 1000,
             scenario_mcf = median_mcf) %>%
      select(mc_classification,
             scenario_mwh,
             scenario_mcf)
  ) %>%
    mutate(scenario = "new_build")

  ctu_energy_profile <- bind_rows(
    ctu_baseline,
    ctu_new,
    ctu_retrofit,
    ctu_heatpump
  )

  return(ctu_energy_profile)

  }
