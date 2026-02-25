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
    parcel_data = ghg.ccap::parcel_ctu,
    resstock_tb = ghg.ccap::resstock_summaries,
    ceestock_tb = ghg.ccap::ceestock_summaries
    ) {
  # bin based on resstock categories
  bin_sqft <- function(sqft) {
    cut(sqft,
      breaks = c(0, 499, 749, 999, 1499, 1999, 2499, 2999, 3999, Inf),
      labels = c(
        "0 to 499", "500 to 749", "750 to 999",
        "1000 to 1499", "1500 to 1999",
        "2000 to 2499", "2500 to 2999", "3000 to 3999",
        "4000+"
      ),
      right = TRUE
    )
  }


  bin_year <- function(year) {
    cut(year,
      breaks = c(0, 1939, 1959, 1979, 1999, 2009, 2025.1),
      labels = c(
        "<1940", "1940-59", "1960-79", "1980-99",
        "2000-09", "2010s"
      ),
      right = TRUE
    )
  }

  browser()

  # Filter and bin parcels
  ctu_binned <- parcel_data %>%
    filter(
      geog_name == .selected_ctu,
      mc_classification %in% c("single_family_detached",
                               "single_family_attached",
                               "multifamily",
                               "manufactured_home")
    ) %>%
    mutate(
      sqft_bin = bin_sqft(sq_ft_use),
      year_bin = as.character(bin_year(median_year))
    )

  ctu_sf    <- filter(ctu_binned, mc_classification %in% c("single_family_detached", "single_family_attached"))
  ctu_other <- filter(ctu_binned, mc_classification %in% c("multifamily", "manufactured_home"))

  build_scenario <- function(scenario_name, cee_sf, res_other, new_build = FALSE) {
    if (new_build) {
      cee_sf    <- filter(cee_sf,    build_year == "2010s")
      res_other <- filter(res_other, build_year == "2010s")
      sf_by    <- c("mc_classification", "sqft_bin")
      other_by <- "mc_classification"
    } else {
      sf_by    <- c("mc_classification", "sqft_bin", "year_bin" = "build_year")
      other_by <- c("mc_classification", "year_bin" = "build_year")
    }
    sf_part <- left_join(ctu_sf, cee_sf, by = sf_by) %>%
      mutate(scenario_mwh = elec_mwh, scenario_mcf = gas_mcf) %>%
      select(mc_classification, scenario_mwh, scenario_mcf)

    other_part <- left_join(ctu_other, res_other, by = other_by) %>%
      mutate(scenario_mwh = median_kwh / 1000, scenario_mcf = median_mcf) %>%
      select(mc_classification, scenario_mwh, scenario_mcf)

    bind_rows(sf_part, other_part) %>%
      mutate(scenario = scenario_name)
  }

  # bind resstock pairs
  res_baseline <- bind_rows(resstock_tb$mf_baseline,  resstock_tb$manufactured_baseline)
  res_envelope <- bind_rows(resstock_tb$mf_envelope,  resstock_tb$manufactured_envelope)
  res_heatpump <- bind_rows(resstock_tb$mf_heatpump,  resstock_tb$manufactured_heatpump)
  res_combo    <- bind_rows(resstock_tb$mf_combo,     resstock_tb$manufactured_combo)

  scenario_config <- list(
    list("baseline",                ceestock_tb$cee_baseline_sf, res_baseline, FALSE),
    list("new_build",               ceestock_tb$cee_baseline_sf, res_baseline, TRUE),
    list("retrofit",                ceestock_tb$cee_retrofit_sf, res_envelope, FALSE),
    list("heatpump",                ceestock_tb$cee_heatpump_sf, res_heatpump, FALSE),
    list("combination",             ceestock_tb$cee_combined_sf, res_combo,    FALSE),
    list("new_build_leed",          ceestock_tb$cee_retrofit_sf, res_envelope, TRUE),
    list("new_build_heatpump",      ceestock_tb$cee_heatpump_sf, res_heatpump, TRUE),
    list("new_build_leed_heatpump", ceestock_tb$cee_combined_sf, res_combo,    TRUE)
  )

  # extract scenario energy profiles
  ctu_energy_profile <- purrr::map(scenario_config, \(cfg) build_scenario(cfg[[1]], cfg[[2]], cfg[[3]], cfg[[4]])) %>%
    bind_rows()

  return(ctu_energy_profile)
}
