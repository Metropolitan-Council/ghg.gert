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
#' library(ghg.gert)
#'
#' adj_unit_counts(
#'   res_tb = compile_bau_building_energy()$residential,
#'   .selected_ctu = "all"
#' )
#' }
#' @importFrom dplyr filter group_by mutate select ungroup anti_join bind_rows
#' @importFrom tidyr pivot_wider
#' @importFrom cli cli_warn
calc_building_energy <- function(
  .selected_ctu,
  parcel_data = ghg.gert::parcel_ctu,
  resstock_tb = ghg.gert::resstock_summaries,
  ceestock_tb = ghg.gert::ceestock_summaries
) {
  # bin based on ceestock categories
  bin_sqft_attached <- function(sqft) {
    cut(sqft,
      breaks = c(0, 999, 1999, 2999, Inf),
      labels = c(
        "<1000",
        "1000 to 1999",
        "2000 to 2999",
        "3000+"
      ),
      right = TRUE
    )
  }
  # browser()
  bin_sqft_detached <- function(sqft) {
    cut(sqft,
      breaks = c(0, 999, 1499, 1999, 2499, 2999, 3999, Inf),
      labels = c(
        "<1000",
        "1000 to 1499",
        "1500 to 1999",
        "2000 to 2499",
        "2500 to 2999",
        "3000 to 3999",
        "4000+"
      ),
      right = TRUE
    )
  }

  bin_year <- function(year) {
    cut(year,
      breaks = c(0, 1939, 1959, 1979, 1999, 2025.1),
      labels = c(
        "<1940", "1940-59", "1960-79", "1980-99",
        "2000+"
      ),
      right = TRUE
    )
  }

  # Filter and bin parcels
  ctu_binned <- parcel_data %>%
    filter(
      geog_name == .selected_ctu,
      mc_classification %in% c(
        "single_family_detached",
        "single_family_attached",
        "multifamily_units",
        "manufactured_home"
      )
    ) %>%
    mutate(
      sqft_bin = case_when(
        mc_classification == "single_family_detached" ~ bin_sqft_detached(sq_ft_use),
        mc_classification == "single_family_attached" ~ bin_sqft_attached(sq_ft_use),
        TRUE ~ NA
      ), # multifamily and manufactured homes don't use square footage due to data limitations
      year_bin = as.character(bin_year(median_year))
    )

  ctu_sf <- filter(ctu_binned, mc_classification %in% c("single_family_detached", "single_family_attached"))
  ctu_other <- filter(ctu_binned, mc_classification %in% c("multifamily_units", "manufactured_home"))

  build_scenario <- function(scenario_name, cee_sf, res_other, new_build = FALSE) {
    if (new_build) {
      cee_sf <- filter(cee_sf, build_year == "2000+")
      res_other <- filter(res_other, build_year == "2000+")
      sf_by <- c("mc_classification", "sqft_bin")
      other_by <- "mc_classification"
    } else {
      sf_by <- c("mc_classification", "sqft_bin", "year_bin" = "build_year")
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
  res_baseline <- bind_rows(resstock_tb$mf_baseline, resstock_tb$manufactured_baseline)
  res_envelope <- bind_rows(resstock_tb$mf_envelope, resstock_tb$manufactured_envelope)
  res_heatpump <- bind_rows(resstock_tb$mf_heatpump, resstock_tb$manufactured_heatpump)
  res_combo <- bind_rows(resstock_tb$mf_combo, resstock_tb$manufactured_combo)


  scenario_config <- list(
    list("baseline", ceestock_tb$cee_baseline_sf, res_baseline, FALSE),
    list("new_build", ceestock_tb$cee_baseline_sf, res_baseline, TRUE),
    list("retrofit", ceestock_tb$cee_retrofit_sf, res_envelope, FALSE),
    list("heatpump", ceestock_tb$cee_heatpump_sf, res_heatpump, FALSE),
    list("combination", ceestock_tb$cee_combined_sf, res_combo, FALSE),
    list("new_build_heatpump", ceestock_tb$cee_heatpump_sf, res_heatpump, TRUE)
  )

  # extract scenario energy profiles
  ctu_energy_profile <- purrr::map(scenario_config, \(cfg) build_scenario(cfg[[1]], cfg[[2]], cfg[[3]], cfg[[4]])) %>%
    bind_rows()

  ### sustainable new building needs to be worked in manually currently

  res_new_build_sust_sf <- bind_rows(
    resstock_tb$sf_attached_vintagesqft_sust_new_build,
    resstock_tb$sf_detached_vintagesqft_sust_new_build
  )

  resstock_sqft_bin <- function(sqft) {
    cut(sqft,
      breaks = c(0, 999, 1499, 1999, 2499, 2999, Inf),
      labels = c(
        "Less than 1,000", "1,000 to 1,499", "1,500 to 1,999",
        "2,000 to 2,499", "2,500 to 2,999", "3,000 or more"
      ),
      right = TRUE
    )
  }


  ctu_sf_res <- ctu_sf %>%
    mutate(res_sq_ft = resstock_sqft_bin(sq_ft_use))

  res_new_build_sust_out <- bind_rows(
    resstock_tb$mf_sust_new_build,
    resstock_tb$manufactured_sust_new_build,
    left_join(
      ctu_sf_res,
      res_new_build_sust_sf,
      join_by(
        mc_classification,
        res_sq_ft == sqft_bin
      )
    )
  ) %>%
    mutate(
      scenario_mwh = median_kwh / 1000,
      scenario = "new_build_leed"
    ) %>%
    select(mc_classification, scenario_mwh, scenario_mcf = median_mcf, scenario)

  ctu_energy_profile_out <- bind_rows(
    ctu_energy_profile,
    res_new_build_sust_out
  )

  return(ctu_energy_profile_out)
}
