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

  browser()

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

  # Filter and bin parcels
  ctu_binned <- parcel_data %>%
    filter(
      geog_name == .selected_ctu,
      mc_classification %in% c("single_family_detached", "single_family_attached")
    ) %>%
    mutate(
      sqft_bin = bin_sqft(sq_ft_use),
      year_bin = as.character(bin_year(median_year))
    )

  ### calculate ctu single family baseline based on size and year
  ctu_sf_baseline <- left_join(ctu_binned,
      ceestock_tb$cee_baseline_sf,
    by = c(
      "mc_classification",
      "sqft_bin",
      "year_bin" = "build_year"
    )
  ) %>%
    # take the mean mwh and mcf of the two characteristics
    mutate(
      scenario_mwh = elec_mwh,
      scenario_mcf = gas_mcf
    ) %>%
    select(
      mc_classification,
      scenario_mwh,
      scenario_mcf
    )

  ctu_baseline <- bind_rows(
    ctu_sf_baseline,
    resstock_tb$mf_baseline %>%
      mutate(
        scenario_mwh = median_kwh / 1000,
        scenario_mcf = median_mcf
      ) %>%
      select(
        mc_classification,
        scenario_mwh,
        scenario_mcf
      ),
    resstock_tb$manufactured_baseline %>%
      mutate(
        scenario_mwh = median_kwh / 1000,
        scenario_mcf = median_mcf
      ) %>%
      select(
        mc_classification,
        scenario_mwh,
        scenario_mcf
      )
  ) %>%
    mutate(scenario = "baseline")

  ctu_sf_retrofit <- left_join(ctu_binned,
      ceestock_tb$cee_retrofit_sf,
      by = c(
        "mc_classification",
        "sqft_bin",
        "year_bin" = "build_year"
      )
  ) %>%
    mutate(
      scenario_mwh = elec_mwh,
      scenario_mcf = gas_mcf
    ) %>%
    select(
      mc_classification,
      scenario_mwh,
      scenario_mcf
    )

  ctu_retrofit <- bind_rows(
    ctu_sf_retrofit,
    resstock_tb$mf_envelope %>%
      mutate(
        scenario_mwh = median_kwh / 1000,
        scenario_mcf = median_mcf
      ) %>%
      select(
        mc_classification,
        scenario_mwh,
        scenario_mcf
      ),
    resstock_tb$manufactured_envelope %>%
      mutate(
        scenario_mwh = median_kwh / 1000,
        scenario_mcf = median_mcf
      ) %>%
      select(
        mc_classification,
        scenario_mwh,
        scenario_mcf
      )
  ) %>%
    mutate(scenario = "retrofit")

  ctu_sf_heatpump <- left_join(ctu_binned,
                               ceestock_tb$cee_heatpump_sf,
                               by = c(
                                 "mc_classification",
                                 "sqft_bin",
                                 "year_bin" = "build_year"
                               )
  ) %>%
    mutate(
      scenario_mwh = elec_mwh,
      scenario_mcf = gas_mcf
    ) %>%
    select(
      mc_classification,
      scenario_mwh,
      scenario_mcf
    )

  ctu_heatpump <- bind_rows(
    ctu_sf_heatpump,
    resstock_tb$mf_heatpump %>%
      mutate(
        scenario_mwh = median_kwh / 1000,
        scenario_mcf = median_mcf
      ) %>%
      select(
        mc_classification,
        scenario_mwh,
        scenario_mcf
      ),
    resstock_tb$manufactured_heatpump %>%
      mutate(
        scenario_mwh = median_kwh / 1000,
        scenario_mcf = median_mcf
      ) %>%
      select(
        mc_classification,
        scenario_mwh,
        scenario_mcf
      )
  ) %>%
    mutate(scenario = "heatpump")

  ctu_sf_combo <- left_join(ctu_binned,
                              ceestock_tb$cee_combined_sf,
                               by = c(
                                 "mc_classification",
                                 "sqft_bin",
                                 "year_bin" = "build_year"
                               )
  ) %>%
    mutate(
      scenario_mwh = elec_mwh,
      scenario_mcf = gas_mcf
    ) %>%
    select(
      mc_classification,
      scenario_mwh,
      scenario_mcf
    )

  ## combo scenario does not exist for

  ctu_sf_new <- left_join(ctu_binned,
    ceestock_tb$cee_retrofit_sf %>%
      filter(build_year == "2010s"),
    by = c(
      "mc_classification",
      "sqft_bin"
    )
  ) %>%
    mutate(
      scenario_mwh = elec_mwh,
      scenario_mcf = gas_mcf
    ) %>%
    select(
      mc_classification,
      scenario_mwh,
      scenario_mcf
    )

  ctu_new <- bind_rows(
    ctu_sf_new,
    resstock_tb$mf_baseline %>%
      mutate(
        scenario_mwh = median_kwh / 1000,
        scenario_mcf = median_mcf
      ) %>%
      select(
        mc_classification,
        scenario_mwh,
        scenario_mcf
      ),
    resstock_tb$manufactured_envelope %>%
      mutate(
        scenario_mwh = median_kwh / 1000,
        scenario_mcf = median_mcf
      ) %>%
      select(
        mc_classification,
        scenario_mwh,
        scenario_mcf
      )
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
