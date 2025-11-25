#' @title Calculate non-residential building energy
#' @family buildings
#' @family non-residential
#' @family emissions
#'
#' @description Estimates total energy demand
#'      from the non-residential building sector by city/township
#'      for the user-specified scenario, and the business-as-usual scenario.
#'
#' @note `calc_energy_non_residential()` estimates the building energy demand
#'      based on the housing efficiency assumptions. For a function that compiles all
#'      nonresidential strategies refer to [`scen_non_residential_building()`].
#'
#' @param non_res_tb [tibble::tibble()].
#'      Table, table with residential building data.
#'
#' @inheritParams run_module_transportation
#' @inheritParams scen_building_non_residential
#'
#' @return [tibble::tibble()].
#'    A table with columns
#'    `geog_name`,
#'    `inventory_year`,
#'    `geog_id`,
#'    `nonresidential_mwh`,
#'    `nonresidential_mcf`,
#'    `scenario`
#'
#' @examples
#' \dontrun{
#' library(ghg.ccap)
#'
#' calc_ghg_non_residential(
#'   res_tb = building_data$residential,
#'   res_tb_bau = building_data$residential,
#'   .selected_ctu = "all",
#'   .grid_decarbonization_pct = 1,
#'   .enviro_factors = enviro_factors
#' )
#' }
#' @export
calc_energy_non_residential <- function(non_res_tb,
                                    non_res_tb_bau,
                                    .heatpump_start_year,
                                    .heatpump_end_year,
                                    .baseline_year,
                                    .scenario = "alt",
                                    .selected_ctu,
                                    .jobs_heatpump_pct,
                                    .enviro_factors = ghg.ccap::enviro_factors) {
  # cli::cli_progress_message("*** calculating residential ghg emissions \n")

  check_inputs(name = "jobs_heatpump_pct", .jobs_heatpump_pct)
  check_inputs(name = "heatpump_start_year", .heatpump_start_year)

  # browser()

  non_res_tb <- filter_ctu(non_res_tb, .selected_ctu = .selected_ctu)
  non_res_tb_bau <- filter_ctu(non_res_tb_bau, .selected_ctu = .selected_ctu)

  # snag the .selected community designation -- breaks when .selected_city = "all" and just uses Afton/first city
  pluck_commDesgn <- non_res_tb_bau %>%
    summarise(val = first(imagine_designation)) %>%
    pull(val)

  baseline_energy <- left_join(
    filter_ctu(ghg.ccap::building_energy_data$electricity_inventory,
               .selected_ctu = .selected_ctu
    ) %>%
      dplyr::filter(
        inventory_year <= .baseline_year,
        sector == "Business"
      ),
    filter_ctu(ghg.ccap::building_energy_data$natgas_inventory,
               .selected_ctu = .selected_ctu
    ) %>%
      dplyr::filter(
        inventory_year <= .baseline_year,
        sector == "Business"
      ),
    by = join_by(geog_name, geog_id, geog_level, sector, inventory_year)
  )

  # Pull relevant community designation's energy profile for .selected_ctu
  ctu_energy_profile <- ghg.ccap::imagine_commDesgn_mwh_mcf_perJob_perScenario_coefficients %>%
    filter(imagine_designation == pluck_commDesgn) %>%
    mutate(cat_match = case_when(
      scenario == "baseline" ~ "existing_nonretrofit_jobs",
      scenario == "retrofit_efficiency" ~ "retrofit_jobs",
      scenario == "new_non_leed_jobs" ~ "new_non_leed_jobs",
      scenario == "electrification" ~ "heatpump_jobs",
      scenario == "new_leed_jobs" ~ "new_leed_jobs"
      )
    )

  ### adjust the model prediction to the sum of the last 5 observed years
  mwh_adjustment <-
    (baseline_energy %>%
       filter(inventory_year >= (.baseline_year - 4)) %>%
       pull(mwh) %>%
       sum()) /
    (non_res_tb_bau %>%
       filter(inventory_year >= (.baseline_year - 4) & inventory_year <= .baseline_year) %>%
       distinct(geog_name, imagine_designation, inventory_year, value) %>%
       left_join(
         ctu_energy_profile %>%
           filter(scenario == "baseline"),
         by = "imagine_designation"
       ) %>%
       mutate(mwh_pred = value * mwh_per_job) %>%
       pull(mwh_pred) %>%
       sum())

  mcf_adjustment <-
    (baseline_energy %>%
       filter(inventory_year >= (.baseline_year - 4)) %>%
       pull(mcf) %>%
       sum()) /
    (non_res_tb_bau %>%
       filter(inventory_year >= (.baseline_year - 4) & inventory_year <= .baseline_year) %>%
       distinct(geog_name, imagine_designation, inventory_year, value) %>%
       left_join(
         ctu_energy_profile %>%
           filter(scenario == "baseline"),
         by = "imagine_designation"
       ) %>%
       mutate(mcf_pred = value * mcf_per_job) %>%
       pull(mcf_pred) %>%
       sum())


  # heat pump expected energy will be lowered for retrofit buildings
  # ctu average energy load will be split based on heat pump percentage
  ctu_energy_profile_adjustments <- ctu_energy_profile %>% select(-cat_match) %>%
    tidyr::pivot_wider(
      names_from = scenario,
      values_from = c(mwh_per_job, mcf_per_job),
      names_glue = "{scenario}_{.value}"
    ) %>%
    mutate(
      heatpump_mwh = electrification_mwh_per_job - baseline_mwh_per_job, # heat pump scen mwh addition to baseline is assumed to be all heating gain
      retrofit_heating_pct = (retrofit_efficiency_mcf_per_job - electrification_mcf_per_job) / # calculate what amount of nat gas was for heating in retrofit
        (baseline_mcf_per_job - electrification_mcf_per_job),
      appliance_mcf = electrification_mcf_per_job # how much nat gas used when no heating required?
    ) %>%
    select(
      imagine_designation,
      heatpump_mwh,
      retrofit_heating_pct,
      appliance_mcf
    )


  ### calculate heat pump effects here
  energy_calc <- function(tb,
                          .heatpump_start_year = .heatpump_start_year,
                          .heatpump_end_year = .heatpump_end_year,
                          .jobs_heatpump_pct = .jobs_heatpump_pct
                          ) {

    ### ramp up heat pump installation evenly from start year to end year
    ramp_years <- .heatpump_start_year:.heatpump_end_year
    n_ramp <- length(ramp_years)

    pct_ramp <- tibble::tibble(
      inventory_year = ramp_years,
      hp_pct = seq(
        from = .jobs_heatpump_pct / n_ramp,
        to = .jobs_heatpump_pct,
        length.out = n_ramp
      )
    )

    # Join pct values by condition
    pct_by_year <- tibble::tibble(inventory_year = 2005:2050) %>%
      left_join(pct_ramp, by = "inventory_year") %>%
      dplyr::mutate(
        hp_pct = dplyr::case_when(
          inventory_year < .heatpump_start_year ~ 0,
          inventory_year > .heatpump_end_year ~ .jobs_heatpump_pct,
          TRUE ~ hp_pct
        )
      )

    # browser()
    energy_tb <- tb %>%
      filter(inventory_year > .baseline_year) %>%
      left_join(pct_by_year, by = "inventory_year") %>%
      left_join(ctu_energy_profile,
                by = join_by(efficiency_description == cat_match,
                             imagine_designation == imagine_designation)
                ) %>%
      left_join(ctu_energy_profile_adjustments,
                by = join_by("imagine_designation")
      ) %>%
      mutate(
        non_residential_mwh = case_when( # will take the weighted average of heatpump/non-heatpump homes
          efficiency_description  %in% c("existing_nonretrofit_jobs", "new_non_leed_jobs") ~
            ((mwh_per_job * (1 - hp_pct)) + ((mwh_per_job + heatpump_mwh) * hp_pct)) * efficiency_unit_value * mwh_adjustment,
          efficiency_description == "retrofit_jobs" ~
            ((mwh_per_job * (1 - hp_pct)) + ((mwh_per_job + (heatpump_mwh * retrofit_heating_pct)) * hp_pct)) * efficiency_unit_value * mwh_adjustment,
          efficiency_description == "new_leed_jobs" ~
            (((mwh_per_job * (1 - hp_pct)) + ((mwh_per_job + (heatpump_mwh)) * hp_pct))) * .enviro_factors$LEED_GOLD_REDUCTION_PCT * efficiency_unit_value * mwh_adjustment
        ),
        non_residential_mcf = case_when(
          efficiency_description %in% c("existing_nonretrofit_jobs", "new_non_leed_jobs") ~
            ((mcf_per_job * (1 - hp_pct)) + (appliance_mcf * hp_pct)) * efficiency_unit_value * mcf_adjustment,
          efficiency_description == "retrofit_jobs" ~
            ((mcf_per_job * (1 - hp_pct)) + (appliance_mcf * hp_pct)) * efficiency_unit_value * mcf_adjustment,
          efficiency_description == "new_leed_jobs" ~
            (((mcf_per_job * (1 - hp_pct)) * .enviro_factors$LEED_GOLD_REDUCTION_PCT + (appliance_mcf * hp_pct))) * efficiency_unit_value * mcf_adjustment
        )
      ) %>%
      dplyr::group_by(geog_name, geog_id, inventory_year) %>%
      dplyr::summarize(
        non_residential_mwh = sum(non_residential_mwh),
        non_residential_mcf = sum(non_residential_mcf)
      ) %>%
      dplyr::select(
        geog_name,
        inventory_year,
        geog_id,
        non_residential_mwh,
        non_residential_mcf
      )

    return(energy_tb)
  }


  energy_bau <- bind_rows(
    baseline_energy %>%
      select(geog_name,
             geog_id,
             inventory_year,
             non_residential_mwh = mwh,
             non_residential_mcf = mcf
      ),
    energy_calc(
      tb = non_res_tb_bau,
      .heatpump_start_year = .heatpump_start_year,
      .heatpump_end_year = .heatpump_end_year,
      .jobs_heatpump_pct = 0
    )
  )


  energy_strategy <- bind_rows(
    baseline_energy %>%
      select(geog_name,
             geog_id,
             inventory_year,
             non_residential_mwh = mwh,
             non_residential_mcf = mcf
      ),
    energy_calc(
      tb = non_res_tb,
      .heatpump_start_year = .heatpump_start_year,
      .heatpump_end_year = .heatpump_end_year,
      .jobs_heatpump_pct = .jobs_heatpump_pct
    )
  )

  energy_final <- bind_rows(
    energy_bau %>%
      mutate(scenario = "bau"),
    energy_strategy %>%
      mutate(scenario = .scenario)
  )

  return(energy_final)
}


# ALL OF THE BELOW CODE IS TEMPORARY TO FACILITATE GETTING THE FORECAST BAU NUMBERS OUT OF THE PACKAGE EARLY.
# elec_inv <- readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/205-ctu-ghg-compiler/_energy/data/_ctu_electricity_emissions.RDS") %>%
#   rename(
#     value_emissions_elec = value_emissions
#   ) %>%
#   distinct(ctu_name, ctu_class, sector, inventory_year, .keep_all = TRUE) %>%
#   select(
#     -category, -source, -Source, -factor_source, -mt_co2e_mwh, -units_emissions
#   )
#
# ng_inv <- readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/205-ctu-ghg-compiler/_energy/data/_ctu_natgas_emissions.RDS") %>%
#   rename(
#     value_emissions_ng = value_emissions
#   ) %>%
#   distinct(ctu_name, ctu_class, sector, inventory_year, .keep_all = TRUE) %>%
#   select(
#     -category, -source, -factor_source, -mt_co2e_mcf, -units_emissions
#   )
#
# inventories_combined <- elec_inv %>%
#   left_join(ng_inv,
#             by = join_by(ctu_name, ctu_class, sector, inventory_year)
#   ) %>%
#   #some 2015-2018 (and two 2022... Minnetrista and Medicine Lake) missing from NG data, need to revisit
#   #filter(!is.na(mcf) | !is.na(mwh)) %>%
#   filter(sector == "Business") %>%
#   left_join(
#     cprg_ctu_desgn,
#     by = join_by("ctu_name", "ctu_class")
#   )
#
#
# # MWH AND MCF ADJUSTMENT
# # Observed inventories (both electricity and natural gas)
# obs_df <- inventories_combined %>%
#   filter(inventory_year >= (.baseline_year - 4)) %>%
#   group_by(ctu_name, ctu_class, sector) %>%
#   summarize(
#     ctu_five_yr_window_mwh = sum(mwh, na.rm = TRUE),
#     ctu_five_yr_window_mcf = sum(mcf, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   filter(sector == "Business") %>%
#   select(ctu_name, ctu_class, ctu_five_yr_window_mwh, ctu_five_yr_window_mcf)
#
# # Predicted values for inventory years for both mwh & mcf
# pred_df <- non_res_tb_bau %>%
#   filter(inventory_year >= (.baseline_year - 4) & inventory_year <= .baseline_year) %>%
#   distinct(geog_name, geog_level, sp_categories, imagine_designation, inventory_year, value) %>%
#   left_join(
#     commDesgn_energy_profile %>% filter(scenario == "baseline"),
#     by = "imagine_designation"
#   ) %>%
#   mutate(
#     mwh_pred = value * mwh_per_job,
#     mcf_pred = value * mcf_per_job
#   ) %>%
#   group_by(geog_name, geog_level, sp_categories) %>%
#   summarize(
#     ctu_five_yr_window_pred_mwh = sum(mwh_pred, na.rm = TRUE),
#     ctu_five_yr_window_pred_mcf = sum(mcf_pred, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   transmute(ctu_name = geog_name,
#             ctu_class = geog_level,
#             ctu_five_yr_window_pred_mwh,
#             ctu_five_yr_window_pred_mcf)
#
#
# # Join and compute both adjustment ratios at ctu_name-ctu_class granularity. Inner join to remove counties from pred_df.
# adjustments <- inner_join(obs_df, pred_df, by = c("ctu_name", "ctu_class")) %>%
#   mutate(
#     mwh_adjustment = case_when(
#       is.na(ctu_five_yr_window_pred_mwh) | ctu_five_yr_window_pred_mwh == 0 ~ NA_real_,
#       TRUE ~ ctu_five_yr_window_mwh / ctu_five_yr_window_pred_mwh
#     ),
#     mcf_adjustment = case_when(
#       is.na(ctu_five_yr_window_pred_mcf) | ctu_five_yr_window_pred_mcf == 0 ~ NA_real_,
#       TRUE ~ ctu_five_yr_window_mcf / ctu_five_yr_window_pred_mcf
#     )
#   ) %>%
#   select(ctu_name, ctu_class, mwh_adjustment, mcf_adjustment)
#
#
# temp_bau_forecast_approach <- non_res_tb_bau %>%
#   distinct(geog_name, geog_level, imagine_designation, inventory_year, value) %>%
#   left_join(
#     commDesgn_energy_profile %>%
#       filter(scenario == "baseline"),
#     by = "imagine_designation"
#   ) %>%
#   filter(!is.na(imagine_designation)) %>%
#   mutate(mcf_pred = value * mcf_per_job,
#          mwh_pred = value * mwh_per_job) %>%
#   left_join(adjustments,
#             by = join_by(geog_name == ctu_name,
#                          geog_level == ctu_class)
#   ) %>%
#   mutate(mcf_adj = mcf_pred * mcf_adjustment,
#          mwh_adj = mwh_pred * mwh_adjustment
#   )
#
# # Clean and harmonize observed and predicted
# # Observed inventories (2005–2022)
# observed_clean <- inventories_combined %>%
#   transmute(
#     ctu_name,
#     ctu_class,
#     sector = "Business",
#     imagine_designation,
#     inventory_year,
#     mwh = as.numeric(mwh),
#     mcf = as.numeric(mcf),
#     source = "observed",
#     # keep the raw source fields, but also a concise note
#     data_source_elec = data_source.x,
#     data_source_ng   = data_source.y,
#   ) %>%
#   filter(inventory_year >= 2005, inventory_year <= 2022)
#
# # Adjusted forecast (2023–2050),
# forecast_clean <- temp_bau_forecast_approach %>%
#   transmute(
#     ctu_name = geog_name,
#     ctu_class = geog_level,
#     sector = "Business",
#     imagine_designation,
#     inventory_year,
#     mwh = as.numeric(mwh_adj),            # use adjusted values
#     mcf = as.numeric(mcf_adj),            # use adjusted values
#     source = "Adjusted forecast: jobs × baseline coeffs × CTU-level 5yr ratio (obs/pred)",
#     # keep the adjustment scalars so you can audit later (still "minimal" + meaningful)
#     mwh_adjustment,
#     mcf_adjustment
#   ) %>%
#   filter(inventory_year >= 2023, inventory_year <= 2050)
#
# # Combine into one clean df
# nonres_bau_2005_2050 <- bind_rows(
#   observed_clean,
#   forecast_clean
# ) %>%
#   arrange(ctu_name, ctu_class, inventory_year)
#
# write.csv(nonres_bau_2005_2050, "C:/Users/LimeriSA/Documents/Projects/ghg.ccap/data-raw/building_energy_data_processing/nonres_bau_2005_2050.csv")
#
#
# # --- 3) REGION plot: actual vs predicted (color = source) --------------------
#
# region_series <- nonres_bau_2005_2050 %>%
#   group_by(inventory_year, source) %>%
#   summarize(
#     mwh = sum(mwh, na.rm = TRUE),
#     mcf = sum(mcf, na.rm = TRUE),
#     .groups = "drop"
#   )
#
# # Electricity (MWh) region-wide
# ggplot(region_series, aes(x = inventory_year, y = mwh, color = source)) +
#   geom_line(linewidth = 1) +
#   scale_x_continuous(breaks = seq(2005, 2050, 5)) +
#   labs(
#     title = "Region-wide Business Electricity (MWh), 2005–2050",
#     x = NULL, y = "MWh", color = NULL
#   ) +
#   theme_minimal(base_size = 12)
#
# #If you also want a natural gas region plot (MCF), uncomment:
# ggplot(region_series, aes(x = inventory_year, y = mcf, color = source)) +
#   geom_line(linewidth = 1) +
#   scale_x_continuous(breaks = seq(2005, 2050, 5)) +
#   labs(
#     title = "Region-wide Business Natural Gas (MCF), 2005–2050",
#     x = NULL, y = "MCF", color = NULL
#   ) +
#   theme_minimal(base_size = 12)
#
# # --- 4) FACET plot by city, color-coded by imagine_designation --------------
#
# # For the city facets, show MWh; encode "actual/predicted" as linetype and
# # land-use designation as color (so you can read both at once).
# # ----- Build a clean city-level series you can trust -----
# city_series <- nonres_bau_2005_2050 %>%
#   # keep cities and the Business sector (matches your observed sample + forecast)
#   filter(ctu_class == "CITY", sector == "Business") %>%
#   # keep only rows that can actually plot
#   drop_na(ctu_name, imagine_designation, inventory_year, mwh) %>%
#   mutate(
#     # nice labels without needing forcats
#     source = dplyr::recode(
#       source,
#       "observed" = "Actual (Observed)",
#       "forecast_adjusted" = "Predicted (Adjusted)"
#     )
#   ) %>%
#   arrange(ctu_name, imagine_designation, source, inventory_year)
#
# # ----- Quick diagnostics (helps avoid silent empty data) -----
# if (nrow(city_series) == 0) {
#   message("city_series is empty after filtering. Here are some quick checks:")
#   message("Distinct ctu_class: ", paste(unique(nonres_bau_2005_2050$ctu_class), collapse = ", "))
#   message("Distinct sector: ", paste(unique(nonres_bau_2005_2050$sector), collapse = ", "))
#   message("Years: ", paste(range(nonres_bau_2005_2050$inventory_year, na.rm = TRUE), collapse = "–"))
# }
#
# # Optional: view counts by label to confirm data presence
# city_series %>%
#   count(source) %>%
#   print(n = 50)
#
# # Split once for plotting
# city_obs <- filter(city_series, source == "Actual (Observed)")
# city_fc  <- filter(city_series, source == "Predicted (Adjusted)")
#
# # If either side is empty, ggplot can still draw the other layer safely.
# ggplot() +
#   # Actual
#   geom_line(
#     data = city_obs,
#     aes(x = inventory_year, y = mwh,
#         color = imagine_designation,
#         linetype = "Actual (Observed)",
#         group = interaction(ctu_name, imagine_designation)),
#     linewidth = 0.8
#   ) +
#   # Forecast
#   geom_line(
#     data = city_fc,
#     aes(x = inventory_year, y = mwh,
#         color = imagine_designation,
#         linetype = "Predicted (Adjusted)",
#         group = interaction(ctu_name, imagine_designation)),
#     linewidth = 0.8
#   ) +
#   scale_linetype_manual(
#     name = NULL,
#     values = c("Actual (Observed)" = "solid",
#                "Predicted (Adjusted)" = "longdash")
#   ) +
#   scale_x_continuous(breaks = seq(2005, 2050, 5)) +
#   labs(
#     title = "Business Electricity (MWh) by City, 2005–2050",
#     subtitle = "Color = Imagine designation; Linetype = Actual vs Predicted",
#     x = NULL, y = "MWh", color = "Imagine designation"
#   ) +
#   facet_wrap(~ ctu_name, scales = "free_y") +
#   theme_minimal(base_size = 11) +
#   theme(legend.position = "bottom")

# --- (Optional) If you want to save the harmonized table for downstream use ---
# write_csv(nonres_bau_2005_2050, "nonres_business_bau_2005_2050.csv")
