### compile_resstock_data.R
###
### Import and summarize ResStock scenario data for building energy profiles.
### Filtered to the seven-county Twin Cities metro area to match the
### CEEStock ground-truth data and the ghg.ccap pipeline geography.
###
### Includes end-use decomposition (heating vs appliances) for constructing
### full-electrification profiles downstream.
###
### QC check verifies that ResStock maintains SFA < SFD at same sqft/vintage,
### which is required for the downstream scalar correction approach.
###
### Dependency chain:
###   this script  ->  resstock_summaries
### Contact Peter Wilfahrt for raw file access

library(dplyr)
library(tidyr)
library(stringr)
library(readr)

metro_counties <- c(
  "Anoka County", "Carver County", "Dakota County",
  "Hennepin County", "Ramsey County", "Scott County", "Washington County"
)

bin_sqft <- function(sqft) {
  cut(sqft,
    breaks = c(0, 999, 1499, 1999, 2499, 2999, Inf),
    labels = c(
      "Less than 1,000", "1,000 to 1,499", "1,500 to 1,999",
      "2,000 to 2,499", "2,500 to 2,999", "3,000 or more"
    ),
    right = TRUE
  )
}


# -- Load function -------------------------------------------------------------

load_resstock <- function(path, heating_fuel_filter = "Natural Gas") {
  raw <- readr::read_csv(path, show_col_types = FALSE) %>%
    janitor::clean_names()

  # Accommodate files that use "in_county" instead of "in_county_name"
  if (!"in_county_name" %in% names(raw) && "in_county" %in% names(raw)) {
    county_lookup <- geog_index %>%
      filter(geog_level == "COUNTY") %>%
      select(geog_id, county_name = geog_name)
    raw <- raw %>%
      mutate(
        in_county_fips = paste0(substr(in_county, 2, 3), substr(in_county, 5, 7))
      ) %>%
      left_join(county_lookup, by = c("in_county_fips" = "geog_id")) %>%
      mutate(in_county_name = county_name) %>%
      select(-county_name, -in_county_fips)
  }


  base_cols <- c(
    sqft = "in_sqft",
    build_year = "in_vintage_acs",
    building_type = "in_geometry_building_type_recs",
    heating_fuel = "in_heating_fuel",
    hvac_heating_type = "in_hvac_heating_type",
    county_name = "in_county_name",
    building_kwh = "out_electricity_net_energy_consumption_kwh",
    building_nat_gas_kwh = "out_natural_gas_total_energy_consumption_kwh"
  )

  ng_enduse_cols <- c(
    ng_heating_kwh   = "out_natural_gas_heating_energy_consumption_kwh",
    ng_hp_bkup_kwh   = "out_natural_gas_heating_hp_bkup_energy_consumption_kwh",
    ng_hot_water_kwh = "out_natural_gas_hot_water_energy_consumption_kwh",
    ng_dryer_kwh     = "out_natural_gas_clothes_dryer_energy_consumption_kwh",
    ng_range_kwh     = "out_natural_gas_range_oven_energy_consumption_kwh",
    ng_fireplace_kwh = "out_natural_gas_fireplace_energy_consumption_kwh",
    ng_grill_kwh     = "out_natural_gas_grill_energy_consumption_kwh",
    ng_lighting_kwh  = "out_natural_gas_lighting_energy_consumption_kwh"
  )

  prop_enduse_cols <- c(
    prop_heating_kwh   = "out_propane_heating_energy_consumption_kwh",
    prop_hp_bkup_kwh   = "out_propane_heating_hp_bkup_energy_consumption_kwh",
    prop_hot_water_kwh = "out_propane_hot_water_energy_consumption_kwh",
    prop_range_kwh     = "out_propane_range_oven_energy_consumption_kwh",
    prop_total_kwh     = "out_propane_total_energy_consumption_kwh"
  )

  all_desired <- c(base_cols, ng_enduse_cols, prop_enduse_cols)
  available <- intersect(all_desired, names(raw))

  out <- raw %>%
    select(all_of(available)) %>%
    rename(any_of(setNames(available, names(all_desired)[match(available, all_desired)])))

  for (nm in names(all_desired)) {
    if (!nm %in% names(out)) out[[nm]] <- 0
  }

  out %>%
    filter(
      heating_fuel == heating_fuel_filter,
      county_name %in% metro_counties
    ) %>%
    mutate(
      building_mcf = building_nat_gas_kwh * 0.00329026,
      ng_heating_mcf = (ng_heating_kwh + ng_hp_bkup_kwh) * 0.00329026,
      ng_appliance_mcf = (ng_hot_water_kwh + ng_dryer_kwh + ng_range_kwh +
        ng_fireplace_kwh + ng_grill_kwh + ng_lighting_kwh) * 0.00329026,
      prop_heating_mmbtu = (prop_heating_kwh + prop_hp_bkup_kwh) * 0.003412,
      prop_appliance_mmbtu = (prop_hot_water_kwh + prop_range_kwh) * 0.003412,
      prop_total_mmbtu = prop_total_kwh * 0.003412,
      build_year = if_else(build_year %in% c("2000-09", "2010s"), "2000+", build_year),
      mc_classification = case_when(
        grepl("Multi", building_type, ignore.case = TRUE) ~ "multifamily_units",
        grepl("Detached", building_type, ignore.case = TRUE) ~ "single_family_detached",
        grepl("Attached", building_type, ignore.case = TRUE) ~ "single_family_attached",
        grepl("Mobile", building_type, ignore.case = TRUE) ~ "manufactured_homes",
        TRUE ~ "other"
      ),
      sqft_bin = bin_sqft(sqft)
    )
}


# -- Summarize functions -------------------------------------------------------

summarize_resstock <- function(df, building_pattern, grouping_vars, min_n = 5) {
  df %>%
    filter(grepl(building_pattern, building_type, ignore.case = TRUE)) %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarize(
      median_kwh = median(building_kwh, na.rm = TRUE),
      median_mcf = median(building_mcf, na.rm = TRUE),
      n_obs = n(),
      .groups = "drop"
    ) %>%
    filter(n_obs >= min_n)
}

summarize_enduse <- function(df, building_pattern, grouping_vars,
                             fuel = c("natural_gas", "propane")) {
  fuel <- match.arg(fuel)
  df %>%
    filter(grepl(building_pattern, building_type, ignore.case = TRUE)) %>%
    group_by(across(all_of(grouping_vars))) %>%
    {
      if (fuel == "natural_gas") {
        summarize(.,
          median_kwh = median(building_kwh, na.rm = TRUE),
          median_mcf = median(building_mcf, na.rm = TRUE),
          median_heating_mcf = median(ng_heating_mcf, na.rm = TRUE),
          median_appliance_mcf = median(ng_appliance_mcf, na.rm = TRUE),
          median_heating_mmbtu = median(ng_heating_mcf, na.rm = TRUE) * 1.037,
          median_appliance_mmbtu = median(ng_appliance_mcf, na.rm = TRUE) * 1.037,
          n_obs = n(),
          .groups = "drop"
        )
      } else {
        summarize(.,
          median_kwh = median(building_kwh, na.rm = TRUE),
          median_total_mmbtu = median(prop_total_mmbtu, na.rm = TRUE),
          median_heating_mmbtu = median(prop_heating_mmbtu, na.rm = TRUE),
          median_appliance_mmbtu = median(prop_appliance_mmbtu, na.rm = TRUE),
          n_obs = n(),
          .groups = "drop"
        )
      }
    }
}


# -- Load scenario files -------------------------------------------------------

resstock_path <- "./data-raw/building_energy_data_processing/resstock data/"

baseline <- load_resstock(file.path(resstock_path, "MN_baseline_metadata_and_annual_results.csv"))
heatpump <- load_resstock(file.path(resstock_path, "MN_upgrade02_metadata_and_annual_results_heat_pump.csv"))
envelope <- load_resstock(file.path(resstock_path, "MN_upgrade16_metadata_and_annual_results.csv"))
combo <- load_resstock(file.path(resstock_path, "MN_upgrade07_metadata_and_annual_results.csv"))

sust_new_build <- load_resstock(
  file.path(resstock_path, "MN_upgrade15_metadata_and_annual_results.csv")
) %>%
  filter(build_year == "2000+", building_nat_gas_kwh == 0)

baseline_propane <- load_resstock(
  file.path(resstock_path, "MN_baseline_metadata_and_annual_results.csv"),
  heating_fuel_filter = "Propane"
)


# -- Summary list --------------------------------------------------------------

resstock_summaries <- list(
  # -- Baseline ----------------------------------------------------------------
  mf_baseline = summarize_resstock(baseline, "Multi", c("mc_classification", "build_year")),
  manufactured_baseline = summarize_resstock(baseline, "Mobile", c("mc_classification", "build_year")),

  # -- Heat pump ---------------------------------------------------------------
  mf_heatpump = summarize_resstock(heatpump, "Multi", c("mc_classification", "build_year")),
  manufactured_heatpump = summarize_resstock(heatpump, "Mobile", c("mc_classification", "build_year")),

  # -- Envelope ----------------------------------------------------------------
  mf_envelope = summarize_resstock(envelope, "Multi", c("mc_classification", "build_year")),
  manufactured_envelope = summarize_resstock(envelope, "Mobile", c("mc_classification", "build_year")),

  # -- Combination -------------------------------------------------------------
  mf_combo = summarize_resstock(combo, "Multi", c("mc_classification", "build_year")),
  manufactured_combo = summarize_resstock(combo, "Mobile", c("mc_classification", "build_year")),

  # -- Sustainable new build ---------------------------------------------------
  mf_sust_new_build = summarize_resstock(sust_new_build, "Multi", c("mc_classification", "build_year")),
  manufactured_sust_new_build = summarize_resstock(sust_new_build, "Mobile", c("mc_classification", "build_year")),

  # -- Vintage x sqft (SF, all scenarios) --------------------------------------
  # Used by the scalar correction and SFA profile construction in
  # compile_building_summaries.R
  sf_detached_vintagesqft_baseline = summarize_resstock(
    baseline, "Detached", c("mc_classification", "build_year", "sqft_bin")
  ),
  sf_attached_vintagesqft_baseline = summarize_resstock(
    baseline, "Attached", c("mc_classification", "build_year", "sqft_bin")
  ),
  sf_detached_vintagesqft_heatpump = summarize_resstock(
    heatpump, "Detached", c("mc_classification", "build_year", "sqft_bin")
  ),
  sf_attached_vintagesqft_heatpump = summarize_resstock(
    heatpump, "Attached", c("mc_classification", "build_year", "sqft_bin")
  ),
  sf_detached_vintagesqft_envelope = summarize_resstock(
    envelope, "Detached", c("mc_classification", "build_year", "sqft_bin")
  ),
  sf_attached_vintagesqft_envelope = summarize_resstock(
    envelope, "Attached", c("mc_classification", "build_year", "sqft_bin")
  ),
  sf_detached_vintagesqft_combo = summarize_resstock(
    combo, "Detached", c("mc_classification", "build_year", "sqft_bin")
  ),
  sf_attached_vintagesqft_combo = summarize_resstock(
    combo, "Attached", c("mc_classification", "build_year", "sqft_bin")
  ),
  sf_detached_vintagesqft_sust_new_build = summarize_resstock(
    sust_new_build, "Detached", c("mc_classification", "build_year", "sqft_bin")
  ),
  sf_attached_vintagesqft_sust_new_build = summarize_resstock(
    sust_new_build, "Attached", c("mc_classification", "build_year", "sqft_bin")
  ),

  # -- End-use decomposition ---------------------------------------------------
  mf_baseline_enduse = summarize_enduse(
    baseline, "Multi", c("mc_classification", "build_year"),
    fuel = "natural_gas"
  ),
  manufactured_baseline_enduse = summarize_enduse(
    baseline, "Mobile", c("mc_classification", "build_year"),
    fuel = "natural_gas"
  ),
  sf_detached_baseline_enduse = summarize_enduse(
    baseline, "Detached", c("mc_classification", "build_year"),
    fuel = "natural_gas"
  ),
  sf_attached_baseline_enduse = summarize_enduse(
    baseline, "Attached", c("mc_classification", "build_year"),
    fuel = "natural_gas"
  ),

  # -- Propane baselines -------------------------------------------------------
  sf_detached_propane_enduse = summarize_enduse(
    baseline_propane, "Detached", c("mc_classification", "build_year"),
    fuel = "propane"
  ),
  manufactured_propane_enduse = summarize_enduse(
    baseline_propane, "Mobile", c("mc_classification", "build_year"),
    fuel = "propane"
  ),
  mf_propane_enduse = summarize_enduse(
    baseline_propane, "Multi", c("mc_classification"),
    fuel = "propane"
  )
)


# -- QC: verify SFA < SFD at same sqft/vintage ---------------------------------
# This is a precondition for the scalar correction approach: if ResStock
# internally has SFA > SFD at any bin, the scalar correction won't fix it.

cli::cli_alert_info("\n-- SFA < SFD check (ResStock baseline, metro-filtered) --")

sfa_sfd_check <- resstock_summaries$sf_attached_vintagesqft_baseline %>%
  inner_join(
    resstock_summaries$sf_detached_vintagesqft_baseline,
    by = c("build_year", "sqft_bin"),
    suffix = c("_sfa", "_sfd")
  ) %>%
  mutate(
    sfa_lt_sfd_kwh = median_kwh_sfa <= median_kwh_sfd,
    sfa_lt_sfd_mcf = median_mcf_sfa <= median_mcf_sfd
  )

violations <- sfa_sfd_check %>%
  filter(!sfa_lt_sfd_kwh | !sfa_lt_sfd_mcf)

if (nrow(violations) > 0) {
  cli::cli_alert_warning(sprintf(
    "ResStock SFA exceeds SFD at %d vintage x sqft bins (see below).\n",
    nrow(violations)
  ))
  # this is a relatively small proportion of the total housing stock
  # though not ideal, it won't significantly impact the overall analysis

  print(
    violations %>%
      select(build_year, sqft_bin,
        kwh_sfa = median_kwh_sfa, kwh_sfd = median_kwh_sfd, kwh_ok = sfa_lt_sfd_kwh,
        mcf_sfa = median_mcf_sfa, mcf_sfd = median_mcf_sfd, mcf_ok = sfa_lt_sfd_mcf
      ),
    n = Inf
  )
} else {
  cli::cli_alert_success("  PASS: SFA <= SFD at all 30 vintage x sqft bins")
}

cli::cli_alert_info(sprintf(
  "\n  Metro baseline counts: SFD=%d, SFA=%d, MF=%d, Mfg=%d",
  sum(baseline$mc_classification == "single_family_detached"),
  sum(baseline$mc_classification == "single_family_attached"),
  sum(baseline$mc_classification == "multifamily_units"),
  sum(baseline$mc_classification == "manufactured_homes")
))


usethis::use_data(resstock_summaries, overwrite = TRUE)
