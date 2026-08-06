### import ResStock data for electrification and retrofit elasticities
###
### Expanded from original to pull natural gas and propane end-use columns
### (heating, hot water, dryer, range, etc.) needed for full-electrification
### profile construction. The end-use decomposition tells us what fraction of
### total combustion fuel is heating vs appliances, so we can apply CEEStock-
### derived electrification ratios to MF and manufactured housing types where
### CEEStock has no direct coverage.
###
### Propane end-use data confirms that propane-heated homes are already
### mostly electric for appliances (1.6–3.6% of propane is non-heating),
### so the full-electrification impact for propane houses is almost entirely
### about heating conversion.

# library(dplyr, tidyr, readr)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

## function to match eia sqft bins
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

# ── Load function ────────────────────────────────────────────────────────────
#
# Pulls total energy plus end-use decomposition for the primary combustion
# fuel. heating_fuel_filter defaults to "Natural Gas"; pass "Propane" for
# propane-heated homes (same column structure, different fuel columns).

load_resstock <- function(path, heating_fuel_filter = "Natural Gas") {

  raw <- readr::read_csv(path, show_col_types = FALSE) %>%
    janitor::clean_names()

  # Column names after janitor: dots → underscores
  # e.g. out.natural_gas.heating.energy_consumption.kwh → out_natural_gas_heating_energy_consumption_kwh

  base_cols <- c(
    sqft             = "in_sqft",
    build_year       = "in_vintage_acs",
    building_type    = "in_geometry_building_type_recs",
    heating_fuel     = "in_heating_fuel",
    hvac_heating_type = "in_hvac_heating_type",
    building_kwh     = "out_electricity_net_energy_consumption_kwh",
    building_nat_gas_kwh = "out_natural_gas_total_energy_consumption_kwh"
  )

  # Natural gas end-use columns
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

  # Propane end-use columns
  prop_enduse_cols <- c(
    prop_heating_kwh   = "out_propane_heating_energy_consumption_kwh",
    prop_hp_bkup_kwh   = "out_propane_heating_hp_bkup_energy_consumption_kwh",
    prop_hot_water_kwh = "out_propane_hot_water_energy_consumption_kwh",
    prop_range_kwh     = "out_propane_range_oven_energy_consumption_kwh",
    prop_total_kwh     = "out_propane_total_energy_consumption_kwh"
  )

  # Only select columns that actually exist in the file
  all_desired <- c(base_cols, ng_enduse_cols, prop_enduse_cols)
  available <- intersect(all_desired, names(raw))
  missing <- setdiff(all_desired, names(raw))
  if (length(missing) > 0) {
    message("  Note: columns not found (will be 0): ", paste(names(all_desired)[all_desired %in% missing], collapse = ", "))
  }

  # Build selection — missing columns get filled with 0
  out <- raw %>%
    select(all_of(available)) %>%
    rename(any_of(setNames(available, names(all_desired)[match(available, all_desired)])))

  # Add missing columns as 0
  for (nm in names(all_desired)) {
    if (!nm %in% names(out)) {
      out[[nm]] <- 0
    }
  }

  out %>%
    filter(heating_fuel == heating_fuel_filter) %>%
    mutate(
      # Total combustion fuel in MCF (for natgas) or kWh (propane stays in kWh
      # until downstream converts to mmBtu)
      building_mcf = building_nat_gas_kwh * 0.00329026,

      # Natural gas end-use decomposition (MCF)
      ng_heating_mcf   = (ng_heating_kwh + ng_hp_bkup_kwh) * 0.00329026,
      ng_appliance_mcf = (ng_hot_water_kwh + ng_dryer_kwh + ng_range_kwh +
                            ng_fireplace_kwh + ng_grill_kwh + ng_lighting_kwh) * 0.00329026,

      # Propane end-use decomposition (mmBtu)
      # 1 kWh = 0.003412 mmBtu
      prop_heating_mmbtu   = (prop_heating_kwh + prop_hp_bkup_kwh) * 0.003412,
      prop_appliance_mmbtu = (prop_hot_water_kwh + prop_range_kwh) * 0.003412,
      prop_total_mmbtu     = prop_total_kwh * 0.003412,

      # Standard derived columns
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

## ── Summarize functions ───────────────────────────────────────────────────────

# Original summarizer: median total kwh and mcf (used for all scenarios)
summarize_resstock <- function(df, building_pattern, grouping_vars) {
  df %>%
    filter(grepl(building_pattern, building_type, ignore.case = TRUE)) %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarize(
      median_kwh = median(building_kwh, na.rm = TRUE),
      median_mcf = median(building_mcf, na.rm = TRUE),
      .groups = "drop"
    )
}

# End-use summarizer: heating/appliance split for baseline profiles.
# Used to construct full-electrification profiles via CEEStock ratios.
summarize_enduse <- function(df, building_pattern, grouping_vars,
                             fuel = c("natural_gas", "propane")) {
  fuel <- match.arg(fuel)

  df %>%
    filter(grepl(building_pattern, building_type, ignore.case = TRUE)) %>%
    group_by(across(all_of(grouping_vars))) %>%
    {
      if (fuel == "natural_gas") {
        summarize(.,
                  median_kwh          = median(building_kwh, na.rm = TRUE),
                  median_mcf          = median(building_mcf, na.rm = TRUE),
                  median_heating_mcf  = median(ng_heating_mcf, na.rm = TRUE),
                  median_appliance_mcf = median(ng_appliance_mcf, na.rm = TRUE),
                  # Convert to mmBtu for fuel-agnostic ratio application
                  # 1 MCF = 1.037 mmBtu
                  median_heating_mmbtu  = median(ng_heating_mcf, na.rm = TRUE) * 1.037,
                  median_appliance_mmbtu = median(ng_appliance_mcf, na.rm = TRUE) * 1.037,
                  n_obs = n(),
                  .groups = "drop"
        )
      } else {
        summarize(.,
                  median_kwh             = median(building_kwh, na.rm = TRUE),
                  median_total_mmbtu     = median(prop_total_mmbtu, na.rm = TRUE),
                  median_heating_mmbtu   = median(prop_heating_mmbtu, na.rm = TRUE),
                  median_appliance_mmbtu = median(prop_appliance_mmbtu, na.rm = TRUE),
                  n_obs = n(),
                  .groups = "drop"
        )
      }
    }
}


# ── Load scenario files ─────────────────────────────────────────────────────

baseline <- load_resstock(
  "./data-raw/building_energy_data_processing/resstock data/MN_baseline_metadata_and_annual_results.csv"
)

heatpump <- load_resstock(
  "./data-raw/building_energy_data_processing/resstock data/MN_upgrade02_metadata_and_annual_results_heat_pump.csv"
)

envelope <- load_resstock(
  "./data-raw/building_energy_data_processing/resstock data/MN_upgrade2.04_metadata_and_annual_results_intermediate_envelope.csv"
)

combo <- load_resstock(
  "./data-raw/building_energy_data_processing/resstock data/MN_upgrade07_metadata_and_annual_results.csv"
)

sust_new_build <- load_resstock(
  "./data-raw/building_energy_data_processing/resstock data/MN_upgrade15_metadata_and_annual_results.csv"
) %>%
  filter(
    build_year == "2000+",
    building_nat_gas_kwh == 0
  ) # strategy isn't penetrating many multifamily units, need to investigate why but for now taking effective cases

# Propane baseline (for propane end-use decomposition)
baseline_propane <- load_resstock(
  "./data-raw/building_energy_data_processing/resstock data/MN_baseline_metadata_and_annual_results.csv",
  heating_fuel_filter = "Propane"
)


# ── Summary list ─────────────────────────────────────────────────────────────

resstock_summaries <- list(
  # ── Baseline summaries (original) ──────────────────────────────────────────
  mf_baseline = summarize_resstock(baseline, "Multi", c("mc_classification", "build_year")),
  manufactured_baseline = summarize_resstock(baseline, "Mobile", c("mc_classification", "build_year")),
  sf_attached_sqft_baseline = summarize_resstock(baseline, "Attached", c("mc_classification", "sqft_bin")),
  sf_attached_year_baseline = summarize_resstock(baseline, "Attached", c("mc_classification", "build_year")),
  sf_detached_sqft_baseline = summarize_resstock(baseline, "Detached", c("mc_classification", "sqft_bin")),
  sf_detached_year_baseline = summarize_resstock(baseline, "Detached", c("mc_classification", "build_year")),

  # ── Heat pump summaries ────────────────────────────────────────────────────
  mf_heatpump = summarize_resstock(heatpump, "Multi", c("mc_classification", "build_year")),
  manufactured_heatpump = summarize_resstock(heatpump, "Mobile", c("mc_classification", "build_year")),
  sf_attached_sqft_heatpump = summarize_resstock(heatpump, "Attached", c("mc_classification", "sqft_bin")),
  sf_attached_year_heatpump = summarize_resstock(heatpump, "Attached", c("mc_classification", "build_year")),
  sf_detached_sqft_heatpump = summarize_resstock(heatpump, "Detached", c("mc_classification", "sqft_bin")),
  sf_detached_year_heatpump = summarize_resstock(heatpump, "Detached", c("mc_classification", "build_year")),

  # ── Retrofit summaries ─────────────────────────────────────────────────────
  mf_envelope = summarize_resstock(envelope, "Multi", c("mc_classification", "build_year")),
  manufactured_envelope = summarize_resstock(envelope, "Mobile", c("mc_classification", "build_year")),
  sf_attached_sqft_envelope = summarize_resstock(envelope, "Attached", c("mc_classification", "sqft_bin")),
  sf_attached_year_envelope = summarize_resstock(envelope, "Attached", c("mc_classification", "build_year")),
  sf_detached_sqft_envelope = summarize_resstock(envelope, "Detached", c("mc_classification", "sqft_bin")),
  sf_detached_year_envelope = summarize_resstock(envelope, "Detached", c("mc_classification", "build_year")),

  # ── Combination summaries ──────────────────────────────────────────────────
  mf_combo = summarize_resstock(combo, "Multi", c("mc_classification", "build_year")),
  manufactured_combo = summarize_resstock(combo, "Mobile", c("mc_classification", "build_year")),
  sf_attached_sqft_combo = summarize_resstock(combo, "Attached", c("mc_classification", "sqft_bin")),
  sf_attached_year_combo = summarize_resstock(combo, "Attached", c("mc_classification", "build_year")),
  sf_detached_sqft_combo = summarize_resstock(combo, "Detached", c("mc_classification", "sqft_bin")),
  sf_detached_year_combo = summarize_resstock(combo, "Detached", c("mc_classification", "build_year")),

  # ── Sustainable new build summaries ────────────────────────────────────────
  mf_sust_new_build = summarize_resstock(sust_new_build, "Multi", c("mc_classification", "build_year")),
  manufactured_sust_new_build = summarize_resstock(sust_new_build, "Mobile", c("mc_classification", "build_year")),
  sf_attached_sqft_sust_new_build = summarize_resstock(sust_new_build, "Attached", c("mc_classification", "sqft_bin")),
  sf_attached_year_sust_new_build = summarize_resstock(sust_new_build, "Attached", c("mc_classification", "build_year")),
  sf_detached_sqft_sust_new_build = summarize_resstock(sust_new_build, "Detached", c("mc_classification", "sqft_bin")),
  sf_detached_year_sust_new_build = summarize_resstock(sust_new_build, "Detached", c("mc_classification", "build_year")),

  ### year x sq ft (used by CEEStock SFA smoothing)
  sf_attached_vintagesqft_baseline = summarize_resstock(
    baseline, "Attached", c("mc_classification", "build_year", "sqft_bin")
  ),
  sf_detached_vintagesqft_baseline = summarize_resstock(
    baseline, "Detached", c("mc_classification", "build_year", "sqft_bin")
  ),
  sf_attached_vintagesqft_heatpump = summarize_resstock(
    heatpump, "Attached", c("mc_classification", "build_year", "sqft_bin")
  ),
  sf_detached_vintagesqft_heatpump = summarize_resstock(
    heatpump, "Detached", c("mc_classification", "build_year", "sqft_bin")
  ),
  sf_attached_vintagesqft_envelope = summarize_resstock(
    envelope, "Attached", c("mc_classification", "build_year", "sqft_bin")
  ),
  sf_detached_vintagesqft_envelope = summarize_resstock(
    envelope, "Detached", c("mc_classification", "build_year", "sqft_bin")
  ),
  sf_attached_vintagesqft_combo = summarize_resstock(
    combo, "Attached", c("mc_classification", "build_year", "sqft_bin")
  ),
  sf_detached_vintagesqft_combo = summarize_resstock(
    combo, "Detached", c("mc_classification", "build_year", "sqft_bin")
  ),
  sf_attached_vintagesqft_sust_new_build = summarize_resstock(
    sust_new_build, "Attached", c("mc_classification", "build_year", "sqft_bin")
  ),
  sf_detached_vintagesqft_sust_new_build = summarize_resstock(
    sust_new_build, "Detached", c("mc_classification", "build_year", "sqft_bin")
  ),

  # ══════════════════════════════════════════════════════════════════════════
  # End-use decomposition (NEW)
  # ══════════════════════════════════════════════════════════════════════════
  #
  # Heating/appliance split for baseline profiles, used to construct
  # full-electrification profiles via CEEStock-derived ratios.
  # Includes median_heating_mmbtu and median_appliance_mmbtu for
  # fuel-agnostic ratio application.

  # Natural gas baselines with end-use split
  mf_baseline_enduse = summarize_enduse(
    baseline, "Multi", c("mc_classification", "build_year"), fuel = "natural_gas"
  ),
  manufactured_baseline_enduse = summarize_enduse(
    baseline, "Mobile", c("mc_classification", "build_year"), fuel = "natural_gas"
  ),
  sf_detached_baseline_enduse = summarize_enduse(
    baseline, "Detached", c("mc_classification", "build_year"), fuel = "natural_gas"
  ),
  sf_attached_baseline_enduse = summarize_enduse(
    baseline, "Attached", c("mc_classification", "build_year"), fuel = "natural_gas"
  ),

  # Propane baselines with end-use split
  # SFD n=999, manufactured n=113 — solid sample sizes
  # MF 5+ n=11 — marginal; MF 2-4 n=1 and SFA n=2 — unusable
  # For propane MF, use SFD propane ratios as proxy
  sf_detached_propane_enduse = summarize_enduse(
    baseline_propane, "Detached", c("mc_classification", "build_year"), fuel = "propane"
  ),
  manufactured_propane_enduse = summarize_enduse(
    baseline_propane, "Mobile", c("mc_classification", "build_year"), fuel = "propane"
  ),
  # MF propane: pooled across vintages due to small n (11 total for 5+)
  mf_propane_enduse = summarize_enduse(
    baseline_propane, "Multi", c("mc_classification"), fuel = "propane"
  )
)


# ── QC: print end-use summaries ──────────────────────────────────────────────

message("\n── Natural gas heating/appliance split (baseline) ──────────────")
for (key in c("mf_baseline_enduse", "manufactured_baseline_enduse",
              "sf_detached_baseline_enduse", "sf_attached_baseline_enduse")) {
  message("\n", key, ":")
  tbl <- resstock_summaries[[key]]
  tbl %>%
    mutate(
      heat_pct = round(median_heating_mmbtu /
                         (median_heating_mmbtu + median_appliance_mmbtu) * 100, 1),
      app_pct = round(median_appliance_mmbtu /
                        (median_heating_mmbtu + median_appliance_mmbtu) * 100, 1)
    ) %>%
    print(n = Inf)
}

message("\n── Propane heating/appliance split (baseline) ───────────────────")
for (key in c("sf_detached_propane_enduse", "manufactured_propane_enduse",
              "mf_propane_enduse")) {
  message("\n", key, ":")
  tbl <- resstock_summaries[[key]]
  tbl %>%
    mutate(
      heat_pct = round(median_heating_mmbtu /
                         (median_heating_mmbtu + median_appliance_mmbtu) * 100, 1),
      app_pct = round(median_appliance_mmbtu /
                        (median_heating_mmbtu + median_appliance_mmbtu) * 100, 1)
    ) %>%
    print(n = Inf)
}


usethis::use_data(resstock_summaries, overwrite = TRUE)
