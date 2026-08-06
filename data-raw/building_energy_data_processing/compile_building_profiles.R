### compile_building_summaries.R
###
### Runs AFTER compile_resstock_data.R and compile_ceestock_data.R.
### Produces `building_summaries`: a single named list where every entry
### is a profile table with identical columns:
###
###   mc_classification | build_year | sqft_bin | scenario_mwh | scenario_mcf
###
### For MF and manufactured housing, sqft_bin = "all" (no sqft dimension).
### For SF, sqft_bin uses CEEStock labels: detached has 7 bins, attached
### has 4 coarser bins. ResStock labels are relabeled to match.
###
### This uniform schema means calc_building_energy can join every
### scenario the same way — no separate SF-vs-MF code paths, no dual
### column names, no ResStock sqft relabeling at join time.
###
### Dependency chain:
###   compile_resstock_data.R  ->  resstock_summaries
###   compile_ceestock_data.R  ->  ceestock_summaries, electrification_ratios
###   this script              ->  building_summaries

library(dplyr)


# -- Standardization helpers --------------------------------------------------

#' Reshape a CEEStock SF profile to the standard schema.
#' CEEStock profiles have: scenario, build_year, sqft_bin, mc_classification,
#' elec_mwh, gas_mcf (and sometimes change columns we don't need here).
standardize_sf <- function(sf_profile) {
  sf_profile %>%
    transmute(
      mc_classification,
      build_year,
      sqft_bin,
      scenario_mwh = elec_mwh,
      scenario_mcf = gas_mcf
    )
}

#' Reshape a ResStock MF or manufactured profile to the standard schema.
#' ResStock profiles have: mc_classification, build_year, median_kwh, median_mcf.
#' sqft_bin is set to "all" since these profiles have no sqft dimension.
standardize_mf <- function(res_profile) {
  res_profile %>%
    transmute(
      mc_classification,
      build_year,
      sqft_bin = "all",
      scenario_mwh = median_kwh / 1000,
      scenario_mcf = median_mcf
    )
}

#' Combine SF + MF + manufactured into one unified profile table.
unify_profile <- function(sf, mf, manufactured) {
  bind_rows(
    standardize_sf(sf),
    standardize_mf(mf),
    standardize_mf(manufactured)
  )
}

#' Relabel ResStock sqft bins to CEEStock labels.
#' Used for sustainable-new-build SF profiles that come from ResStock.
relabel_resstock_sqft <- function(x) {
  case_match(
    x,
    "Less than 1,000"  ~ "<1000",
    "1,000 to 1,499"   ~ "1000 to 1499",
    "1,500 to 1,999"   ~ "1500 to 1999",
    "2,000 to 2,499"   ~ "2000 to 2499",
    "2,500 to 2,999"   ~ "2500 to 2999",
    "3,000 or more"    ~ "3000 to 3999",
    .default = x
  )
}


# -- Full-electrification profile builder -------------------------------------
#
# Takes a ResStock _enduse baseline summary and applies the three CEEStock-
# derived scalars to produce a full-electrification profile. Output matches
# the standard schema (with sqft_bin = "all").
#
# Optional retrofit_scenario: if provided, uses its post-retrofit total MCF
# to derive reduced heating demand. Appliance gas is unchanged by envelope work.

build_full_elec_profile <- function(baseline_enduse,
                                    ratios,
                                    retrofit_scenario = NULL) {

  if (!is.null(retrofit_scenario)) {
    profile <- baseline_enduse %>%
      inner_join(
        retrofit_scenario %>% select(build_year, retrofit_mcf = median_mcf),
        by = "build_year"
      ) %>%
      mutate(
        adj_heating_mcf     = pmax(retrofit_mcf - median_appliance_mcf, 0),
        adj_heating_mmbtu   = adj_heating_mcf * 1.037,
        adj_appliance_mmbtu = median_appliance_mmbtu
      )
  } else {
    profile <- baseline_enduse %>%
      mutate(
        adj_heating_mmbtu   = median_heating_mmbtu,
        adj_appliance_mmbtu = median_appliance_mmbtu
      )
  }

  profile %>%
    transmute(
      mc_classification,
      build_year,
      sqft_bin = "all",
      scenario_mcf = (adj_heating_mmbtu * ratios$heating_retention_frac) / 1.037,
      scenario_mwh = (median_kwh +
                        (adj_heating_mmbtu * (1 - ratios$heating_retention_frac) *
                           ratios$heating_mwh_per_mmbtu * 1000) +
                        (adj_appliance_mmbtu * ratios$appliance_mwh_per_mmbtu * 1000)
      ) / 1000
    )
}


# -- Propane variant -----------------------------------------------------------
# Same math, different input/output columns. Propane stays in mmBtu.

build_full_elec_profile_propane <- function(baseline_enduse, ratios) {
  baseline_enduse %>%
    transmute(
      mc_classification,
      build_year = if ("build_year" %in% names(cur_data())) build_year else "all",
      median_propane_mmbtu = median_heating_mmbtu * ratios$heating_retention_frac,
      median_kwh = median_kwh +
        (median_heating_mmbtu * (1 - ratios$heating_retention_frac) *
           ratios$heating_mwh_per_mmbtu * 1000) +
        (median_appliance_mmbtu * ratios$appliance_mwh_per_mmbtu * 1000)
    )
}


# ==============================================================================
# Assemble profiles
# ==============================================================================

# -- Standard scenarios --------------------------------------------------------

baseline <- unify_profile(
  ceestock_summaries$cee_baseline_sf,
  resstock_summaries$mf_baseline,
  resstock_summaries$manufactured_baseline
)

retrofit <- unify_profile(
  ceestock_summaries$cee_retrofit_sf,
  resstock_summaries$mf_envelope,
  resstock_summaries$manufactured_envelope
)

heatpump <- unify_profile(
  ceestock_summaries$cee_heatpump_sf,
  resstock_summaries$mf_heatpump,
  resstock_summaries$manufactured_heatpump
)

electric_appliances <- unify_profile(
  ceestock_summaries$cee_appliance_sf,
  # MF/manufactured: no separate appliance-only scenario in ResStock;
  # appliance electrification handled through full_electrification
  resstock_summaries$mf_baseline,
  resstock_summaries$manufactured_baseline
)

combination <- unify_profile(
  ceestock_summaries$cee_combined_sf,
  resstock_summaries$mf_combo,
  resstock_summaries$manufactured_combo
)

full_electrification <- bind_rows(
  standardize_sf(ceestock_summaries$cee_full_elec_sf),
  build_full_elec_profile(
    resstock_summaries$mf_baseline_enduse,
    electrification_ratios
  ),
  build_full_elec_profile(
    resstock_summaries$manufactured_baseline_enduse,
    electrification_ratios
  )
)

retrofit_full_electrification <- bind_rows(
  standardize_sf(ceestock_summaries$cee_retrofit_full_elec_sf),
  build_full_elec_profile(
    resstock_summaries$mf_baseline_enduse,
    electrification_ratios,
    retrofit_scenario = resstock_summaries$mf_envelope
  ),
  build_full_elec_profile(
    resstock_summaries$manufactured_baseline_enduse,
    electrification_ratios,
    retrofit_scenario = resstock_summaries$manufactured_envelope
  )
)

# -- New-build variants (2000+ vintage only) -----------------------------------

new_build <- baseline %>% filter(build_year == "2000+")
new_build_heatpump <- heatpump %>% filter(build_year == "2000+")
new_build_full_electrification <- full_electrification %>% filter(build_year == "2000+")

# Sustainable new build: ResStock upgrade15, all-electric, 2000+ only.
# SF sqft bins relabeled from ResStock to CEEStock convention.
sust_new_build_sf <- bind_rows(
  resstock_summaries$sf_attached_vintagesqft_sust_new_build,
  resstock_summaries$sf_detached_vintagesqft_sust_new_build
) %>%
  transmute(
    mc_classification,
    build_year,
    sqft_bin = relabel_resstock_sqft(as.character(sqft_bin)),
    scenario_mwh = median_kwh / 1000,
    scenario_mcf = median_mcf
  )

new_build_leed <- bind_rows(
  sust_new_build_sf,
  standardize_mf(resstock_summaries$mf_sust_new_build),
  standardize_mf(resstock_summaries$manufactured_sust_new_build)
)


# ==============================================================================
# Export
# ==============================================================================

building_summaries <- list(
  baseline                       = baseline,
  retrofit                       = retrofit,
  heatpump                       = heatpump,
  electric_appliances            = electric_appliances,
  combination                    = combination,
  full_electrification           = full_electrification,
  retrofit_full_electrification  = retrofit_full_electrification,
  new_build                      = new_build,
  new_build_heatpump             = new_build_heatpump,
  new_build_full_electrification = new_build_full_electrification,
  new_build_leed                 = new_build_leed,

  # Propane (separate schema: median_propane_mmbtu + median_kwh)
  propane_sfd_full_elec = build_full_elec_profile_propane(
    resstock_summaries$sf_detached_propane_enduse, electrification_ratios
  ),
  propane_manufactured_full_elec = build_full_elec_profile_propane(
    resstock_summaries$manufactured_propane_enduse, electrification_ratios
  ),
  propane_mf_full_elec = build_full_elec_profile_propane(
    resstock_summaries$mf_propane_enduse, electrification_ratios
  ),

  # Ratios (single-row tibble, for documentation)
  electrification_ratios = electrification_ratios
)


# -- QC -----------------------------------------------------------------------

message("\n-- Profile schema check ---")
standard_cols <- c("mc_classification", "build_year", "sqft_bin",
                   "scenario_mwh", "scenario_mcf")
for (key in c("baseline", "retrofit", "heatpump", "full_electrification",
              "combination", "new_build", "new_build_leed")) {
  tbl <- building_summaries[[key]]
  has_cols <- all(standard_cols %in% names(tbl))
  n_mc <- n_distinct(tbl$mc_classification)
  message(sprintf("  %-35s  cols_ok=%s  n_mc=%d  rows=%d",
                  key, has_cols, n_mc, nrow(tbl)))
}

message("\n-- Full electrification vs baseline ---")
for (mc in c("multifamily_units", "manufactured_homes")) {
  bl <- building_summaries$baseline %>% filter(mc_classification == mc)
  fe <- building_summaries$full_electrification %>% filter(mc_classification == mc)
  comp <- inner_join(bl, fe, by = c("mc_classification", "build_year", "sqft_bin"),
                     suffix = c("_bl", "_fe")) %>%
    mutate(
      mcf_reduction = paste0(round((1 - scenario_mcf_fe / scenario_mcf_bl) * 100), "%"),
      mwh_increase  = paste0(round((scenario_mwh_fe / scenario_mwh_bl - 1) * 100), "%")
    )
  message(sprintf("\n  %s:", mc))
  print(select(comp, build_year, scenario_mcf_bl, scenario_mcf_fe, mcf_reduction,
               scenario_mwh_bl, scenario_mwh_fe, mwh_increase), n = Inf)
}


usethis::use_data(building_summaries, overwrite = TRUE)
