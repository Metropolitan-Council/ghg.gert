### compile_building_summaries.R
###
### Runs AFTER compile_resstock_data.R and compile_ceestock_data.R.
### Produces `building_summaries`: one named list, one schema per entry:
###
###   mc_classification | build_year | sqft_bin | scenario_mwh | scenario_mcf
###
### Data sources by housing type:
###   SFD  — CEEStock directly (ground-truth MN data)
###   SFA  — ResStock (metro-filtered) x CEEStock-derived correction scalar
###   MF   — ResStock for standard scenarios; electrification ratios for full elec
###   Mfg  — same as MF
###
### SFA approach: ResStock has full vintage x sqft coverage and natural
### monotonicity with size. CEEStock has sparse SFA observations but serves
### as MN ground truth. We compute a correction scalar (CEEStock / ResStock)
### at overlap points, split pre-1940 vs post-1940 where the bias differs,
### and multiply all ResStock SFA profiles by that scalar. This preserves
### ResStock's monotonic structure while matching MN reality.
###
### For sqft_bin: MF and manufactured = "all". SFD uses 7 fine bins.
### SFA uses 4 coarse bins (CEEStock convention). ResStock sqft labels
### are relabeled and collapsed to match.

library(dplyr)


# ==============================================================================
# Helpers
# ==============================================================================

# -- Column standardization ----------------------------------------------------

standardize_sf <- function(sf_profile) {
  sf_profile %>%
    transmute(mc_classification, build_year, sqft_bin,
              scenario_mwh = elec_mwh, scenario_mcf = gas_mcf)
}

standardize_mf <- function(res_profile) {
  res_profile %>%
    transmute(mc_classification, build_year, sqft_bin = "all",
              scenario_mwh = median_kwh / 1000, scenario_mcf = median_mcf)
}

# -- Sqft bin relabeling -------------------------------------------------------

#' Relabel ResStock sqft bins to SFD CEEStock labels (7 fine bins).
#' "3,000 or more" maps to "3000 to 3999" (no 4000+ split in ResStock).
relabel_resstock_to_sfd <- function(x) {
  case_match(as.character(x),
             "Less than 1,000"  ~ "<1000",
             "1,000 to 1,499"   ~ "1000 to 1499",
             "1,500 to 1,999"   ~ "1500 to 1999",
             "2,000 to 2,499"   ~ "2000 to 2499",
             "2,500 to 2,999"   ~ "2500 to 2999",
             "3,000 or more"    ~ "3000 to 3999",
             .default = as.character(x)
  )
}

#' Relabel ResStock sqft bins to SFA CEEStock labels (4 coarse bins).
#' Multiple ResStock bins collapse into each coarse bin.
relabel_resstock_to_sfa <- function(x) {
  case_match(as.character(x),
             "Less than 1,000"                          ~ "<1000",
             c("1,000 to 1,499", "1,500 to 1,999")     ~ "1000 to 1999",
             c("2,000 to 2,499", "2,500 to 2,999")     ~ "2000 to 2999",
             "3,000 or more"                            ~ "3000+",
             .default = as.character(x)
  )
}


# -- Monotonicity enforcement --------------------------------------------------
#
# Ensures scenario_mwh and scenario_mcf are non-decreasing with dwelling size
# within each mc_classification x build_year group. Only affects SF rows
# (sqft_bin != "all"). Applied after all profiles are assembled.

enforce_monotonic <- function(profile) {
  sqft_order <- list(
    single_family_detached = c(
      "<1000", "1000 to 1499", "1500 to 1999", "2000 to 2499",
      "2500 to 2999", "3000 to 3999", "4000+"
    ),
    single_family_attached = c("<1000", "1000 to 1999", "2000 to 2999", "3000+")
  )

  non_sf <- profile %>% filter(sqft_bin == "all")

  sf <- profile %>%
    filter(sqft_bin != "all") %>%
    group_by(mc_classification, build_year) %>%
    arrange(match(sqft_bin, sqft_order[[mc_classification[1]]]), .by_group = TRUE) %>%
    mutate(
      scenario_mwh = cummax(scenario_mwh),
      scenario_mcf = cummax(scenario_mcf)
    ) %>%
    ungroup()

  bind_rows(sf, non_sf)
}


# ==============================================================================
# SFA correction scalars
# ==============================================================================
#
# Compare CEEStock observed SFA baseline values against metro-filtered
# ResStock SFA at the same vintage x coarse sqft bins. The scalar corrects
# for ResStock's bias relative to MN ground truth.
#
# Pre-1940 and post-1940 are separated because:
#   - Pre-1940: ResStock overshoots both fuels (older urban core well-represented
#     in the national model but over-predicted for MN)
#   - Post-1940: ResStock undershoots gas ~34% (underestimates MN heating demand)
#     and gets electricity roughly right

compute_sfa_scalars <- function(cee_baseline_sf, rs_sfa_vintagesqft) {
  # CEEStock observed SFA means at coarse bins
  cee_obs <- cee_baseline_sf %>%
    filter(mc_classification == "single_family_attached", observed) %>%
    transmute(build_year, sqft_bin,
              cee_mwh = elec_mwh, cee_mcf = gas_mcf, cee_n = 1)
  # Note: cee_n=1 per row since these are already group means. For weighting
  # we could track the original n, but with 11 overlap points it's fine.

  # ResStock SFA collapsed to coarse bins
  rs_sfa <- rs_sfa_vintagesqft %>%
    mutate(sqft_bin = relabel_resstock_to_sfa(sqft_bin)) %>%
    group_by(build_year, sqft_bin) %>%
    summarise(
      rs_mwh = median(median_kwh, na.rm = TRUE) / 1000,
      rs_mcf = median(median_mcf, na.rm = TRUE),
      .groups = "drop"
    )

  paired <- inner_join(cee_obs, rs_sfa, by = c("build_year", "sqft_bin")) %>%
    mutate(
      vintage_group = if_else(build_year == "<1940", "pre1940", "post1940"),
      scalar_mwh = cee_mwh / rs_mwh,
      scalar_mcf = cee_mcf / rs_mcf
    )

  message("\n-- SFA scalar overlap points --")
  print(select(paired, build_year, sqft_bin, cee_mwh, rs_mwh, scalar_mwh,
               cee_mcf, rs_mcf, scalar_mcf), n = Inf)

  scalars <- paired %>%
    group_by(vintage_group) %>%
    summarise(
      scalar_mwh = mean(scalar_mwh),
      scalar_mcf = mean(scalar_mcf),
      n_overlap  = n(),
      .groups = "drop"
    )

  message("\n-- SFA scalars by vintage group --")
  print(scalars, n = Inf)

  scalars
}


# ==============================================================================
# SFA profile builders
# ==============================================================================

#' Build an SFA profile from a ResStock vintagesqft scenario summary,
#' applying CEEStock correction scalars.
#' Relabels sqft bins to coarse SFA convention, collapses, and scales.
build_sfa_profile <- function(rs_vintagesqft, scalars) {
  rs_vintagesqft %>%
    mutate(
      sqft_bin = relabel_resstock_to_sfa(sqft_bin),
      vintage_group = if_else(build_year == "<1940", "pre1940", "post1940")
    ) %>%
    group_by(mc_classification, build_year, sqft_bin, vintage_group) %>%
    summarise(
      scenario_mwh = median(median_kwh, na.rm = TRUE) / 1000,
      scenario_mcf = median(median_mcf, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(scalars, by = "vintage_group") %>%
    transmute(
      mc_classification, build_year, sqft_bin,
      scenario_mwh = scenario_mwh * scalar_mwh,
      scenario_mcf = scenario_mcf * scalar_mcf
    )
}


#' Build SFA full-electrification profile.
#' Uses ResStock SFA baseline (scalar-corrected) decomposed into
#' heating/appliance via the vintage-level end-use proportions,
#' then applies electrification ratios.
build_sfa_full_elec <- function(rs_sfa_vintagesqft_baseline,
                                sfa_enduse,
                                scalars,
                                ratios) {
  # Heating fraction per vintage from end-use summary
  heat_frac <- sfa_enduse %>%
    transmute(
      build_year,
      heat_frac = median_heating_mcf / (median_heating_mcf + median_appliance_mcf)
    )

  # Scalar-corrected baseline at vintage x sqft
  rs_sfa_vintagesqft_baseline %>%
    mutate(
      sqft_bin = relabel_resstock_to_sfa(sqft_bin),
      vintage_group = if_else(build_year == "<1940", "pre1940", "post1940")
    ) %>%
    group_by(mc_classification, build_year, sqft_bin, vintage_group) %>%
    summarise(
      total_kwh = median(median_kwh, na.rm = TRUE),
      total_mcf = median(median_mcf, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(scalars, by = "vintage_group") %>%
    mutate(
      total_kwh = total_kwh * scalar_mwh,
      total_mcf = total_mcf * scalar_mcf
    ) %>%
    # Decompose using vintage-level heating fraction
    left_join(heat_frac, by = "build_year") %>%
    mutate(
      heating_mmbtu   = total_mcf * heat_frac * 1.037,
      appliance_mmbtu = total_mcf * (1 - heat_frac) * 1.037
    ) %>%
    # Apply electrification ratios
    transmute(
      mc_classification, build_year, sqft_bin,
      scenario_mcf = (heating_mmbtu * ratios$heating_retention_frac) / 1.037,
      scenario_mwh = (total_kwh +
                        (heating_mmbtu * (1 - ratios$heating_retention_frac) *
                           ratios$heating_mwh_per_mmbtu * 1000) +
                        (appliance_mmbtu * ratios$appliance_mwh_per_mmbtu * 1000)
      ) / 1000
    )
}


# ==============================================================================
# MF / manufactured full-electrification builder
# ==============================================================================

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
      mc_classification, build_year, sqft_bin = "all",
      scenario_mcf = (adj_heating_mmbtu * ratios$heating_retention_frac) / 1.037,
      scenario_mwh = (median_kwh +
                        (adj_heating_mmbtu * (1 - ratios$heating_retention_frac) *
                           ratios$heating_mwh_per_mmbtu * 1000) +
                        (adj_appliance_mmbtu * ratios$appliance_mwh_per_mmbtu * 1000)
      ) / 1000
    )
}

# Propane variant
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

# Compute SFA correction scalars from baseline overlap
sfa_scalars <- compute_sfa_scalars(
  ceestock_summaries$cee_baseline_sf,
  resstock_summaries$sf_attached_vintagesqft_baseline
)

# -- Helper: combine SFD (CEEStock) + SFA (ResStock x scalar) + MF + Mfg ------
build_unified <- function(sfd_cee, sfa_rs_key, mf_rs, manuf_rs) {
  sfa <- build_sfa_profile(
    resstock_summaries[[paste0("sf_attached_vintagesqft_", sfa_rs_key)]],
    sfa_scalars
  )
  bind_rows(
    standardize_sf(sfd_cee),
    sfa,
    standardize_mf(mf_rs),
    standardize_mf(manuf_rs)
  )
}

# -- Standard scenarios --------------------------------------------------------

baseline <- build_unified(
  ceestock_summaries$cee_baseline_sf, "baseline",
  resstock_summaries$mf_baseline, resstock_summaries$manufactured_baseline
)

retrofit <- build_unified(
  ceestock_summaries$cee_retrofit_sf, "envelope",
  resstock_summaries$mf_envelope, resstock_summaries$manufactured_envelope
)

heatpump <- build_unified(
  ceestock_summaries$cee_heatpump_sf, "heatpump",
  resstock_summaries$mf_heatpump, resstock_summaries$manufactured_heatpump
)

electric_appliances <- build_unified(
  ceestock_summaries$cee_appliance_sf, "baseline",
  # MF/Mfg: no separate appliance-only scenario; placeholder baseline
  resstock_summaries$mf_baseline, resstock_summaries$manufactured_baseline
)

combination <- build_unified(
  ceestock_summaries$cee_combined_sf, "combo",
  resstock_summaries$mf_combo, resstock_summaries$manufactured_combo
)

# -- Full electrification ------------------------------------------------------
# SFD: from CEEStock directly
# SFA: from ResStock baseline x scalar, decomposed by end-use, ratio-applied
# MF/Mfg: from ResStock baseline end-use, ratio-applied

full_electrification <- bind_rows(
  standardize_sf(ceestock_summaries$cee_full_elec_sf),
  build_sfa_full_elec(
    resstock_summaries$sf_attached_vintagesqft_baseline,
    resstock_summaries$sf_attached_baseline_enduse,
    sfa_scalars,
    electrification_ratios
  ),
  build_full_elec_profile(
    resstock_summaries$mf_baseline_enduse, electrification_ratios
  ),
  build_full_elec_profile(
    resstock_summaries$manufactured_baseline_enduse, electrification_ratios
  )
)

# Retrofit + full electrification
retrofit_full_electrification <- bind_rows(
  standardize_sf(ceestock_summaries$cee_retrofit_full_elec_sf),
  # SFA: use envelope scenario totals to derive reduced heating,
  # then apply full electrification ratios
  {
    sfa_env <- build_sfa_profile(
      resstock_summaries$sf_attached_vintagesqft_envelope, sfa_scalars
    )
    sfa_bl <- build_sfa_profile(
      resstock_summaries$sf_attached_vintagesqft_baseline, sfa_scalars
    )
    heat_frac <- resstock_summaries$sf_attached_baseline_enduse %>%
      transmute(build_year,
                heat_frac = median_heating_mcf / (median_heating_mcf + median_appliance_mcf))

    sfa_env %>%
      # Envelope changes heating, not appliances. Use baseline appliance MCF.
      inner_join(
        sfa_bl %>% select(build_year, sqft_bin, bl_mcf = scenario_mcf),
        by = c("build_year", "sqft_bin")
      ) %>%
      left_join(heat_frac, by = "build_year") %>%
      mutate(
        bl_appliance_mcf = bl_mcf * (1 - heat_frac),
        adj_heating_mcf  = pmax(scenario_mcf - bl_appliance_mcf, 0),
        heating_mmbtu    = adj_heating_mcf * 1.037,
        appliance_mmbtu  = bl_appliance_mcf * 1.037
      ) %>%
      transmute(
        mc_classification, build_year, sqft_bin,
        scenario_mcf = (heating_mmbtu * electrification_ratios$heating_retention_frac) / 1.037,
        scenario_mwh = (scenario_mwh * 1000 +  # start from envelope electricity (already kWh-scaled)
                          (heating_mmbtu * (1 - electrification_ratios$heating_retention_frac) *
                             electrification_ratios$heating_mwh_per_mmbtu * 1000) +
                          (appliance_mmbtu * electrification_ratios$appliance_mwh_per_mmbtu * 1000)
        ) / 1000
      )
  },
  build_full_elec_profile(
    resstock_summaries$mf_baseline_enduse, electrification_ratios,
    retrofit_scenario = resstock_summaries$mf_envelope
  ),
  build_full_elec_profile(
    resstock_summaries$manufactured_baseline_enduse, electrification_ratios,
    retrofit_scenario = resstock_summaries$manufactured_envelope
  )
)

# -- New-build variants (2000+ only) -------------------------------------------

new_build <- baseline %>% filter(build_year == "2000+")
new_build_heatpump <- heatpump %>% filter(build_year == "2000+")
new_build_full_electrification <- full_electrification %>% filter(build_year == "2000+")

# Sustainable new build (ResStock upgrade15, all-electric)
sust_sf <- bind_rows(
  resstock_summaries$sf_attached_vintagesqft_sust_new_build %>%
    mutate(sqft_bin = relabel_resstock_to_sfa(as.character(sqft_bin))) %>%
    group_by(mc_classification, build_year, sqft_bin) %>%
    summarise(scenario_mwh = median(median_kwh) / 1000,
              scenario_mcf = median(median_mcf), .groups = "drop"),
  resstock_summaries$sf_detached_vintagesqft_sust_new_build %>%
    transmute(mc_classification, build_year,
              sqft_bin = relabel_resstock_to_sfd(as.character(sqft_bin)),
              scenario_mwh = median_kwh / 1000, scenario_mcf = median_mcf)
)

new_build_leed <- bind_rows(
  sust_sf,
  standardize_mf(resstock_summaries$mf_sust_new_build),
  standardize_mf(resstock_summaries$manufactured_sust_new_build)
)


# ==============================================================================
# Enforce monotonicity and export
# ==============================================================================

building_summaries <- list(
  baseline                       = enforce_monotonic(baseline),
  retrofit                       = enforce_monotonic(retrofit),
  heatpump                       = enforce_monotonic(heatpump),
  electric_appliances            = enforce_monotonic(electric_appliances),
  combination                    = enforce_monotonic(combination),
  full_electrification           = enforce_monotonic(full_electrification),
  retrofit_full_electrification  = enforce_monotonic(retrofit_full_electrification),
  new_build                      = enforce_monotonic(new_build),
  new_build_heatpump             = enforce_monotonic(new_build_heatpump),
  new_build_full_electrification = enforce_monotonic(new_build_full_electrification),
  new_build_leed                 = enforce_monotonic(new_build_leed),

  # Propane full-electrification (separate schema)
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
  electrification_ratios = electrification_ratios,

  # SFA scalars (for documentation)
  sfa_scalars = sfa_scalars
)


# -- QC -----------------------------------------------------------------------

message("\n-- Profile schema check --")
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

message("\n-- Full electrification vs baseline --")
for (mc in c("single_family_attached", "multifamily_units", "manufactured_homes")) {
  bl <- building_summaries$baseline %>% filter(mc_classification == mc)
  fe <- building_summaries$full_electrification %>% filter(mc_classification == mc)
  comp <- inner_join(bl, fe, by = c("mc_classification", "build_year", "sqft_bin"),
                     suffix = c("_bl", "_fe"))
  if (nrow(comp) == 0) next
  comp <- comp %>%
    mutate(
      mcf_reduction = paste0(round((1 - scenario_mcf_fe / scenario_mcf_bl) * 100), "%"),
      mwh_increase  = paste0(round((scenario_mwh_fe / scenario_mwh_bl - 1) * 100), "%")
    )
  message(sprintf("\n  %s:", mc))
  print(select(comp, build_year, sqft_bin, scenario_mcf_bl, scenario_mcf_fe,
               mcf_reduction, scenario_mwh_bl, scenario_mwh_fe, mwh_increase), n = Inf)
}

message("\n-- SFA monotonicity spot check --")
sfa_bl <- building_summaries$baseline %>%
  filter(mc_classification == "single_family_attached") %>%
  arrange(build_year, match(sqft_bin, c("<1000", "1000 to 1999", "2000 to 2999", "3000+")))
print(sfa_bl, n = Inf)


usethis::use_data(building_summaries, overwrite = TRUE)
