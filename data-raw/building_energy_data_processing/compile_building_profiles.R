### compile_building_summaries.R
###
### Consolidates all building energy profiles into `building_summaries`.
### Every entry has the same schema:
###
###   mc_classification | build_year | sqft_bin | scenario_mwh | scenario_mcf
###
### Six scenarios:
###   baseline              — no interventions
###   retrofit              — envelope upgrade only
###   full_electrification  — heat pump + appliance electrification, no envelope
###   combination           — envelope + full electrification
###   new_build             — baseline filtered to 2000+ vintage
###   new_build_sustainable — high-performance new construction
###
### Approach:
###   SFD  — CEEStock directly (MN ground truth)
###   ALL OTHER TYPES — ResStock x vintage-level SFD correction scalar
###
### The scalar is computed per vintage by comparing CEEStock SFD against
### ResStock SFD, then averaged across sqft bins. This captures the
### vintage-dependent bias in ResStock's national model for MN (e.g.
### pre-1940 housing overshooted, post-war undershoots gas).
###
### A uniform per-vintage scalar guarantees SFA < SFD by construction:
### ResStock already maintains that ordering internally, and multiplying
### all types by the same constant preserves it.
###
### For scenarios that include an envelope retrofit, non-SFD types receive
### a per-vintage "retrofit boost" that calibrates ResStock's envelope
### effect to the magnitude observed in CEEStock SFD. This preserves the
### relative ordering across housing types while ensuring the overall
### retrofit intensity is anchored to MN ground truth. Separate boost
### factors are computed for each envelope context (baseline vs fully
### electrified) because the same physical envelope improvement yields
### different relative savings depending on the HVAC system.
###
### Dependency chain:
###   compile_resstock_data.R  ->  resstock_summaries
###   compile_ceestock_data.R  ->  ceestock_summaries, electrification_ratios
###   this script              ->  building_summaries

library(dplyr)


# ==============================================================================
# Helpers
# ==============================================================================

standardize_sfd <- function(sfd_profile) {
  sfd_profile %>%
    transmute(mc_classification, build_year, sqft_bin,
              scenario_mwh = elec_mwh, scenario_mcf = gas_mcf)
}

#' Relabel ResStock sqft bins to CEEStock convention.
relabel_resstock_sqft <- function(x) {
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

#' Enforce monotonic increase with dwelling size.
enforce_monotonic <- function(profile) {
  sqft_order <- c(
    "<1000", "1000 to 1499", "1500 to 1999", "2000 to 2499",
    "2500 to 2999", "3000 to 3999", "4000+"
  )
  non_sf <- profile %>% filter(sqft_bin == "all")
  sf <- profile %>%
    filter(sqft_bin != "all") %>%
    group_by(mc_classification, build_year) %>%
    arrange(match(sqft_bin, sqft_order), .by_group = TRUE) %>%
    mutate(
      scenario_mwh = cummax(scenario_mwh),
      scenario_mcf = cummax(scenario_mcf)
    ) %>%
    ungroup()
  bind_rows(sf, non_sf)
}

#' Cap SFA at SFD values at the same vintage and sqft.
#' Attached homes with shared walls cannot use more energy than
#' detached homes. Applied after enforce_monotonic to ensure the
#' harder physical constraint is the final pass.
cap_sfa_at_sfd <- function(profile) {
  sfd <- profile %>%
    filter(mc_classification == "single_family_detached") %>%
    select(build_year, sqft_bin, sfd_mwh = scenario_mwh, sfd_mcf = scenario_mcf)

  sfa <- profile %>%
    filter(mc_classification == "single_family_attached") %>%
    left_join(sfd, by = c("build_year", "sqft_bin")) %>%
    mutate(
      scenario_mwh = pmin(scenario_mwh, coalesce(sfd_mwh, Inf)),
      scenario_mcf = pmin(scenario_mcf, coalesce(sfd_mcf, Inf))
    ) %>%
    select(-sfd_mwh, -sfd_mcf)

  profile %>%
    filter(mc_classification != "single_family_attached") %>%
    bind_rows(sfa)
}


# ==============================================================================
# Vintage-level SFD correction scalar
# ==============================================================================
#
# Computed at vintage x sqft for transparency, then averaged to vintage-only
# for application. The sqft-level breakdown is printed for QC but not used
# downstream — the vintage mean washes out noise from thin sqft cells.

compute_vintage_scalar <- function(cee_baseline_sfd, rs_sfd_vintagesqft) {
  cee <- cee_baseline_sfd %>%
    transmute(build_year, sqft_bin,
              cee_mwh = elec_mwh, cee_mcf = gas_mcf)

  rs <- rs_sfd_vintagesqft %>%
    transmute(build_year,
              sqft_bin = relabel_resstock_sqft(sqft_bin),
              rs_mwh = median_kwh / 1000, rs_mcf = median_mcf) %>%
    group_by(build_year, sqft_bin) %>%
    summarise(rs_mwh = median(rs_mwh), rs_mcf = median(rs_mcf), .groups = "drop")

  by_sqft <- inner_join(cee, rs, by = c("build_year", "sqft_bin")) %>%
    mutate(scalar_mwh = cee_mwh / rs_mwh, scalar_mcf = cee_mcf / rs_mcf)

  message("\n-- SFD scalar by vintage x sqft (QC only) --")
  print(by_sqft %>% arrange(build_year, sqft_bin) %>%
          select(build_year, sqft_bin, scalar_mwh, scalar_mcf), n = Inf)

  # Average to vintage-only
  scalars <- by_sqft %>%
    group_by(build_year) %>%
    summarise(
      scalar_mwh = mean(scalar_mwh),
      scalar_mcf = mean(scalar_mcf),
      n_sqft_bins = n(),
      .groups = "drop"
    )

  message("\n-- Vintage-level scalars (applied to all ResStock types) --")
  print(scalars, n = Inf)

  scalars
}


# ==============================================================================
# Profile builders
# ==============================================================================

#' Apply vintage scalar to a ResStock vintagesqft summary (for SF types).
#' Relabels sqft bins and multiplies by the vintage-level scalar.
apply_scalar_vintagesqft <- function(rs_vintagesqft, scalars) {
  rs_vintagesqft %>%
    mutate(sqft_bin = relabel_resstock_sqft(sqft_bin)) %>%
    group_by(mc_classification, build_year, sqft_bin) %>%
    summarise(
      scenario_mwh = median(median_kwh, na.rm = TRUE) / 1000,
      scenario_mcf = median(median_mcf, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    inner_join(scalars, by = "build_year") %>%
    transmute(mc_classification, build_year, sqft_bin,
              scenario_mwh = scenario_mwh * scalar_mwh,
              scenario_mcf = scenario_mcf * scalar_mcf)
}

#' Apply vintage scalar to a ResStock vintage-only summary (MF, manufactured).
apply_scalar_vintage <- function(rs_summary, scalars) {
  rs_summary %>%
    transmute(mc_classification, build_year, sqft_bin = "all",
              scenario_mwh = median_kwh / 1000,
              scenario_mcf = median_mcf) %>%
    inner_join(scalars, by = "build_year") %>%
    transmute(mc_classification, build_year, sqft_bin,
              scenario_mwh = scenario_mwh * scalar_mwh,
              scenario_mcf = scenario_mcf * scalar_mcf)
}

#' Build full-electrification profile from scalar-corrected baseline.
#' Works for SF (with sqft) and MF/manufactured (vintage only).
#' Decomposes total MCF into heating/appliance using vintage-level end-use
#' proportions, then applies electrification ratios.
build_full_elec <- function(rs_baseline, enduse, scalars, ratios,
                            has_sqft = FALSE,
                            retrofit_baseline = NULL) {
  # Heating fraction per vintage
  heat_frac <- enduse %>%
    transmute(build_year,
              heat_frac = median_heating_mcf / (median_heating_mcf + median_appliance_mcf))

  if (has_sqft) {
    base <- rs_baseline %>%
      mutate(sqft_bin = relabel_resstock_sqft(sqft_bin)) %>%
      group_by(mc_classification, build_year, sqft_bin) %>%
      summarise(raw_kwh = median(median_kwh, na.rm = TRUE),
                raw_mcf = median(median_mcf, na.rm = TRUE), .groups = "drop")
  } else {
    base <- rs_baseline %>%
      transmute(mc_classification, build_year, sqft_bin = "all",
                raw_kwh = median_kwh, raw_mcf = median_mcf)
  }

  # Apply scalar
  base <- base %>%
    inner_join(scalars, by = "build_year") %>%
    mutate(total_kwh = raw_kwh * scalar_mwh,
           total_mcf = raw_mcf * scalar_mcf)

  # If retrofit provided, swap in its scalar-corrected values
  if (!is.null(retrofit_baseline)) {
    if (has_sqft) {
      ret <- retrofit_baseline %>%
        mutate(sqft_bin = relabel_resstock_sqft(sqft_bin)) %>%
        group_by(mc_classification, build_year, sqft_bin) %>%
        summarise(ret_kwh = median(median_kwh, na.rm = TRUE),
                  ret_mcf = median(median_mcf, na.rm = TRUE), .groups = "drop")
    } else {
      ret <- retrofit_baseline %>%
        transmute(mc_classification, build_year,
                  ret_kwh = median_kwh, ret_mcf = median_mcf)
    }
    join_by <- if (has_sqft) c("mc_classification", "build_year", "sqft_bin") else c("mc_classification", "build_year")
    base <- base %>%
      inner_join(ret, by = join_by) %>%
      mutate(total_kwh = ret_kwh * scalar_mwh,
             total_mcf = ret_mcf * scalar_mcf)
  }

  base %>%
    left_join(heat_frac, by = "build_year") %>%
    mutate(
      heating_mmbtu   = total_mcf * heat_frac * 1.037,
      appliance_mmbtu = total_mcf * (1 - heat_frac) * 1.037
    ) %>%
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

# Propane full electrification (separate schema, no scalar correction)
build_full_elec_propane <- function(enduse, ratios) {
  enduse %>%
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

#' Build SFD + SFA + MF + manufactured for a non-envelope scenario.
build_unified <- function(cee_sfd_key, rs_sfa_key, rs_mf, rs_manuf) {
  bind_rows(
    standardize_sfd(ceestock_summaries[[cee_sfd_key]]),
    apply_scalar_vintagesqft(
      resstock_summaries[[paste0("sf_attached_vintagesqft_", rs_sfa_key)]],
      vintage_scalars
    ),
    apply_scalar_vintage(rs_mf, vintage_scalars),
    apply_scalar_vintage(rs_manuf, vintage_scalars)
  )
}


# ==============================================================================
# Compute vintage scalar
# ==============================================================================

vintage_scalars <- compute_vintage_scalar(
  ceestock_summaries$cee_baseline_sfd,
  resstock_summaries$sf_detached_vintagesqft_baseline
)


# ==============================================================================
# Retrofit boost: calibrate ResStock envelope effect to CEEStock
# ==============================================================================
#
# Per vintage, compute how much stronger CEEStock's envelope effect is
# relative to ResStock's for SFD, then use that ratio to boost the
# ResStock-derived envelope effect for non-SFD types.
#
# Two boost factor sets: one for the envelope effect in a baseline
# (gas furnace) context, one for the envelope effect in a fully
# electrified context.

compute_retrofit_boost <- function(cee_baseline, cee_retrofit,
                                   rs_baseline_vintagesqft, rs_retrofit_vintagesqft,
                                   scalars) {
  # CEEStock SFD: average retention fraction per vintage
  cee_bl <- standardize_sfd(cee_baseline) %>%
    group_by(build_year) %>%
    summarise(cee_bl_mwh = mean(scenario_mwh), cee_bl_mcf = mean(scenario_mcf),
              .groups = "drop")
  cee_ret <- standardize_sfd(cee_retrofit) %>%
    group_by(build_year) %>%
    summarise(cee_ret_mwh = mean(scenario_mwh), cee_ret_mcf = mean(scenario_mcf),
              .groups = "drop")

  # ResStock SFD (scalar-corrected): average retention fraction per vintage
  rs_bl <- apply_scalar_vintagesqft(rs_baseline_vintagesqft, scalars) %>%
    group_by(build_year) %>%
    summarise(rs_bl_mwh = mean(scenario_mwh), rs_bl_mcf = mean(scenario_mcf),
              .groups = "drop")
  rs_ret <- apply_scalar_vintagesqft(rs_retrofit_vintagesqft, scalars) %>%
    group_by(build_year) %>%
    summarise(rs_ret_mwh = mean(scenario_mwh), rs_ret_mcf = mean(scenario_mcf),
              .groups = "drop")

  boost <- cee_bl %>%
    inner_join(cee_ret, by = "build_year") %>%
    inner_join(rs_bl, by = "build_year") %>%
    inner_join(rs_ret, by = "build_year") %>%
    mutate(
      # Reduction fractions (1 - retention)
      cee_red_mwh = 1 - cee_ret_mwh / cee_bl_mwh,
      cee_red_mcf = 1 - cee_ret_mcf / cee_bl_mcf,
      rs_red_mwh  = 1 - rs_ret_mwh / rs_bl_mwh,
      rs_red_mcf  = 1 - rs_ret_mcf / rs_bl_mcf,
      # Boost = how many times stronger CEEStock's effect is
      boost_mwh = cee_red_mwh / rs_red_mwh,
      boost_mcf = cee_red_mcf / rs_red_mcf
    )

  message("\n-- Retrofit boost factors by vintage --")
  print(boost %>% select(build_year, cee_red_mwh, rs_red_mwh, boost_mwh,
                         cee_red_mcf, rs_red_mcf, boost_mcf), n = Inf)

  boost %>%
    select(build_year, boost_mwh, boost_mcf)
}

#' Apply retrofit boost to a non-SFD profile.
#' Amplifies the ResStock envelope reduction to match CEEStock's SFD effect.
#' The baseline profile provides the anchor; the retrofit profile provides
#' the raw ResStock reduction; the boost scales that reduction up.
apply_retrofit_boost <- function(corrected_baseline, corrected_retrofit, boost) {
  corrected_baseline %>%
    inner_join(corrected_retrofit,
               by = c("mc_classification", "build_year", "sqft_bin"),
               suffix = c("_bl", "_ret")) %>%
    inner_join(boost, by = "build_year") %>%
    mutate(
      # Original RS retention fraction
      ret_frac_mwh = scenario_mwh_ret / scenario_mwh_bl,
      ret_frac_mcf = scenario_mcf_ret / scenario_mcf_bl,
      # Boosted reduction: clamp to prevent overshooting zero
      scenario_mwh = scenario_mwh_bl * pmax(0, 1 - (1 - ret_frac_mwh) * boost_mwh),
      scenario_mcf = scenario_mcf_bl * pmax(0, 1 - (1 - ret_frac_mcf) * boost_mcf)
    ) %>%
    select(mc_classification, build_year, sqft_bin, scenario_mwh, scenario_mcf)
}

# -- Boost factor sets ---------------------------------------------------------

# Envelope effect in baseline (gas furnace) context
retrofit_boost <- compute_retrofit_boost(
  ceestock_summaries$cee_baseline_sfd, ceestock_summaries$cee_retrofit_sfd,
  resstock_summaries$sf_detached_vintagesqft_baseline,
  resstock_summaries$sf_detached_vintagesqft_envelope,
  vintage_scalars
)

# Envelope effect in fully electrified context
retrofit_elec_boost <- compute_retrofit_boost(
  ceestock_summaries$cee_full_elec_sfd, ceestock_summaries$cee_retrofit_full_elec_sfd,
  resstock_summaries$sf_detached_vintagesqft_baseline,
  resstock_summaries$sf_detached_vintagesqft_envelope,
  vintage_scalars
)


# ==============================================================================
# Assemble profiles
# ==============================================================================

# -- Baseline (no interventions) -----------------------------------------------

baseline <- build_unified(
  "cee_baseline_sfd", "baseline",
  resstock_summaries$mf_baseline, resstock_summaries$manufactured_baseline
)

# -- Retrofit (envelope only, boosted non-SFD) ---------------------------------

retrofit <- bind_rows(
  standardize_sfd(ceestock_summaries$cee_retrofit_sfd),
  apply_retrofit_boost(
    apply_scalar_vintagesqft(resstock_summaries$sf_attached_vintagesqft_baseline, vintage_scalars),
    apply_scalar_vintagesqft(resstock_summaries$sf_attached_vintagesqft_envelope, vintage_scalars),
    retrofit_boost
  ),
  apply_retrofit_boost(
    apply_scalar_vintage(resstock_summaries$mf_baseline, vintage_scalars),
    apply_scalar_vintage(resstock_summaries$mf_envelope, vintage_scalars),
    retrofit_boost
  ),
  apply_retrofit_boost(
    apply_scalar_vintage(resstock_summaries$manufactured_baseline, vintage_scalars),
    apply_scalar_vintage(resstock_summaries$manufactured_envelope, vintage_scalars),
    retrofit_boost
  )
)

# -- Full electrification (no envelope) ----------------------------------------

full_electrification <- bind_rows(
  standardize_sfd(ceestock_summaries$cee_full_elec_sfd),
  build_full_elec(
    resstock_summaries$sf_attached_vintagesqft_baseline,
    resstock_summaries$sf_attached_baseline_enduse,
    vintage_scalars, electrification_ratios, has_sqft = TRUE
  ),
  build_full_elec(
    resstock_summaries$mf_baseline_enduse,
    resstock_summaries$mf_baseline_enduse,
    vintage_scalars, electrification_ratios, has_sqft = FALSE
  ),
  build_full_elec(
    resstock_summaries$manufactured_baseline_enduse,
    resstock_summaries$manufactured_baseline_enduse,
    vintage_scalars, electrification_ratios, has_sqft = FALSE
  )
)

# -- Combination (envelope + full electrification, boosted non-SFD) ------------
# Build unboosted full_elec and retrofit_full_elec for non-SFD, then apply
# the retrofit_elec_boost to calibrate the envelope effect.

fe_sfa <- build_full_elec(
  resstock_summaries$sf_attached_vintagesqft_baseline,
  resstock_summaries$sf_attached_baseline_enduse,
  vintage_scalars, electrification_ratios, has_sqft = TRUE
)
rfe_sfa <- build_full_elec(
  resstock_summaries$sf_attached_vintagesqft_baseline,
  resstock_summaries$sf_attached_baseline_enduse,
  vintage_scalars, electrification_ratios, has_sqft = TRUE,
  retrofit_baseline = resstock_summaries$sf_attached_vintagesqft_envelope
)

fe_mf <- build_full_elec(
  resstock_summaries$mf_baseline_enduse,
  resstock_summaries$mf_baseline_enduse,
  vintage_scalars, electrification_ratios, has_sqft = FALSE
)
rfe_mf <- build_full_elec(
  resstock_summaries$mf_baseline_enduse,
  resstock_summaries$mf_baseline_enduse,
  vintage_scalars, electrification_ratios, has_sqft = FALSE,
  retrofit_baseline = resstock_summaries$mf_envelope
)

fe_manuf <- build_full_elec(
  resstock_summaries$manufactured_baseline_enduse,
  resstock_summaries$manufactured_baseline_enduse,
  vintage_scalars, electrification_ratios, has_sqft = FALSE
)
rfe_manuf <- build_full_elec(
  resstock_summaries$manufactured_baseline_enduse,
  resstock_summaries$manufactured_baseline_enduse,
  vintage_scalars, electrification_ratios, has_sqft = FALSE,
  retrofit_baseline = resstock_summaries$manufactured_envelope
)

combination <- bind_rows(
  standardize_sfd(ceestock_summaries$cee_retrofit_full_elec_sfd),
  apply_retrofit_boost(fe_sfa, rfe_sfa, retrofit_elec_boost),
  apply_retrofit_boost(fe_mf, rfe_mf, retrofit_elec_boost),
  apply_retrofit_boost(fe_manuf, rfe_manuf, retrofit_elec_boost)
)

rm(fe_sfa, rfe_sfa, fe_mf, rfe_mf, fe_manuf, rfe_manuf)

# -- New-build variants --------------------------------------------------------

new_build <- baseline %>% filter(build_year == "2000+")

new_build_sustainable <- bind_rows(
  resstock_summaries$sf_detached_vintagesqft_sust_new_build %>%
    transmute(mc_classification, build_year,
              sqft_bin = relabel_resstock_sqft(sqft_bin),
              scenario_mwh = median_kwh / 1000, scenario_mcf = median_mcf) %>%
    inner_join(vintage_scalars, by = "build_year") %>%
    transmute(mc_classification, build_year, sqft_bin,
              scenario_mwh = scenario_mwh * scalar_mwh,
              scenario_mcf = scenario_mcf * scalar_mcf),
  apply_scalar_vintagesqft(
    resstock_summaries$sf_attached_vintagesqft_sust_new_build, vintage_scalars
  ),
  apply_scalar_vintage(resstock_summaries$mf_sust_new_build, vintage_scalars),
  apply_scalar_vintage(resstock_summaries$manufactured_sust_new_build, vintage_scalars)
)


# ==============================================================================
# Export
# ==============================================================================

# Apply enforce_monotonic first (size ordering), then cap_sfa_at_sfd
# (physical constraint: attached <= detached). Cap is the final pass
# so SFA never exceeds SFD in the exported profiles.
finalize <- function(profile) cap_sfa_at_sfd(enforce_monotonic(profile))

building_summaries <- list(
  baseline             = finalize(baseline),
  retrofit             = finalize(retrofit),
  full_electrification = finalize(full_electrification),
  combination          = finalize(combination),
  new_build            = finalize(new_build),
  new_build_sustainable = finalize(new_build_sustainable),

  # Propane full electrification (separate schema)
  propane_sfd_full_elec = build_full_elec_propane(
    resstock_summaries$sf_detached_propane_enduse, electrification_ratios),
  propane_manufactured_full_elec = build_full_elec_propane(
    resstock_summaries$manufactured_propane_enduse, electrification_ratios),
  propane_mf_full_elec = build_full_elec_propane(
    resstock_summaries$mf_propane_enduse, electrification_ratios),

  # Metadata
  electrification_ratios = electrification_ratios,
  vintage_scalars        = vintage_scalars,
  retrofit_boost         = retrofit_boost,
  retrofit_elec_boost    = retrofit_elec_boost
)


# -- QC -----------------------------------------------------------------------

message("\n-- Profile schema check --")
standard_cols <- c("mc_classification", "build_year", "sqft_bin",
                   "scenario_mwh", "scenario_mcf")
scenario_keys <- c("baseline", "retrofit", "full_electrification",
                   "combination", "new_build", "new_build_sustainable")
for (key in scenario_keys) {
  tbl <- building_summaries[[key]]
  has_cols <- all(standard_cols %in% names(tbl))
  n_mc <- n_distinct(tbl$mc_classification)
  message(sprintf("  %-25s  cols_ok=%s  n_mc=%d  rows=%d",
                  key, has_cols, n_mc, nrow(tbl)))
}

message("\n-- SFA < SFD check (post scalar correction) --")
for (key in scenario_keys) {
  profile <- building_summaries[[key]]
  sfd <- profile %>% filter(mc_classification == "single_family_detached", sqft_bin != "all")
  sfa <- profile %>% filter(mc_classification == "single_family_attached", sqft_bin != "all")
  comp <- inner_join(sfa, sfd, by = c("build_year", "sqft_bin"), suffix = c("_sfa", "_sfd"))
  if (nrow(comp) == 0) next
  violations <- comp %>%
    filter(scenario_mwh_sfa > scenario_mwh_sfd + 1e-6 |
             scenario_mcf_sfa > scenario_mcf_sfd + 1e-6)
  status <- if (nrow(violations) == 0) "PASS" else sprintf("FAIL (%d violations)", nrow(violations))
  message(sprintf("  %-25s  %s", key, status))
  if (nrow(violations) > 0) print(violations, n = Inf)
}

message("\n-- Full electrification vs baseline --")
for (mc in c("single_family_attached", "multifamily_units", "manufactured_homes")) {
  bl <- building_summaries$baseline %>% filter(mc_classification == mc)
  fe <- building_summaries$full_electrification %>% filter(mc_classification == mc)
  comp <- inner_join(bl, fe, by = c("mc_classification", "build_year", "sqft_bin"),
                     suffix = c("_bl", "_fe")) %>%
    mutate(
      mcf_red = paste0(round((1 - scenario_mcf_fe / scenario_mcf_bl) * 100), "%"),
      mwh_inc = paste0(round((scenario_mwh_fe / scenario_mwh_bl - 1) * 100), "%")
    )
  if (nrow(comp) == 0) next
  message(sprintf("\n  %s:", mc))
  print(select(comp, build_year, sqft_bin, scenario_mcf_bl, scenario_mcf_fe,
               mcf_red, scenario_mwh_bl, scenario_mwh_fe, mwh_inc), n = Inf)
}

message("\n-- Retrofit energy reduction: CEEStock SFD --")
for (scenario_pair in list(
  list(label = "retrofit vs baseline", ret = "retrofit", bl = "baseline"),
  list(label = "combination vs full_elec", ret = "combination", bl = "full_electrification")
)) {
  bl_sfd <- building_summaries[[scenario_pair$bl]] %>%
    filter(mc_classification == "single_family_detached")
  ret_sfd <- building_summaries[[scenario_pair$ret]] %>%
    filter(mc_classification == "single_family_detached")
  comp <- inner_join(bl_sfd, ret_sfd,
                     by = c("mc_classification", "build_year", "sqft_bin"),
                     suffix = c("_bl", "_ret"))
  message(sprintf("\n  %s (SFD, CEEStock):", scenario_pair$label))
  message(sprintf("    MWh change: %+.1f%%",
                  mean((comp$scenario_mwh_ret / comp$scenario_mwh_bl - 1) * 100)))
  message(sprintf("    MCF change: %+.1f%%",
                  mean((comp$scenario_mcf_ret / comp$scenario_mcf_bl - 1) * 100)))
}

message("\n-- Retrofit energy reduction: ResStock-derived types (boosted) --")
for (scenario_pair in list(
  list(label = "retrofit vs baseline", ret = "retrofit", bl = "baseline"),
  list(label = "combination vs full_elec", ret = "combination", bl = "full_electrification")
)) {
  for (mc in c("single_family_attached", "multifamily_units", "manufactured_homes")) {
    bl <- building_summaries[[scenario_pair$bl]] %>% filter(mc_classification == mc)
    ret <- building_summaries[[scenario_pair$ret]] %>% filter(mc_classification == mc)
    comp <- inner_join(bl, ret,
                       by = c("mc_classification", "build_year", "sqft_bin"),
                       suffix = c("_bl", "_ret"))
    if (nrow(comp) == 0) {
      message(sprintf("  %s — %s: NO ROWS (check resstock_summaries)", scenario_pair$label, mc))
      next
    }
    message(sprintf("  %s — %s (n=%d):", scenario_pair$label, mc, nrow(comp)))
    message(sprintf("    MWh change: %+.1f%%",
                    mean((comp$scenario_mwh_ret / comp$scenario_mwh_bl - 1) * 100)))
    message(sprintf("    MCF change: %+.1f%%",
                    mean((comp$scenario_mcf_ret / comp$scenario_mcf_bl - 1) * 100)))
  }
}


usethis::use_data(building_summaries, overwrite = TRUE)
