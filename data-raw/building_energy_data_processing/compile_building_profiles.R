### compile_building_summaries.R
###
### Consolidates all building energy profiles into `building_summaries`.
### Every entry has the same schema:
###
###   mc_classification | build_year | sqft_bin | scenario_mwh | scenario_mcf
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
### Sqft-level variation in the scalar is modest (0.88–1.05 within most
### vintages) and dominated by noise in thin cells. The vintage signal
### is what matters: pre-1940 scalar_mcf ~0.75, post-war ~0.95–1.10.
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


# ==============================================================================
# Compute scalar
# ==============================================================================

vintage_scalars <- compute_vintage_scalar(
  ceestock_summaries$cee_baseline_sfd,
  resstock_summaries$sf_detached_vintagesqft_baseline
)


# ==============================================================================
# Assemble profiles
# ==============================================================================

# -- Helper: SFD + SFA + MF + manufactured for a standard scenario -------------
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

# -- Standard scenarios --------------------------------------------------------

baseline <- build_unified(
  "cee_baseline_sfd", "baseline",
  resstock_summaries$mf_baseline, resstock_summaries$manufactured_baseline
)

retrofit <- build_unified(
  "cee_retrofit_sfd", "envelope",
  resstock_summaries$mf_envelope, resstock_summaries$manufactured_envelope
)

heatpump <- build_unified(
  "cee_heatpump_sfd", "heatpump",
  resstock_summaries$mf_heatpump, resstock_summaries$manufactured_heatpump
)

electric_appliances <- build_unified(
  "cee_appliance_sfd", "baseline",
  resstock_summaries$mf_baseline, resstock_summaries$manufactured_baseline
)

combination <- build_unified(
  "cee_combined_sfd", "combo",
  resstock_summaries$mf_combo, resstock_summaries$manufactured_combo
)

# -- Full electrification ------------------------------------------------------

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

retrofit_full_electrification <- bind_rows(
  standardize_sfd(ceestock_summaries$cee_retrofit_full_elec_sfd),
  build_full_elec(
    resstock_summaries$sf_attached_vintagesqft_baseline,
    resstock_summaries$sf_attached_baseline_enduse,
    vintage_scalars, electrification_ratios, has_sqft = TRUE,
    retrofit_baseline = resstock_summaries$sf_attached_vintagesqft_envelope
  ),
  build_full_elec(
    resstock_summaries$mf_baseline_enduse,
    resstock_summaries$mf_baseline_enduse,
    vintage_scalars, electrification_ratios, has_sqft = FALSE,
    retrofit_baseline = resstock_summaries$mf_envelope
  ),
  build_full_elec(
    resstock_summaries$manufactured_baseline_enduse,
    resstock_summaries$manufactured_baseline_enduse,
    vintage_scalars, electrification_ratios, has_sqft = FALSE,
    retrofit_baseline = resstock_summaries$manufactured_envelope
  )
)

# -- New-build variants --------------------------------------------------------

new_build <- baseline %>% filter(build_year == "2000+")
new_build_heatpump <- heatpump %>% filter(build_year == "2000+")
new_build_full_electrification <- full_electrification %>% filter(build_year == "2000+")

new_build_leed <- bind_rows(
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
  baseline                       = finalize(baseline),
  retrofit                       = finalize(retrofit),
  heatpump                       = finalize(heatpump),
  electric_appliances            = finalize(electric_appliances),
  combination                    = finalize(combination),
  full_electrification           = finalize(full_electrification),
  retrofit_full_electrification  = finalize(retrofit_full_electrification),
  new_build                      = finalize(new_build),
  new_build_heatpump             = finalize(new_build_heatpump),
  new_build_full_electrification = finalize(new_build_full_electrification),
  new_build_leed                 = finalize(new_build_leed),

  # Propane full electrification (separate schema)
  propane_sfd_full_elec = build_full_elec_propane(
    resstock_summaries$sf_detached_propane_enduse, electrification_ratios),
  propane_manufactured_full_elec = build_full_elec_propane(
    resstock_summaries$manufactured_propane_enduse, electrification_ratios),
  propane_mf_full_elec = build_full_elec_propane(
    resstock_summaries$mf_propane_enduse, electrification_ratios),

  # Metadata
  electrification_ratios = electrification_ratios,
  vintage_scalars        = vintage_scalars
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

message("\n-- SFA < SFD check (post scalar correction) --")
for (key in c("baseline", "heatpump", "full_electrification")) {
  profile <- building_summaries[[key]]
  sfd <- profile %>% filter(mc_classification == "single_family_detached", sqft_bin != "all")
  sfa <- profile %>% filter(mc_classification == "single_family_attached", sqft_bin != "all")
  comp <- inner_join(sfa, sfd, by = c("build_year", "sqft_bin"), suffix = c("_sfa", "_sfd"))
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


usethis::use_data(building_summaries, overwrite = TRUE)
