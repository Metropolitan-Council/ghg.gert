test_that("energy demand increases with dwelling size within each profile", {
  sqft_order <- c(
    "<1000", "1000 to 1499", "1500 to 1999", "2000 to 2499",
    "2500 to 2999", "3000 to 3999", "4000+"
  )

  profile_keys <- c(
    "baseline", "retrofit", "heatpump", "electric_appliances",
    "combination", "full_electrification", "retrofit_full_electrification",
    "new_build", "new_build_heatpump", "new_build_full_electrification",
    "new_build_leed"
  )

  for (key in profile_keys) {
    profile <- building_summaries[[key]]
    if (is.null(profile)) next

    sf_rows <- profile %>% filter(sqft_bin != "all")
    if (nrow(sf_rows) == 0) next

    for (mc in unique(sf_rows$mc_classification)) {
      mc_rows <- sf_rows %>%
        filter(mc_classification == mc) %>%
        mutate(sqft_rank = match(sqft_bin, sqft_order))

      for (yr in unique(mc_rows$build_year)) {
        slice <- mc_rows %>%
          filter(build_year == yr) %>%
          arrange(sqft_rank)
        if (nrow(slice) < 2) next

        mwh_diffs <- diff(slice$scenario_mwh)
        expect_true(
          all(mwh_diffs >= -1e-6),
          label = sprintf(
            "%s / %s / %s: MWh should increase with sqft (drops at bin %s)",
            key, mc, yr,
            slice$sqft_bin[which(mwh_diffs < -1e-6)[1] + 1]
          )
        )

        mcf_diffs <- diff(slice$scenario_mcf)
        expect_true(
          all(mcf_diffs >= -1e-6),
          label = sprintf(
            "%s / %s / %s: MCF should increase with sqft (drops at bin %s)",
            key, mc, yr,
            slice$sqft_bin[which(mcf_diffs < -1e-6)[1] + 1]
          )
        )
      }
    }
  }
})


test_that("SFA energy demand is less than SFD at comparable sizes", {
  # With the SFD-anchor scalar approach, SFA now uses the same fine sqft bins
  # as SFD (minus 4000+ which is rare for SFA). Direct comparison at each
  # shared bin — no coarse-bin collapsing needed.

  profile_keys <- c(
    "baseline", "retrofit", "heatpump", "electric_appliances",
    "combination", "full_electrification", "retrofit_full_electrification"
  )

  for (key in profile_keys) {
    profile <- building_summaries[[key]]
    if (is.null(profile)) next

    sfd <- profile %>%
      filter(mc_classification == "single_family_detached", sqft_bin != "all")
    sfa <- profile %>%
      filter(mc_classification == "single_family_attached", sqft_bin != "all")

    if (nrow(sfd) == 0 || nrow(sfa) == 0) next

    comparison <- inner_join(sfa, sfd,
                             by = c("build_year", "sqft_bin"),
                             suffix = c("_sfa", "_sfd")
    )
    if (nrow(comparison) == 0) next

    expect_true(
      all(comparison$scenario_mwh_sfa <= comparison$scenario_mwh_sfd + 1e-6),
      label = sprintf("%s: SFA MWh should be <= SFD MWh at same sqft/vintage", key)
    )

    expect_true(
      all(comparison$scenario_mcf_sfa <= comparison$scenario_mcf_sfd + 1e-6),
      label = sprintf("%s: SFA MCF should be <= SFD MCF at same sqft/vintage", key)
    )
  }
})
