test_that("energy demand increases with dwelling size within each profile", {
  # Sqft bin orderings by mc_classification
  sqft_order <- list(
    single_family_detached = c(
      "<1000", "1000 to 1499", "1500 to 1999", "2000 to 2499",
      "2500 to 2999", "3000 to 3999", "4000+"
    ),
    single_family_attached = c(
      "<1000", "1000 to 1999", "2000 to 2999", "3000+"
    )
  )

  # Test every standard profile that has SF rows
  profile_keys <- c(
    "baseline", "retrofit", "heatpump", "electric_appliances",
    "combination", "full_electrification", "retrofit_full_electrification",
    "new_build", "new_build_heatpump", "new_build_full_electrification",
    "new_build_leed"
  )

  for (key in profile_keys) {
    profile <- building_summaries[[key]]
    if (is.null(profile)) next

    sf_rows <- profile %>%
      filter(sqft_bin != "all")

    if (nrow(sf_rows) == 0) next

    for (mc in unique(sf_rows$mc_classification)) {
      ordering <- sqft_order[[mc]]
      if (is.null(ordering)) next

      mc_rows <- sf_rows %>%
        filter(mc_classification == mc) %>%
        mutate(sqft_rank = match(sqft_bin, ordering))

      for (yr in unique(mc_rows$build_year)) {
        slice <- mc_rows %>%
          filter(build_year == yr) %>%
          arrange(sqft_rank)

        if (nrow(slice) < 2) next

        # MWh should be non-decreasing with size
        mwh_diffs <- diff(slice$scenario_mwh)
        expect_true(
          all(mwh_diffs >= -1e-6),
          label = sprintf(
            "%s / %s / %s: MWh should increase with sqft (drops at bin %s)",
            key, mc, yr,
            slice$sqft_bin[which(mwh_diffs < -1e-6)[1] + 1]
          )
        )

        # MCF should be non-decreasing with size
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
  # Map SFD fine bins into SFA coarse bins for comparison
  sfd_to_sfa_bin <- c(
    "<1000"          = "<1000",
    "1000 to 1499"   = "1000 to 1999",
    "1500 to 1999"   = "1000 to 1999",
    "2000 to 2499"   = "2000 to 2999",
    "2500 to 2999"   = "2000 to 2999",
    "3000 to 3999"   = "3000+",
    "4000+"          = "3000+"
  )

  profile_keys <- c(
    "baseline", "retrofit", "heatpump", "electric_appliances",
    "combination", "full_electrification", "retrofit_full_electrification"
  )

  for (key in profile_keys) {
    profile <- building_summaries[[key]]
    if (is.null(profile)) next

    sfd <- profile %>%
      filter(mc_classification == "single_family_detached", sqft_bin != "all") %>%
      mutate(coarse_bin = sfd_to_sfa_bin[sqft_bin]) %>%
      group_by(build_year, coarse_bin) %>%
      summarise(
        sfd_mwh = mean(scenario_mwh),
        sfd_mcf = mean(scenario_mcf),
        .groups = "drop"
      )

    sfa <- profile %>%
      filter(mc_classification == "single_family_attached", sqft_bin != "all")

    if (nrow(sfd) == 0 || nrow(sfa) == 0) next

    comparison <- inner_join(
      sfa, sfd,
      by = c("build_year", "sqft_bin" = "coarse_bin")
    )

    if (nrow(comparison) == 0) next

    # SFA should use less electricity than SFD at same size/vintage
    expect_true(
      all(comparison$scenario_mwh <= comparison$sfd_mwh + 1e-6),
      label = sprintf("%s: SFA MWh should be <= SFD MWh at comparable sizes", key)
    )

    # SFA should use less gas than SFD at same size/vintage
    expect_true(
      all(comparison$scenario_mcf <= comparison$sfd_mcf + 1e-6),
      label = sprintf("%s: SFA MCF should be <= SFD MCF at comparable sizes", key)
    )
  }
})
