# test-adj_buildings.R
#
# Verifies that:
#   1. SFA parcel sqft is capped at median SFD sqft in calc_building_energy
#   2. Density increases do not increase per-unit energy (SFA <= SFD profiles)
#   3. adj_unit_counts shifts units in the expected direction

# Filter to CTUs only (adj_unit_counts and calc_building_energy are CTU-level)
ctu_test_list <- geography_test_list[
  !grepl("County$", geography_test_list)
]


# -- SFA sqft cap in calc_building_energy --------------------------------------

for (ctu in ctu_test_list) {
  test_that(paste("SFA sqft capped at SFD median for", ctu), {
    parcel_data <- ghg.ccap::parcel_ctu

    # browser()
    # Compute the SFD median for this CTU
    sfd_median <- parcel_data %>%
      dplyr::filter(
        geog_name == ctu,
        mc_classification == "single_family_detached"
      ) %>%
      dplyr::pull(sq_ft_use) %>%
      median(na.rm = TRUE)

    # Skip if no SFD parcels (e.g. Landfall)
    skip_if(is.na(sfd_median), message = paste("No SFD parcels in", ctu))

    # Run calc_building_energy — it applies the cap internally
    profiles <- calc_building_energy(.selected_ctu = ctu)

    # The SFA profiles returned should never land in a sqft bin
    # that implies larger homes than the SFD median
    sfa_profiles <- profiles %>%
      dplyr::filter(
        mc_classification == "single_family_attached",
        scenario == "baseline"
      )

    # Skip if no SFA parcels
    skip_if(nrow(sfa_profiles) == 0, message = paste("No SFA parcels in", ctu))

    # Compare: get the raw (uncapped) SFA parcels to verify the cap did something
    # or at least didn't break anything
    raw_sfa <- parcel_data %>%
      dplyr::filter(
        geog_name == ctu,
        mc_classification == "single_family_attached"
      )

    # Any raw SFA parcel above the SFD median should have been capped
    n_would_exceed <- sum(raw_sfa$sq_ft_use > sfd_median, na.rm = TRUE)

    if (n_would_exceed > 0) {
      # The capped profiles should all have sqft_bin values that correspond
      # to sqft <= sfd_median. Check via the bin ordering.
      sfa_bins <- c("<1000", "1000 to 1999", "2000 to 2999", "3000+")
      sfd_bin <- as.character(cut(
        sfd_median,
        breaks = c(0, 999, 1999, 2999, Inf),
        labels = sfa_bins,
        right = TRUE
      ))
      sfd_bin_pos <- match(sfd_bin, sfa_bins)

      # All SFA profile bins should be at or below the SFD median bin
      profile_bin_positions <- match(sfa_profiles$sqft_bin, sfa_bins)
      expect_true(
        all(profile_bin_positions <= sfd_bin_pos, na.rm = TRUE),
        label = paste(ctu, "- all SFA bins at or below SFD median bin")
      )
    } else {
      testthat::expect_equal(
        n_would_exceed, 0,
        label = paste(ctu, "- no raw SFA parcels exceed SFD median")
      )
    }
  })
}


# -- Density increase does not increase total building energy ------------------

for (ctu in ctu_test_list) {
  test_that(paste("Density increase does not increase emissions for", ctu), {
    res_tb <- building_energy_data$residential %>%
      dplyr::filter(geog_name == ctu | geog_name == "All")

    skip_if(
      nrow(dplyr::filter(res_tb, geog_name == ctu)) == 0,
      message = paste("No residential data for", ctu)
    )

    no_density <- list(expected_density = c(1.0, 1.0))
    hi_density <- list(expected_density = c(1.0, 1.5)) # 50% increase

    base <- adj_unit_counts(
      res_tb = res_tb,
      density_output = no_density,
      .selected_ctu = ctu
    )

    dense <- adj_unit_counts(
      res_tb = res_tb,
      density_output = hi_density,
      .selected_ctu = ctu
    )

    # Get profiles for both — the sqft cap ensures new SFA <= SFD
    base_profiles <- calc_building_energy(.selected_ctu = ctu)

    # Compare total energy at 2050: density should not increase it
    # Total energy = sum of (units * per-unit energy) across housing types
    # Since calc_building_energy returns per-unit profiles and adj_unit_counts
    # returns unit counts, we compare unit-weighted totals
    get_total_energy <- function(units_tb, scenario_name = "baseline") {
      units_2050 <- units_tb %>%
        dplyr::filter(emissions_year == 2050) %>%
        dplyr::group_by(sp_categories) %>%
        dplyr::summarise(units = sum(value, na.rm = TRUE), .groups = "drop")

      profiles <- base_profiles %>%
        dplyr::filter(scenario == scenario_name) %>%
        dplyr::group_by(mc_classification) %>%
        dplyr::summarise(
          mwh = median(scenario_mwh, na.rm = TRUE),
          mcf = median(scenario_mcf, na.rm = TRUE),
          .groups = "drop"
        )

      joined <- units_2050 %>%
        dplyr::inner_join(profiles,
          by = c("sp_categories" = "mc_classification")
        )

      sum(joined$units * (joined$mwh * 3.412 + joined$mcf * 1.037), na.rm = TRUE)
    }

    base_energy <- get_total_energy(base)
    dense_energy <- get_total_energy(dense)

    expect_lte(
      dense_energy, base_energy,
      label = paste(ctu, "- density increase should not increase total energy")
    )
  })
}


# -- adj_unit_counts shifts units correctly ------------------------------------

for (ctu in ctu_test_list) {
  test_that(paste("adj_unit_counts reduces SFD and increases SFA/MF for", ctu), {
    res_tb <- building_energy_data$residential %>%
      dplyr::filter(geog_name == ctu | geog_name == "All")

    # Create a density output with a meaningful increase
    density_output <- list(
      expected_density = c(1.0, 1.2) # 20% density increase
    )

    skip_if(
      nrow(dplyr::filter(res_tb, geog_name == ctu)) == 0,
      message = paste("No residential data for", ctu)
    )

    adjusted <- adj_unit_counts(
      res_tb = res_tb,
      density_output = density_output,
      .selected_ctu = ctu
    )

    original <- res_tb %>%
      dplyr::filter(geog_name == ctu)

    # Compare 2050 values
    get_2050 <- function(tb, sp_cat) {
      tb %>%
        dplyr::filter(
          emissions_year == 2050,
          sp_categories == sp_cat
        ) %>%
        dplyr::pull(value) %>%
        sum(na.rm = TRUE)
    }

    orig_sfd <- get_2050(original, "single_family_detached")
    adj_sfd <- get_2050(adjusted, "single_family_detached")

    orig_sfa <- get_2050(original, "single_family_attached")
    adj_sfa <- get_2050(adjusted, "single_family_attached")

    orig_mf <- get_2050(original, "multifamily_units")
    adj_mf <- get_2050(adjusted, "multifamily_units")

    skip_if(orig_sfd == 0, message = paste("No SFD units in", ctu))

    # SFD should decrease or stay the same
    expect_lte(adj_sfd, orig_sfd,
      label = paste(ctu, "- SFD units should decrease with density")
    )

    # SFA and MF should increase or stay the same
    expect_gte(adj_sfa, orig_sfa,
      label = paste(ctu, "- SFA units should increase with density")
    )
    expect_gte(adj_mf, orig_mf,
      label = paste(ctu, "- MF units should increase with density")
    )
  })
}


# -- No NAs in calc_building_energy output -------------------------------------

for (ctu in ctu_test_list) {
  test_that(paste("calc_building_energy has no NAs for", ctu), {
    profiles <- calc_building_energy(.selected_ctu = ctu)

    na_rows <- profiles %>%
      dplyr::filter(is.na(scenario_mwh) | is.na(scenario_mcf))

    expect_equal(
      nrow(na_rows), 0L,
      label = paste(
        ctu, "- NA profiles in scenarios:",
        paste(unique(na_rows$scenario), collapse = ", ")
      )
    )
  })
}
