#' @title Calculate residential building energy profiles
#' @family residential
#' @family buildings
#'
#' @description This function assigns city-level single-family energy demand per building
#' according to parcel data building characteristics: median year of construction,
#' median square footage. SFA parcel sqft is capped at the CTU's median SFD sqft
#' to ensure that density increases (which shift SFD units to SFA) do not
#' inadvertently increase energy use.
#'
#' @inheritParams run_scenario_building
#' @inheritParams filter_ctu
#'
#' @return [tibble::tibble()].
#'       A table with columns `mc_classification`, `sqft_bin`, `year_bin`,
#'       `scenario_mwh`, `scenario_mcf`, and `scenario`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.gert)
#'
#' calc_building_energy(.selected_ctu = "Fridley")
#' }
#' @importFrom dplyr filter mutate select left_join case_when if_else pull bind_rows
#' @importFrom purrr map_dfr map
calc_building_energy <- function(
  .selected_ctu,
  parcel_data = ghg.gert::parcel_ctu,
  building_tb = ghg.gert::building_summaries
) {
  # ── Bin functions ──────────────────────────────────────────────────────────
  # Two sqft schemes (detached = 7 fine bins, attached = 4 coarse bins) match
  # the CEEStock label conventions baked into building_summaries.
  # MF and manufactured get sqft_bin = "all" (no sqft dimension).

  bin_sqft <- function(sqft, classification) {
    case_when(
      classification %in% c("single_family_detached", "single_family_attached") ~
        as.character(cut(sqft,
          breaks = c(0, 999, 1499, 1999, 2499, 2999, 3999, Inf),
          labels = c(
            "<1000", "1000 to 1499", "1500 to 1999", "2000 to 2499",
            "2500 to 2999", "3000 to 3999", "4000+"
          ),
          right = TRUE
        )),
      # MF and manufactured: no sqft dimension
      TRUE ~ "all"
    )
  }

  bin_year <- function(year) {
    as.character(cut(year,
      breaks = c(0, 1939, 1959, 1979, 1999, 2025.1),
      labels = c("<1940", "1940-59", "1960-79", "1980-99", "2000+"),
      right = TRUE
    ))
  }

  # ── Bin orderings for nearest-match fallback ───────────────────────────────

  sqft_orderings <- list(
    # bins for each building classification
    single_family_detached = c(
      "<1000", "1000 to 1499", "1500 to 1999",
      "2000 to 2499", "2500 to 2999",
      "3000 to 3999", "4000+"
    ),
    single_family_attached = c("<1000", "1000 to 1999", "2000 to 2999", "3000+"),
    multifamily_units = "all",
    manufactured_homes = "all"
  )

  year_ordering <- c("<1940", "1940-59", "1960-79", "1980-99", "2000+")

  # ── Nearest-bin fallback ───────────────────────────────────────────────────
  # For parcels that didn't match a profile row (NA scenario_mwh),
  # find the closest match by ordinal bin distance.

  # when building new SFA, it is never building them bigger than SFD median size
  # some cities have really big townhouses, so if they were swtiching from detatched to attached,
  patch_unmatched <- function(joined, lookup) {
    na_mask <- is.na(joined$scenario_mwh)
    if (!any(na_mask)) {
      return(joined)
    }

    complete <- joined[!na_mask, ]
    missing <- joined[na_mask, ]

    patched <- purrr::map_dfr(seq_len(nrow(missing)), function(i) {
      row <- missing[i, ]
      mc <- row$mc_classification

      candidates <- filter(lookup, mc_classification == mc)
      if (nrow(candidates) == 0) {
        return(row)
      }

      # Sqft distance (0 for "all" bins)
      # if this comes out as NA or 0, it gets patched
      sqft_ord <- sqft_orderings[[mc]]
      sqft_dist <- if (length(sqft_ord) == 1 && sqft_ord == "all") {
        rep(0, nrow(candidates))
      } else {
        val_pos <- match(row$sqft_bin, sqft_ord)
        cand_pos <- match(candidates$sqft_bin, sqft_ord)
        ifelse(is.na(val_pos) | is.na(cand_pos), Inf, abs(val_pos - cand_pos))
      }

      # Year distance
      val_yr <- match(row$year_bin, year_ordering)
      cand_yr <- match(candidates$build_year, year_ordering)
      year_dist <- ifelse(is.na(val_yr) | is.na(cand_yr), Inf, abs(val_yr - cand_yr))

      best <- candidates[which.min(sqft_dist + year_dist), ]
      row$scenario_mwh <- best$scenario_mwh
      row$scenario_mcf <- best$scenario_mcf
      row
    })

    bind_rows(complete, patched)
  }

  # ── Filter and bin parcels ─────────────────────────────────────────────────

  keep_classes <- c(
    "single_family_detached", "single_family_attached",
    "multifamily_units", "manufactured_homes"
  )



  ctu_binned <- parcel_data %>%
    filter(geog_name == .selected_ctu, mc_classification %in% keep_classes) %>%
    mutate(
      sqft_bin = bin_sqft(sq_ft_use, mc_classification),
      year_bin = bin_year(median_year)
    )

  # ── Build all scenarios ────────────────────────────────────────────────────
  # Every entry in building_tb has the same columns:
  #   mc_classification, build_year, sqft_bin, scenario_mwh, scenario_mcf
  # So the join logic is identical for every scenario.

  scenario_keys <- c(
    "baseline",
    "retrofit",
    "full_electrification",
    "combination",
    "new_build",
    "new_build_sustainable"
  )

  # Cap new SFA sqft at the CTU's median SFD sqft before binning.
  # Density increases shift SFD units to SFA; new SFA construction shouldn't
  # assume larger homes than the SFD stock it replaces.

  sfd_median_sqft <- parcel_data %>%
    filter(
      geog_name == .selected_ctu,
      mc_classification == "single_family_detached"
    ) %>%
    pull(sq_ft_use) %>%
    median(na.rm = TRUE)

  new_keys <- c("new_build", "new_build_sustainable")

  ctu_energy_profile <- purrr::map(scenario_keys, function(key) {
    profile <- building_tb[[key]]
    if (is.null(profile)) {
      warning("No profile found for scenario: ", key)
      return(NULL)
    }

    parcels <- ctu_binned
    if (key %in% new_keys && !is.na(sfd_median_sqft)) {
      parcels <- parcels %>%
        mutate(
          sqft_bin = if_else(
            mc_classification == "single_family_attached" &
              !is.na(sq_ft_use) & sq_ft_use > sfd_median_sqft,
            bin_sqft(sfd_median_sqft, mc_classification),
            sqft_bin
          )
        )
    }
    # make the join to the profile
    joined <- parcels %>%
      left_join(
        profile,
        by = c("mc_classification", "year_bin" = "build_year", "sqft_bin")
      )

    # perform patch
    joined <- patch_unmatched(joined, profile)

    joined %>%
      select(mc_classification, sqft_bin, year_bin, scenario_mwh, scenario_mcf) %>%
      mutate(scenario = key)
  }) %>%
    bind_rows()

  ctu_energy_profile
}
