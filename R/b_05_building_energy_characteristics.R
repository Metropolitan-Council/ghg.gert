#' @title Adjust residential building energy needs
#' @family residential
#' @family buildings
#'
#' @description This function adjusts city level single-family energy demand per building
#' according to parcel data building characteristics: median year of construction,
#' median square footage
#'
#' @inheritParams run_scenario_building
#' @inheritParams filter_ctu
#' @inheritParams calc_ghg_residential
#' @param .new_homes_to_multifamily_pct numeric,  a value between `0` and `1`.
#'      Percentage of new single-family homes to instead be built as multifamily homes.
#'      Default is `0.0`.
#'
#' @return [tibble::tibble()].
#'       A table with columns `geog_name`, `geog_id`, `year`, `var`, and `value`.
#'       Table contains adjusted `single_family_units` and `multifamily_units` record for column `var`
#'       relative to residential inputs table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.ccap)
#'
#' adj_unit_counts(
#'   res_tb = compile_bau_building_energy()$residential,
#'   .selected_ctu = "all",
#'   .new_homes_to_multifamily_pct = 0.50
#' )
#' }
#' @importFrom dplyr filter group_by mutate select ungroup anti_join bind_rows
#' @importFrom tidyr pivot_wider
#' @importFrom cli cli_warn
calc_building_energy <- function(
    .selected_ctu,
    parcel_data = ghg.ccap::parcel_ctu,
    resstock_tb = ghg.ccap::resstock_summaries,
    ceestock_tb = ghg.ccap::ceestock_summaries
) {
  # --- Bin functions --------------------------------------------------------
  bin_sqft_attached <- function(sqft) {
    cut(sqft,
        breaks = c(0, 999, 1999, 2999, Inf),
        labels = c("<1000", "1000 to 1999", "2000 to 2999", "3000+"),
        right = TRUE
    )
  }

  bin_sqft_detached <- function(sqft) {
    cut(sqft,
        breaks = c(0, 999, 1499, 1999, 2499, 2999, 3999, Inf),
        labels = c(
          "<1000", "1000 to 1499", "1500 to 1999", "2000 to 2499",
          "2500 to 2999", "3000 to 3999", "4000+"
        ),
        right = TRUE
    )
  }

  bin_year <- function(year) {
    cut(year,
        breaks = c(0, 1939, 1959, 1979, 1999, 2025.1),
        labels = c("<1940", "1940-59", "1960-79", "1980-99", "2000+"),
        right = TRUE
    )
  }

  resstock_sqft_bin <- function(sqft) {
    cut(sqft,
        breaks = c(0, 999, 1499, 1999, 2499, 2999, Inf),
        labels = c(
          "Less than 1,000", "1,000 to 1,499", "1,500 to 1,999",
          "2,000 to 2,499", "2,500 to 2,999", "3,000 or more"
        ),
        right = TRUE
    )
  }

  # --- Bin orderings for nearest-match fallback -----------------------------
  bin_orderings <- list(
    sqft_detached = c(
      "<1000", "1000 to 1499", "1500 to 1999", "2000 to 2499",
      "2500 to 2999", "3000 to 3999", "4000+"
    ),
    sqft_attached = c("<1000", "1000 to 1999", "2000 to 2999", "3000+"),
    sqft_resstock = c(
      "Less than 1,000", "1,000 to 1,499", "1,500 to 1,999",
      "2,000 to 2,499", "2,500 to 2,999", "3,000 or more"
    ),
    year = c("<1940", "1940-59", "1960-79", "1980-99", "2000+")
  )

  get_sqft_ordering <- function(classification) {
    if (classification == "single_family_detached") {
      bin_orderings$sqft_detached
    } else {
      bin_orderings$sqft_attached
    }
  }

  # --- General nearest-bin fallback -----------------------------------------
  # For each row with NA scenario_mwh, find the nearest match in the lookup

  # by ordinal bin distance within the same mc_classification.
  #
  # bin_specs: list of list(joined_col, lookup_col, ordering)
  #   - ordering can be a character vector (same for all rows) or
  #     "sqft_by_class" to auto-select based on mc_classification
  # value_specs: list of list(target, source, fn)
  #   - maps lookup columns to joined columns with an optional transform

  patch_unmatched <- function(joined, lookup, bin_specs, value_specs) {
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

      total_dist <- rep(0, nrow(candidates))
      for (spec in bin_specs) {
        ordering <- if (identical(spec$ordering, "sqft_by_class")) {
          get_sqft_ordering(mc)
        } else {
          spec$ordering
        }
        val_pos <- match(as.character(row[[spec$joined_col]]), ordering)
        cand_pos <- match(as.character(candidates[[spec$lookup_col]]), ordering)
        dist <- ifelse(
          is.na(val_pos) | is.na(cand_pos),
          Inf,
          abs(val_pos - cand_pos)
        )
        total_dist <- total_dist + dist
      }

      best <- candidates[which.min(total_dist), ]
      for (vs in value_specs) {
        row[[vs$target]] <- vs$fn(best[[vs$source]])
      }
      row
    })

    bind_rows(complete, patched)
  }

  # Pre-built value specs for the two lookup types
  ceestock_values <- list(
    list(target = "scenario_mwh", source = "elec_mwh", fn = identity),
    list(target = "scenario_mcf", source = "gas_mcf", fn = identity)
  )
  resstock_values <- list(
    list(target = "scenario_mwh", source = "median_kwh", fn = \(x) x / 1000),
    list(target = "scenario_mcf", source = "median_mcf", fn = identity)
  )

  # --- Filter and bin parcels -----------------------------------------------
  ctu_binned <- parcel_data %>%
    filter(
      geog_name == .selected_ctu,
      mc_classification %in% c(
        "single_family_detached", "single_family_attached",
        "multifamily_units", "manufactured_homes"
      )
    ) %>%
    mutate(
      sqft_bin = as.character(case_when(
        mc_classification == "single_family_detached" ~ bin_sqft_detached(sq_ft_use),
        mc_classification == "single_family_attached" ~ bin_sqft_attached(sq_ft_use),
        TRUE ~ NA
      )),
      year_bin = as.character(bin_year(median_year))
    )

  ctu_sf <- filter(ctu_binned, mc_classification %in% c(
    "single_family_detached", "single_family_attached"
  ))
  ctu_other <- filter(ctu_binned, mc_classification %in% c(
    "multifamily_units", "manufactured_homes"
  ))

  # --- Build scenarios ------------------------------------------------------
  build_scenario <- function(scenario_name, cee_sf, res_other, new_build = FALSE) {
    if (new_build) {
      cee_sf <- filter(cee_sf, build_year == "2000+")
      res_other <- filter(res_other, build_year == "2000+")
      sf_by <- c("mc_classification", "sqft_bin")
      other_by <- "mc_classification"
    } else {
      sf_by <- c("mc_classification", "sqft_bin", "year_bin" = "build_year")
      other_by <- c("mc_classification", "year_bin" = "build_year")
    }

    # --- SF part: join against ceestock ---
    sf_part <- left_join(ctu_sf, cee_sf, by = sf_by) %>%
      mutate(scenario_mwh = elec_mwh, scenario_mcf = gas_mcf) %>%
      select(mc_classification, sqft_bin, year_bin, scenario_mwh, scenario_mcf)

    sf_bin_specs <- list(
      list(joined_col = "sqft_bin", lookup_col = "sqft_bin", ordering = "sqft_by_class")
    )
    if (!new_build) {
      sf_bin_specs <- c(sf_bin_specs, list(
        list(joined_col = "year_bin", lookup_col = "build_year", ordering = bin_orderings$year)
      ))
    }

    sf_part <- patch_unmatched(sf_part, cee_sf, sf_bin_specs, ceestock_values) %>%
      select(mc_classification, scenario_mwh, scenario_mcf)

    # --- Other part: join against resstock ---
    other_part <- left_join(ctu_other, res_other, by = other_by) %>%
      mutate(scenario_mwh = median_kwh / 1000, scenario_mcf = median_mcf) %>%
      select(mc_classification, year_bin, scenario_mwh, scenario_mcf)

    if (!new_build) {
      other_bin_specs <- list(
        list(joined_col = "year_bin", lookup_col = "build_year", ordering = bin_orderings$year)
      )
      other_part <- patch_unmatched(other_part, res_other, other_bin_specs, resstock_values)
    }

    other_part <- select(other_part, mc_classification, scenario_mwh, scenario_mcf)

    bind_rows(sf_part, other_part) %>%
      mutate(scenario = scenario_name)
  }

  # --- Scenario configs -----------------------------------------------------
  res_baseline <- bind_rows(resstock_tb$mf_baseline, resstock_tb$manufactured_baseline)
  res_envelope <- bind_rows(resstock_tb$mf_envelope, resstock_tb$manufactured_envelope)
  res_heatpump <- bind_rows(resstock_tb$mf_heatpump, resstock_tb$manufactured_heatpump)
  res_combo <- bind_rows(resstock_tb$mf_combo, resstock_tb$manufactured_combo)

  scenario_config <- list(
    list("baseline", ceestock_tb$cee_baseline_sf, res_baseline, FALSE),
    list("new_build", ceestock_tb$cee_baseline_sf, res_baseline, TRUE),
    list("retrofit", ceestock_tb$cee_retrofit_sf, res_envelope, FALSE),
    list("heatpump", ceestock_tb$cee_heatpump_sf, res_heatpump, FALSE),
    list("combination", ceestock_tb$cee_combined_sf, res_combo, FALSE),
    list("new_build_heatpump", ceestock_tb$cee_heatpump_sf, res_heatpump, TRUE)
  )

  ctu_energy_profile <- purrr::map(
    scenario_config,
    \(cfg) build_scenario(cfg[[1]], cfg[[2]], cfg[[3]], cfg[[4]])
  ) %>%
    bind_rows()

  # --- Sustainable new build (new_build_leed) -------------------------------
  res_new_build_sust_sf <- bind_rows(
    resstock_tb$sf_attached_vintagesqft_sust_new_build,
    resstock_tb$sf_detached_vintagesqft_sust_new_build
  )

  ctu_sf_res <- ctu_sf %>%
    mutate(res_sq_ft = as.character(resstock_sqft_bin(sq_ft_use)))

  sf_sust_joined <- left_join(
    ctu_sf_res,
    res_new_build_sust_sf,
    join_by(mc_classification, res_sq_ft == sqft_bin)
  ) %>%
    mutate(scenario_mwh = median_kwh / 1000, scenario_mcf = median_mcf)

  sf_sust_joined <- patch_unmatched(
    sf_sust_joined,
    res_new_build_sust_sf,
    bin_specs = list(
      list(
        joined_col = "res_sq_ft", lookup_col = "sqft_bin",
        ordering = bin_orderings$sqft_resstock
      )
    ),
    value_specs = resstock_values
  ) %>%
    select(mc_classification, scenario_mwh, scenario_mcf)

  other_sust <- bind_rows(
    resstock_tb$mf_sust_new_build,
    resstock_tb$manufactured_sust_new_build
  ) %>%
    mutate(scenario_mwh = median_kwh / 1000, scenario_mcf = median_mcf) %>%
    select(mc_classification, scenario_mwh, scenario_mcf)

  res_new_build_sust_out <- bind_rows(other_sust, sf_sust_joined) %>%
    mutate(scenario = "new_build_leed")

  # --- Combine and return ---------------------------------------------------
  bind_rows(ctu_energy_profile, res_new_build_sust_out)
}
