# Township group helpers --------------------------------------------------------
#
# Shared definitions and helpers for the aggregate "township group"
# geographies (Carver Townships, Scott Townships). Each data-raw build script
# appends its own township-group rows just before use_data(), using these
# helpers so the membership, IDs and designations live in one place.
#
# Backend build helper, not part of the package: lives in data-raw and is
# sourced by build scripts after load_all():
#   pkgload::load_all()
#   source("data-raw/meta/compile_townships.R")
#
# Typical use in a build script:
#
#   lkp <- twp_group_lookup()                       # or pass a fresh CTU table
#   new <- twp_group_sum(df, "value", c("sector", "emissions_year"),
#                        .fill = list(geog_level = TWP_GROUP_LEVEL),
#                        lookup = lkp)
#   df  <- twp_group_replace(df, new)
#   twp_group_check_sum(df, "value", "my_dataset", lookup = lkp)
#
# Anticipated additions as more scripts adopt this (not built yet):
#   - weighted-mean aggregation (AVO, intersection density, parcel medians)
#     -- twp_wmean() is here as the building block
#   - per-dataset column mapping for tables using ctu_class / fips_id /
#     county_name instead of geog_level / geog_id (handled today via
#     name_col, id_col and .fill)

# Definitions -------------------------------------------------------------------

#' geog_level assigned to township-group rows. A distinct level keeps them out
#' of existing filters such as geog_level %in% c("CITY", "TOWNSHIP") and
#' geog_level == "COUNTY".
TWP_GROUP_LEVEL <- "TOWNSHIP_GROUP"

TWP_GROUP_ID_TYPE <- "township_group"

#' One row per township group.
#'
#' imagine_designation drives the per-job MWh/mcf coefficients in
#' calc_energy_non_residential(). Values are the most common designation
#' among member townships:
#'   Carver: 9 Agricultural, 1 Diversified Rural (Laketown)
#'   Scott:  4 Diversified Rural, 3 Agricultural, 2 Rural Residential,
#'           1 Suburban Edge (Jackson)
twp_group_meta <- function() {
  tibble::tribble(
    ~county_name, ~county_fips, ~geog_name,          ~geog_id,  ~imagine_designation, ~n_members,
    "Carver",     "27019",      "Carver Townships",  "27019T",  "Agricultural",       10L,
    "Scott",      "27139",      "Scott Townships",   "27139T",  "Diversified Rural",  10L
  )
}

twp_group_names <- function() twp_group_meta()$geog_name

# Membership --------------------------------------------------------------------

#' Township -> group lookup
#'
#' Townships are identified by the " Twp." suffix on geog_name, which is the
#' package-wide naming convention (geog_index.R, compile_ccap_geography.R).
#' Group rows themselves never match, so the lookup can be built from a table
#' that already contains them.
#'
#' @param ctu_df Table with geog_name, geog_id, county_name. Defaults to
#'   ctu_county_area. Pass a freshly built table in scripts that run before
#'   ctu_county_area is saved.
#' @return tibble: geog_name, geog_id, county_name, group_name, group_id,
#'   group_designation
twp_group_lookup <- function(ctu_df = ghg.gert::ctu_county_area) {
  meta <- twp_group_meta()

  twp <- ctu_df %>%
    dplyr::filter(
      grepl(" Twp\\.$", geog_name),
      county_name %in% meta$county_name
    )

  # every member township must sit wholly in one county
  if ("pct_of_ctu_area" %in% names(twp)) {
    split_twp <- twp %>% dplyr::filter(round(pct_of_ctu_area, 6) != 1)
    if (nrow(split_twp) > 0) {
      cli::cli_abort(
        "Townships split across counties: {.val {unique(split_twp$geog_name)}}"
      )
    }
  }

  lkp <- twp %>%
    dplyr::distinct(geog_name, geog_id, county_name) %>%
    dplyr::inner_join(
      meta %>%
        dplyr::select(
          county_name,
          group_name = geog_name,
          group_id = geog_id,
          group_designation = imagine_designation
        ),
      by = "county_name"
    )

  counts <- dplyr::count(lkp, county_name)
  expected <- dplyr::select(meta, county_name, n = n_members)
  if (!isTRUE(all.equal(
    dplyr::arrange(counts, county_name),
    dplyr::arrange(expected, county_name),
    check.attributes = FALSE
  ))) {
    cli::cli_abort(c(
      "Township membership does not match twp_group_meta().",
      "i" = "Found: {paste(counts$county_name, counts$n, collapse = ', ')}"
    ))
  }

  if (anyDuplicated(lkp$geog_name)) {
    cli::cli_abort("Duplicate township names in lookup.")
  }

  lkp
}

# Row helpers -------------------------------------------------------------------

#' Attach group_name / group_id / group_designation to township rows.
#' Non-member rows are dropped.
#' @param by column in df holding the township name
twp_group_tag <- function(df, by = "geog_name", lookup = twp_group_lookup()) {
  df %>%
    dplyr::inner_join(
      lookup %>%
        dplyr::select(geog_name, group_county = county_name, group_name, group_id, group_designation),
      by = stats::setNames("geog_name", by)
    )
}

#' Remove township-group rows. Use when reading a dataset as an input to
#' another aggregation (county or region roll-ups) to avoid double counting.
twp_group_drop <- function(df, name_col = "geog_name") {
  df[!df[[name_col]] %in% twp_group_names(), , drop = FALSE]
}

#' Drop any existing group rows from df and bind the new ones, keeping df's
#' column order. Idempotent, so build scripts can be re-run safely.
twp_group_replace <- function(df, new, name_col = "geog_name") {
  new <- dplyr::select(new, dplyr::any_of(names(df)))

  missing_cols <- setdiff(names(df), names(new))
  if (length(missing_cols) > 0) {
    cli::cli_warn("Township group rows: columns filled with NA: {.val {missing_cols}}")
  }

  out <- dplyr::bind_rows(twp_group_drop(df, name_col), new)
  cli::cli_alert_success(
    "Added {nrow(new)} township-group row{?s} ({.val {intersect(twp_group_names(), new[[name_col]])}})"
  )
  out
}

# Aggregation -------------------------------------------------------------------

#' Sum township rows into township-group rows
#'
#' @param df Dataset containing township rows.
#' @param value_cols Columns to sum.
#' @param group_cols Columns to keep as grouping keys (e.g. sector, year).
#'   Do not include geography columns that vary by township.
#' @param name_col,id_col Names of the geography name / id columns in df.
#'   Set id_col = NULL when df has no id column.
#' @param once_cols Columns repeated on every row of a township (e.g.
#'   population, geog_pop). Summed once per township within `once_by`, then
#'   joined back.
#' @param once_by Subset of group_cols that once_cols vary by (e.g. year).
#' @param .fill Named list of constant columns to set on the new rows
#'   (e.g. list(geog_level = TWP_GROUP_LEVEL)).
#' @param designation_col If set, this column gets the group's
#'   imagine_designation.
#' @param county_col If set, this column gets the group's county_name.
#' @return Township-group rows only, ready for twp_group_replace().
twp_group_sum <- function(df,
                          value_cols,
                          group_cols,
                          name_col = "geog_name",
                          id_col = "geog_id",
                          once_cols = NULL,
                          once_by = NULL,
                          .fill = list(),
                          designation_col = NULL,
                          county_col = NULL,
                          lookup = twp_group_lookup()) {
  twp <- twp_group_tag(df, by = name_col, lookup = lookup)
  keys <- c("group_name", "group_id", "group_designation", "group_county")

  out <- twp %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(keys, group_cols)))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(value_cols), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  if (length(once_cols) > 0) {
    once <- twp %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(c("group_name", name_col, once_by, once_cols)))) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c("group_name", once_by)))) %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(once_cols), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )
    out <- dplyr::left_join(out, once, by = c("group_name", once_by))
  }

  out[[name_col]] <- out$group_name
  if (!is.null(id_col)) out[[id_col]] <- out$group_id
  if (!is.null(designation_col)) out[[designation_col]] <- out$group_designation
  if (!is.null(county_col)) out[[county_col]] <- out$group_county
  for (nm in names(.fill)) out[[nm]] <- .fill[[nm]]

  dplyr::select(out, -dplyr::all_of(keys))
}

#' Weighted mean that falls back to a simple mean when weights are all 0/NA.
twp_wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(w[ok]) > 0) stats::weighted.mean(x[ok], w[ok]) else mean(x, na.rm = TRUE)
}

#' Keep a categorical value if all members agree, otherwise `mixed`.
twp_collapse_chr <- function(x, mixed = "aggregated") {
  u <- unique(stats::na.omit(x))
  if (length(u) == 1) u else mixed
}

# Checks ------------------------------------------------------------------------

#' Confirm group totals equal member-township totals for a summed column.
#' Returns TRUE/FALSE invisibly; set strict = TRUE to error on mismatch.
twp_group_check_sum <- function(df, value_col, label = value_col,
                                name_col = "geog_name",
                                lookup = twp_group_lookup(),
                                strict = FALSE) {
  member_total <- twp_group_tag(df, by = name_col, lookup = lookup)[[value_col]] %>%
    sum(na.rm = TRUE)
  group_total <- df[df[[name_col]] %in% twp_group_names(), ][[value_col]] %>%
    sum(na.rm = TRUE)

  ok <- isTRUE(all.equal(member_total, group_total, tolerance = 1e-6))
  if (ok) {
    cli::cli_alert_success("{label}: township group totals match")
  } else {
    msg <- "{label}: members {member_total} vs groups {group_total}"
    if (strict) cli::cli_abort(msg) else cli::cli_alert_danger(msg)
  }
  invisible(ok)
}

#' Confirm a dataset has rows for every township group.
#' Intended for a final pre-release check across all datasets.
twp_group_check_present <- function(df, label, name_col = "geog_name") {
  missing <- setdiff(twp_group_names(), unique(df[[name_col]]))
  if (length(missing) == 0) {
    cli::cli_alert_success("{label}: township groups present")
  } else {
    cli::cli_alert_danger("{label}: missing {.val {missing}}")
  }
  invisible(length(missing) == 0)
}
