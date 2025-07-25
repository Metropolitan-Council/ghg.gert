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
    parcel_data = parcel_ctu,
    eia_type = eia_recs_energy_usage$eia_housing_type,
    eia_age = eia_recs_energy_usage$eia_housing_age,
    eia_sqft = eia_recs_energy_usage$eia_housing_sqft
) {
  # bin based on eia categories
  bin_sqft <- function(sqft) {
    cut(sqft,
        breaks = c(0, 999, 1499, 1999, 2499, 2999, Inf),
        labels = c("Less than 1,000", "1,000 to 1,499", "1,500 to 1,999",
                   "2,000 to 2,499", "2,500 to 2,999", "3,000 or more"),
        right = TRUE)
  }

  bin_year <- function(year) {
    cut(year,
        breaks = c(0, 1949, 1959, 1969, 1979, 1989, 1999, 2009, 2015, 2020),
        labels = c("Before 1950", "1950 to 1959", "1960 to 1969", "1970 to 1979",
                   "1980 to 1989", "1990 to 1999", "2000 to 2009", "2010 to 2015", "2016 to 2020"),
        right = TRUE)
  }

  # create adjustment tables
  sqft_adj <- eia_sqft %>%
    mutate(across(c(mwh, mcf), ~ .x / mean(.x, na.rm = TRUE), .names = "adj_{.col}")) %>%
    rename(sqft_bin = year_built)

  age_adj <- eia_age %>%
    mutate(across(c(mwh, mcf), ~ .x / mean(.x, na.rm = TRUE), .names = "adj_{.col}")) %>%
    rename(year_bin = year_built)

  # Filter and bin parcels
  ctu_binned <- parcel_data %>%
    filter(geog_name == .selected_ctu,
           mc_classification %in% c("sf_detached", "sf_attached")) %>%
    mutate(
      sqft_bin = bin_sqft(sq_ft_use),
      year_bin = bin_year(median_year)
    )

  # Join adjustment factors
  ctu_adj <- ctu_binned %>%
    left_join(sqft_adj, by = "sqft_bin") %>%
    left_join(age_adj, by = "year_bin") %>%
    mutate(
      base_mwh = case_when(
        mc_classification == "sf_detached" ~ eia_type$mwh[eia_type$housing_type == "Single-family detached"],
        mc_classification == "sf_attached" ~ eia_type$mwh[eia_type$housing_type == "Single-family attached"]
      ),
      base_mcf = case_when(
        mc_classification == "sf_detached" ~ eia_type$mcf[eia_type$housing_type == "Single-family detached"],
        mc_classification == "sf_attached" ~ eia_type$mcf[eia_type$housing_type == "Single-family attached"]
      ),
      adj_mwh = base_mwh * adj_mwh.x * adj_mwh.y,
      adj_mcf = base_mcf * adj_mcf.x * adj_mcf.y
    )

  # Summarize by housing type
  ctu_adj %>%
    group_by(mc_classification) %>%
    summarise(
      n_parcels = n(),
      median_sqft = median(sq_ft_use, na.rm = TRUE),
      median_year = median(median_year, na.rm = TRUE),
      adj_mwh = mean(adj_mwh, na.rm = TRUE),
      adj_mcf = mean(adj_mcf, na.rm = TRUE),
      .groups = "drop"
    )
}
