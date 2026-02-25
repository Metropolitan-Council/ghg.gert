#' @title Run Simplified Natural Systems Scenario
#'
#' @description Simplified interface for natural systems carbon sequestration modeling.
#'   Wetlands use GIS-constrained restoration with ambition percentage.
#'   Forests and prairies allow direct acreage specification for planner flexibility.
#'
#'   This function wraps the complexity of land cover conversion into intuitive inputs:
#'   - Wetlands: toggle + ambition slider (GIS-validated)
#'   - Forests/Prairies: direct acreage in sq km (flexible, planner-specified)
#'   - Community trees: percent of developed area (urban canopy)
#'   - Pocket prairies: percent of urban grassland (urban habitat)
#'
#' @param tb_inv Inventory data (list with $ctu, $county, $region)
#' @param tb_future Future projections data (list with $ctu, $county, $region)
#' @param tb_seq Sequestration rates by land cover type
#' @param .selected_ctu City/township selection ("all", "Regional", county name, or CTU name)
#'
#' @param .restore_wetland Logical, whether to restore wetlands (default FALSE)
#' @param .wetland_ambition Numeric 0-100, percent of wetland potential to realize (default 50)
#' @param .forest_area_sqkm Numeric, committed forest restoration area in sq km (default 0)
#' @param .prairie_area_sqkm Numeric, committed prairie/grassland restoration area in sq km (default 0)
#' @param .restoration_start Start year for restoration (default 2025)
#' @param .restoration_end End year for restoration (default 2050)
#'
#' @param .community_tree_pct Percent of developed area for community trees (0-100, default 0)
#' @param .community_tree_start Start year for community tree planting (default 2025)
#' @param .community_tree_end End year for community tree planting (default 2050)
#'
#' @param .pocket_prairie_pct Percent of urban grassland for pocket prairies (0-100, default 0)
#' @param .pocket_prairie_start Start year for pocket prairies (default 2025)
#' @param .pocket_prairie_end End year for pocket prairies (default 2050)
#'
#' @param .enviro_factors Environmental factors data
#' @param detail Logical, return detailed output (default FALSE)
#'
#' @return Dataframe with carbon sequestration projections by land cover type and year.
#'   Includes attributes for restoration metadata and validation warnings.
#'
#' @export
#' @importFrom dplyr filter select case_when across bind_rows cur_column
#' @import tidyr
#'
#' @examples
#' \dontrun{
#' # Wetland restoration at 40% ambition + 5 sq km forest
#' result <- run_scenario_natural_systems(
#'   .selected_ctu = "Lakeville",
#'   .restore_wetland = TRUE,
#'   .wetland_ambition = 40,
#'   .forest_area_sqkm = 5
#' )
#'
#' # Check for validation warnings
#' attr(result, "validation_warnings")
#'
#' # Direct forest and prairie specification
#' result <- run_scenario_natural_systems(
#'   .selected_ctu = "Minneapolis",
#'   .forest_area_sqkm = 10,
#'   .prairie_area_sqkm = 5,
#'   .community_tree_pct = 25
#' )
#' }
run_scenario_natural_systems <- function(
    tb_inv = bind_rows(ghg.ccap::natural_systems_data$inventory),
    tb_future = bind_rows(ghg.ccap::natural_systems_data$projections),
    tb_seq = ghg.ccap::natural_systems_data$land_cover_carbon,
    .selected_ctu = "Regional",

    # Wetland restoration (GIS-constrained)
    .restore_wetland = FALSE,
    .wetland_ambition = 50,

    # Forest and prairie restoration (direct acreage)
    .forest_area_sqkm = 0,
    .prairie_area_sqkm = 0,

    # Restoration timing
    .restoration_start = 2025,
    .restoration_end = 2050,

    # Community tree planting (urban - separate from restoration)
    .community_tree_pct = 0,
    .community_tree_start = 2025,
    .community_tree_end = 2050,

    # Pocket prairies - urban grassland upgrade (separate from restoration)
    .pocket_prairie_pct = 0,
    .pocket_prairie_start = 2025,
    .pocket_prairie_end = 2050,

    .enviro_factors = ghg.ccap::enviro_factors,
    detail = FALSE
) {

  # ===========================================================================
  # Select appropriate data based on geography
  # ===========================================================================

  df_hist <- ghg.ccap::filter_ctu(tb_inv, .selected_ctu = .selected_ctu)
  df_null <- ghg.ccap::filter_ctu(tb_future, .selected_ctu = .selected_ctu)

  # ===========================================================================
  # Module 1: Ecosystem Restoration (wetlands, forests, prairies)
  # ===========================================================================
  restoration_allocations <- NULL
  restoration_summary <- NULL
  validation_warnings <- NULL
  validation_info <- NULL

  has_restoration <- .restore_wetland | .forest_area_sqkm > 0 | .prairie_area_sqkm > 0

  if (has_restoration) {
    tb01 <- ghg.ccap::restore_ecosystems(
      df_null = df_null,
      restore_wetland = .restore_wetland,
      wetland_ambition_pct = .wetland_ambition,
      forest_area_sqkm = .forest_area_sqkm,
      prairie_area_sqkm = .prairie_area_sqkm,
      start_yr = .restoration_start,
      end_yr = .restoration_end
    )

    # Store restoration metadata
    restoration_allocations <- attr(tb01, "restoration_allocations")
    restoration_summary <- attr(tb01, "restoration_summary")
    validation_warnings <- attr(tb01, "validation_warnings")
    validation_info <- attr(tb01, "validation_info")
  } else {
    tb01 <- df_null
  }

  # ===========================================================================
  # Module 2: Community Tree Planting (urban)
  # ===========================================================================
  if (.community_tree_pct > 0) {
    tb02 <- ghg.ccap::plant_community_trees(
      df_null = tb01,
      start_yr = .community_tree_start,
      end_yr = .community_tree_end,
      area_pct = .community_tree_pct
    )
  } else {
    tb02 <- tb01
  }

  # ===========================================================================
  # Module 3: Pocket Prairies (urban grassland → grassland upgrade)
  # ===========================================================================
  if (.pocket_prairie_pct > 0) {
    tb03 <- ghg.ccap::plant_pocket_prairies(
      df_null = tb02,
      start_yr = .pocket_prairie_start,
      end_yr = .pocket_prairie_end,
      area_pct = .pocket_prairie_pct
    )
  } else {
    tb03 <- tb02
  }

  # ===========================================================================
  # Calculate carbon sequestration and stock potential
  # ===========================================================================
  carbon_sequestration_out <- rbind(df_hist, tb03) %>%
    dplyr::arrange(inventory_year, land_cover_type) %>%
    dplyr::left_join(tb_seq, by = c("land_cover_type")) %>%
    dplyr::mutate(
      value_emissions = area * seq_mtco2e_sqkm,
      value_stock_potential = area * stock_mtco2e_sqkm
    )

  # ===========================================================================
  # Attach metadata as attributes for UI consumption
  # ===========================================================================
  if (!is.null(restoration_allocations)) {
    attr(carbon_sequestration_out, "restoration_allocations") <- restoration_allocations
  }
  if (!is.null(restoration_summary)) {
    attr(carbon_sequestration_out, "restoration_summary") <- restoration_summary
  }
  if (!is.null(validation_warnings)) {
    attr(carbon_sequestration_out, "validation_warnings") <- validation_warnings
  }
  if (!is.null(validation_info)) {
    attr(carbon_sequestration_out, "validation_info") <- validation_info
  }

  return(carbon_sequestration_out)
}


#' @title Get Natural Systems Restoration Potential for UI
#'
#' @description Convenience function to get restoration potential and validation
#'   limits for a selected jurisdiction. Useful for populating UI elements that
#'   show users what's achievable and setting input constraints.
#'
#' @param tb_future Future projections data
#' @param .selected_ctu City/township selection
#'
#' @return List with restoration potential values and validation limits
#'
#' @export
get_ctu_restoration_potential <- function(
    tb_future = bind_rows(ghg.ccap::natural_systems_data$projections),
    .selected_ctu = "Regional"
) {

  df_null <- ghg.ccap::filter_ctu(tb_future, .selected_ctu = .selected_ctu)

  get_restoration_potential(df_null)
}
