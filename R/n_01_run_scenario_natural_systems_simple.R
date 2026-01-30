#' @title Run Simplified Natural Systems Scenario
#'
#' @description Simplified interface for natural systems carbon sequestration modeling.
#'   Uses a unified restoration approach with automatic source allocation, reducing
#'   the number of user inputs while maintaining scientific defensibility.
#'
#'   This function wraps the complexity of land cover conversion into three simple
#'   toggles (wetland/forest/prairie) and a single ambition slider. Source allocation
#'   happens automatically with a priority order that minimizes ecological impact.
#'
#' @param tb_inv Inventory data (list with $ctu, $county, $region)
#' @param tb_future Future projections data (list with $ctu, $county, $region)
#' @param tb_seq Sequestration rates by land cover type
#' @param .selected_ctu City/township selection ("all", "Regional", county name, or CTU name)
#'
#' @param .restore_wetland Logical, whether to restore wetlands (default FALSE)
#' @param .restore_forest Logical, whether to restore forests (default FALSE)
#' @param .restore_prairie Logical, whether to restore native prairies (default FALSE)
#' @param .restoration_ambition Numeric 0-100, how ambitious the restoration effort (default 50)
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
#' @return Dataframe with carbon sequestration projections by land cover type and year
#'
#' @export
#' @import dplyr
#' @import tidyr
#'
#' @examples
#' \dontrun{
#' # Simple restoration scenario
#' result <- run_scenario_natural_systems_simple(
#'   .selected_ctu = "Lakeville",
#'   .restore_wetland = TRUE,
#'   .restore_forest = TRUE,
#'   .restoration_ambition = 40
#' )
#'
#' # Combined with urban tree planting
#' result <- run_scenario_natural_systems_simple(
#'   .selected_ctu = "Minneapolis",
#'   .restore_wetland = TRUE,
#'   .restoration_ambition = 30,
#'   .community_tree_pct = 25
#' )
#' }
run_scenario_natural_systems_simple <- function(
    tb_inv = natural_systems_data$inventory,
    tb_future = natural_systems_data$projections,
    tb_seq = natural_systems_data$land_cover_carbon,
    .selected_ctu = "all",

    # Unified restoration module
    .restore_wetland = FALSE,
    .restore_forest = FALSE,
    .restore_prairie = FALSE,
    .restoration_ambition = 50,
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
  if (grepl("County", .selected_ctu)) {
    df_hist <- tb_inv$county %>% filter(geog_name == .selected_ctu)
    df_null <- tb_future$county %>% filter(geog_name == .selected_ctu)
  } else if (.selected_ctu == "Regional") {
    df_hist <- tb_inv$region
    df_null <- tb_future$region
  } else if (.selected_ctu == "all") {
    df_hist <- tb_inv$ctu
    df_null <- tb_future$ctu
  } else {
    df_hist <- tb_inv$ctu %>% filter(geog_name == .selected_ctu)
    df_null <- tb_future$ctu %>% filter(geog_name == .selected_ctu)
  }

  # ===========================================================================
  # Module 1: Unified Ecosystem Restoration (wetlands, forests, prairies)
  # ===========================================================================
  restoration_allocations <- NULL
  restoration_summary <- NULL

  if (.restore_wetland | .restore_forest | .restore_prairie) {
    tb01 <- ghg.ccap::restore_ecosystems(
      df_null = df_null,
      restore_wetland = .restore_wetland,
      restore_forest = .restore_forest,
      restore_prairie = .restore_prairie,
      ambition_pct = .restoration_ambition,
      start_yr = .restoration_start,
      end_yr = .restoration_end
    )

    # Store restoration metadata for potential use
    restoration_allocations <- attr(tb01, "restoration_allocations")
    restoration_summary <- attr(tb01, "restoration_summary")
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

  # Optionally attach restoration metadata as attributes
  if (!is.null(restoration_allocations)) {
    attr(carbon_sequestration_out, "restoration_allocations") <- restoration_allocations
  }
  if (!is.null(restoration_summary)) {
    attr(carbon_sequestration_out, "restoration_summary") <- restoration_summary
  }

  return(carbon_sequestration_out)
}


#' @title Get Natural Systems Restoration Potential for UI
#'
#' @description Convenience function to get restoration potential for a
#'   selected jurisdiction. Useful for populating UI elements that show
#'   users what's achievable.
#'
#' @param tb_future Future projections data
#' @param .selected_ctu City/township selection
#'
#' @return List with restoration potential values
#'
#' @export
get_ctu_restoration_potential <- function(
    tb_future = natural_systems_data$projections,
    .selected_ctu = "all"
) {

  # Select appropriate data based on geography
  if (grepl("County", .selected_ctu)) {
    df_null <- tb_future$county %>% filter(geog_name == .selected_ctu)
  } else if (.selected_ctu == "Regional") {
    df_null <- tb_future$region
  } else if (.selected_ctu == "all") {
    df_null <- tb_future$ctu
  } else {
    df_null <- tb_future$ctu %>% filter(geog_name == .selected_ctu)
  }

  get_restoration_potential(df_null)
}
