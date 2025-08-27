#' Calculate GHG emissions from municipal solid waste sent to landfills using waste composition.
#'
#' @param waste_inv table, waste inventory data
#' @param waste_future table, projected waste data
#' @param waste_char table, output of 01_mpca_waste_characterization.R
#' @param .methane_recovery_pct single value, percentage of landfills using methane recovery
#' @param .methane_recovery_start single value, year when methane recovery starts
#' @param .methane_recovery_end single value, year when methane recovery ends
#' @return a list containing two data tables with geoid, source, inventory_year, value_activity,
#' units_activity, value_emissions, and units_emissions
#' @export
calculate_landfill_emissions <- function(waste_inv,
                                         waste_future,
                                         waste_char,
                                         .methane_recovery_pct = 0,
                                         .methane_recovery_start = 2025,
                                         .methane_recovery_end = 2050) {



  # create empty methane recovery df
  inventory_year = unique(waste_future$inventory_year)
  methane_recovery_table = tibble::tibble(
    inventory_year, percent_recovered = rep(.methane_recovery_pct, length(inventory_year))
    )

  # now let's create a table but where the percentage increases linearly over time
  # between methane_recovery_start and methane_recovery_end. So if methane_recovery_pct == 0.5,
  # and methane_recovery_start == 2025, and methane_recovery_end == 2050, then
  # the percentage will be 0 in 2025 and will increase linearly to 0.5 by the year 2050

  if (.methane_recovery_pct > 0) {
    methane_recovery_table <- methane_recovery_table %>%
      dplyr::mutate(
        percent_recovered = dplyr::case_when(
          inventory_year < .methane_recovery_start ~ 0,
          inventory_year >= .methane_recovery_start & inventory_year <= .methane_recovery_end ~
            (.methane_recovery_pct / (.methane_recovery_end - .methane_recovery_start)) * (inventory_year - .methane_recovery_start),
          TRUE ~ .methane_recovery_pct
        )
      )
  }


  # methane correction factor (MCF)
  # assuming landfills managed well, semi-aerobic (see GHG Protocol)
  mcf <- 0.5
  # fraction of degradable organic carbon degraded (DOC_f)
  doc_f <- 0.6
  # fraction of methane in landfill gas (F)
  f <- 0.5
  # oxidation factor (OX)
  ox <- 0.1 # for well-managed landfills


  # Calculate DOC using IPCC equation (see documentation)
  # waste composition from MPCA report https://www.pca.state.mn.us/sites/default/files/w-sw1-60.pdf
  # cleaned in _waste/data-raw/clean_tabula_tables.R

  ipcc_doc_factors <- tibble::tibble(
    Category = c("Paper", "Textiles", "Organics (Non-Food)", "Organics (Food)", "Wood"),
    Factor = c(0.4, 0.4, 0.17, 0.15, 0.3)
  )

  doc_sum <- waste_char %>%
    dplyr::inner_join(ipcc_doc_factors, by = dplyr::join_by(Category)) %>%
    dplyr::mutate(doc_content = Mean * Factor) %>%
    dplyr::summarize(doc_total = sum(doc_content), degradable_fraction = sum(Mean))

  doc <- doc_sum$doc_total

  # methane generation potential
  l_0 <- mcf * doc * doc_f * f * 16 / 12

  landfill_emissions <- list()

  landfill_emissions$inv <- waste_inv %>%
    dplyr::filter(source == "Landfill") %>%
    dplyr::mutate(
      value_emissions = (value_activity * l_0) * (1 - ox),
      units_emissions = "Metric tons CH4"
    )
# browser()

  landfill_emissions$future <- waste_future %>%
    dplyr::filter(source == "Landfill") %>%
    dplyr::left_join(methane_recovery_table, by = dplyr::join_by(inventory_year)) %>%
    dplyr::mutate(
      value_emissions = (value_activity * l_0) * (1 - ox) * (1-percent_recovered),
      units_emissions = "Metric tons CH4"
    ) %>%
    dplyr::select(
      -percent_recovered
    )

  return(landfill_emissions)
}
