#' Calculate waste activity diverted to recycling
#'
#' @param waste_tb table, waste activity data
#' @param .diverted_to_recycle_pct single value, percent of solid waste that's diverted to recycling
#' @param .diverted_to_organics_pct single value, percent of solid waste that's diverted to organics
#' @param .diverted_to_wte_pct single value, percent of solid waste that's diverted to waste-to-energy
#' @param .diverted_to_recycle_start single value, year to start source diversion (recycling)
#' @param .diverted_to_organics_start single value, year to start source diversion (organics)
#' @param .diverted_to_wte_start single value, year to start source diversion (waste-to-energy)
#' @param .diverted_to_recycle_end single value, year to end source diversion (recycling)
#' @param .diverted_to_organics_end single value, year to end source diversion (organics)
#' @param .diverted_to_wte_end single value, year to end source diversion (waste-to-energy)
#' @return a data table with geoid, source, inventory_year, value_activity,
#' units_activity, value_emissions, and units_emissions
#' @export
#'
#'
#'
divert_waste <- function(waste_tb,
                         .diverted_to_recycle_pct = 0,
                         .diverted_to_organics_pct = 0,
                         .diverted_to_wte_pct = 0,
                         .diverted_to_recycle_start = 2025,
                         .diverted_to_recycle_end = 2050,
                         .diverted_to_organics_start = 2025,
                         .diverted_to_organics_end = 2050,
                         .diverted_to_wte_start = 2025,
                         .diverted_to_wte_end = 2050) {

  browser()
  # Input checks
  all_pcts <- c(.diverted_to_recycle_pct, .diverted_to_organics_pct, .diverted_to_wte_pct)
  if (any(!is.numeric(all_pcts)) || any(all_pcts < 0) || any(all_pcts > 100)) {
    stop("All diverted percentage inputs must be numeric values between 0 and 100.")
  }
  if (sum(all_pcts) > 100) {
    stop("Total diversion percentage (recycling + organics + WTE) cannot exceed 100%.")
  }

  # Get unique years
  inventory_years <- unique(waste_tb$inventory_year)

  # Calculate current diversion percentages per stream at start years
  get_current_pct <- function(df, stream, start_year) {
    df %>%
      filter(inventory_year == start_year) %>%
      group_by(inventory_year) %>%
      mutate(total_activity = sum(value_activity, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(source == stream) %>%
      summarise(
        pct = ifelse(total_activity[1] > 0, sum(value_activity, na.rm = TRUE) / total_activity[1] * 100, 0)
      ) %>%
      pull(pct)
  }


  recycle_current <- get_current_pct(waste_tb, "Recycling", .diverted_to_recycle_start)
  organics_current <- get_current_pct(waste_tb, "Organics", .diverted_to_organics_start)
  wte_current <- get_current_pct(waste_tb, "Waste to energy", .diverted_to_wte_start)

  # Build the projections table for each diversion stream
  build_projection <- function(start, end, pct, current) {
    tibble(inventory_year = inventory_years) %>%
      mutate(projected = case_when(
        inventory_year < start ~ current,
        inventory_year >= start & inventory_year <= end ~
          current + ((pct - current) / (end - start)) * (inventory_year - start),
        inventory_year > end ~ pct,
        TRUE ~ 0
      ))
  }

  proj_recycle <- build_projection(.diverted_to_recycle_start, .diverted_to_recycle_end, .diverted_to_recycle_pct, recycle_current)
  proj_organics <- build_projection(.diverted_to_organics_start, .diverted_to_organics_end, .diverted_to_organics_pct, organics_current)
  proj_wte <- build_projection(.diverted_to_wte_start, .diverted_to_wte_end, .diverted_to_wte_pct, wte_current)

  # Merge projections
  projections_table <- proj_recycle %>%
    rename(recycle = projected) %>%
    left_join(proj_organics %>% rename(organics = projected), by = "inventory_year") %>%
    left_join(proj_wte %>% rename(wte = projected), by = "inventory_year") %>%
    mutate(total = recycle + organics + wte) %>%
    mutate(across(c(recycle, organics, wte), ~ if_else(total > 100, . * (100 / total), .))) %>%
    select(-total)

  # Get total waste per year
  base <- waste_tb %>%
    group_by(inventory_year) %>%
    mutate(total_activity = sum(value_activity, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(source %in% c("Landfill", "Recycling", "Organics", "WasteToEnergy")) %>%
    pivot_wider(names_from = source, values_from = value_activity, values_fill = 0)

  # Join projections
  base <- base %>%
    left_join(projections_table, by = "inventory_year") %>%
    mutate(
      desired_recycle = recycle / 100 * total_activity,
      desired_organics = organics / 100 * total_activity,
      desired_wte = wte / 100 * total_activity
    ) %>%
    mutate(
      delta_recycle = desired_recycle - Recycling,
      delta_organics = desired_organics - Organics,
      delta_wte = desired_wte - WasteToEnergy,
      total_delta = delta_recycle + delta_organics + delta_wte,
      Landfill = Landfill - total_delta,
      Recycling = Recycling + delta_recycle,
      Organics = Organics + delta_organics,
      WasteToEnergy = WasteToEnergy + delta_wte
    ) %>%
    pivot_longer(cols = c("Landfill", "Recycling", "Organics", "WasteToEnergy"), names_to = "source", values_to = "value_activity") %>%
    dplyr::select(inventory_year, source, value_activity) %>%
    left_join(dplyr::select(waste_tb, -value_activity), by = c("inventory_year", "source"))

  return(base)
}







# divert_to_recycling <- function(waste_tb,
#                                 .diverted_to_recycle_pct = 0,
#                                 .diverted_to_recycle_start = 2025,
#                                 .diverted_to_recycle_end = 2050) {
#
#
#   # Input checks
#   if (!is.numeric(.diverted_to_recycle_pct) || .diverted_to_recycle_pct < 0 || .diverted_to_recycle_pct > 100) {
#     stop(".diverted_to_recycle_pct must be a number between 0 and 100.")
#   }
#
#
#   # Return the current percentage of waste that's diverted to recycling
#   diverted_to_recycle_current <- waste_tb %>%
#     group_by(inventory_year) %>%
#     mutate(
#       total_activity = sum(value_activity, na.rm = TRUE),
#       pct_of_total = value_activity / total_activity * 100
#     ) %>%
#     ungroup() %>%
#     filter(source %in% c("Landfill","Recycling")) %>%
#     pivot_wider(
#       names_from = source,
#       values_from = c(value_activity, pct_of_total),
#       values_fill = 0
#     ) %>%
#     # calculate the current percentage of Recycling at the start of change
#     filter(inventory_year == .diverted_to_recycle_start) %>%
#     pull(pct_of_total_Recycling)
#
#
#   # create empty methane recovery df
#   inventory_year = unique(waste_tb$inventory_year)
#   projections_table = tibble::tibble(
#     inventory_year, percent_diverted_to_recycling = rep(.diverted_to_recycle_pct, length(inventory_year))
#     )
#
#
#   # now let's create a table but where the percentage increases linearly over time
#   # between start and end year. So if pct change == 50,
#   # and start == 2025, and end == 2050, then
#   # the percentage will be 0 in 2025 and will increase linearly to 50 by the year 2050.
#   # However, if the percentage that's already diverted is to recycling is greater than 0
#   # then we will start from that percentage and increase from there.
#   # finally, we need to make sure that the final value that is reached is maintained
#   # through the end of the dataset
#
#   if (.diverted_to_recycle_pct > 0) {
#     projections_table <- projections_table %>%
#       dplyr::mutate(
#         percent_diverted_to_recycling = dplyr::case_when(
#           inventory_year < .diverted_to_recycle_start ~ diverted_to_recycle_current,
#           inventory_year >= .diverted_to_recycle_start & inventory_year <= .diverted_to_recycle_end ~
#             (diverted_to_recycle_current + (.diverted_to_recycle_pct / (.diverted_to_recycle_end - .diverted_to_recycle_start)) * (inventory_year - .diverted_to_recycle_start)),
#           # when inventory_year is greater than the end of the diverted to recycle period
#           # use the final value of the diverted to recycle percentage
#           inventory_year > .diverted_to_recycle_end ~
#             dplyr::if_else(diverted_to_recycle_current == 0,
#                            0,
#                            diverted_to_recycle_current + .diverted_to_recycle_pct),
#
#           TRUE ~ .diverted_to_recycle_pct
#         )
#       )
#   }
#
#
#
#
#   waste_proj <- waste_tb %>%
#     group_by(inventory_year) %>%
#     mutate(
#       total_activity = sum(value_activity, na.rm = TRUE),
#       pct_of_total = value_activity / total_activity * 100
#     ) %>%
#     ungroup() %>%
#     left_join(projections_table, by = "inventory_year") %>%
#     filter(source %in% c("Landfill","Recycling")) %>%
#     pivot_wider(
#       names_from = source,
#       values_from = c(value_activity, pct_of_total),
#       values_fill = 0
#     ) %>%
#     mutate(
#       change_in_Landfill = case_when(
#         # if percent_diverted_to_recycling is less than the current percentage
#         # that Recycling is already at, then we don't change the value_activity
#         percent_diverted_to_recycling <= pct_of_total_Recycling ~ 0,
#         # otherwise, we calculate the change based on the percentage diverted
#         TRUE ~ -(percent_diverted_to_recycling - pct_of_total_Recycling) / 100 * total_activity
#       ),
#       change_in_Recycling = -change_in_Landfill,
#       value_activity_Landfill = value_activity_Landfill + change_in_Landfill,
#       value_activity_Recycling = value_activity_Recycling + change_in_Recycling
#     ) %>%
#     dplyr::select(-c(change_in_Landfill, change_in_Recycling,
#                      total_activity, percent_diverted_to_recycling,
#                      pct_of_total_Landfill, pct_of_total_Recycling)) %>%
#     # remove "value_activity_" prefix from column names
#     rename_with(~ gsub("value_activity_", "", .), starts_with("value_activity_")) %>%
#     pivot_longer(
#       cols = c("Landfill", "Recycling"),
#       names_to = "source",
#       values_to = "value_activity"
#     ) %>% relocate(c(source, value_activity), .before="units_activity")
#
#
#   waste_proj <- waste_tb %>%
#     filter(!source %in% c("Landfill", "Recycling")) %>%
#     bind_rows(waste_proj) %>%
#     arrange(inventory_year, source)
#
#
#   # Return the modified waste activity table
#   return(waste_proj)
#
# }
