#' @title Execute climate smart agriculture
#' @family agriculture
#'
#' @description This function generates the outputs of the agriculture module
#'    for decreasing cropland emissions from three strategies: smart fertilizer,
#'    cover crops, no-till. These are inter-related so run together here.
#'
#' @inheritParams run_module_agriculture
#' @inheritParams run_all_modules
#'
#'
#' @return [tibble::tibble()].
#'       Returns a table with columns `geog_name`, `geog_id`, `var`, `scen`, `year`, and `value`.
#'       The table is the output of the agriculture module, any modification to
#'       the inputs of the agriculture module must be specified as an argument
#'       to the function `run_scenario_building()`
#'
#' @export
#' @importFrom cli cli_progress_message
#' @examples
#'
#'

calculate_cropland_emissions <- function(fertilizer_tb = .fertilizer_tb,
                                   crops_tb = .crops_tb,
                                   # adjust_livestock_baseline = FALSE,
                                   .baseline_year = .baseline_year,
                                   .cropland_decrease_2050 = .cropland_decrease_2050,
                                   .selected_ctu = .selected_ctu,
                                   .scenario = "alt",
                                   .smart_fertilizer_start_year = .smart_fertilizer_start_year,
                                   .smart_fertilizer_current = .smart_fertilizer_current,
                                   .smart_fertilizer_goal = .smart_fertilizer_goal,
                                   # crops
                                   .cover_crops_start_year = .cover_crops_start_year,
                                   .cover_crops_current = .cover_crops_current,
                                   .cover_crops_goal = .cover_crops_goal,
                                   .no_till_start_year = .no_till_start_year,
                                   .no_till_current = .no_till_current,
                                   .no_till_goal = .no_till_goal
) {

  fertilizer_tb <- filter_ctu(fertilizer_tb, .selected_ctu = .selected_ctu)
  crops_tb <- filter_ctu(crops_tb, .selected_ctu = .selected_ctu)

  cropland_emissions <- filter_ctu(agricultural_emissions, .selected_ctu = .selected_ctu) %>%
    filter(category == "Cropland")

  cropland_bau <- if(.cropland_decrease_2050 == 0) cropland_emissions %>%
    mutate(scenario == "bau") else {
      adj_cropland_area(
        emissions = cropland_emissions,
        .cropland_decrease_2050 = .cropland_decrease_2050
      )
    }


  browser()

  ### adjust fertilizer emissions

  return(cropland_ouput)
}
