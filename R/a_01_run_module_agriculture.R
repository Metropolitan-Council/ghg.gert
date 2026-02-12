#' @title Execute agriculture module
#' @family agriculture
#'
#' @description This function generates the outputs of the agriculture module
#'    for the given scenario at the county/city/township level. It splits action into
#'    livestock, manure, and crop soils.
#'
#' @inheritParams scen_building_residential
#' @inheritParams scen_building_non_residential
#' @inheritParams calc_residential_renewable_ng
#' @inheritParams calc_ghg_non_residential
#' @inheritParams calc_energy_residential
#' @inheritParams calc_residential_retrofit
#' @inheritParams calc_housing_leed
#' @inheritParams adj_unit_counts
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
#'
run_module_agriculture <- function(livestock_tb = agriculture_activity_data$livestock,
                                  fertilizer_tb = agriculture_activity_data$fertilizer,
                                  crops_tb = agriculture_activity_data$crops,
                                  area_tb = agriculture_area,
                                  # adjust_livestock_baseline = FALSE,
                                  cropland_decrease_2050 = 0,
                                  #run_manure = TRUE, # currently no flag needed as it's preloaded df
                                  run_fertilizer = TRUE,
                                  run_crops = TRUE,
                                  .baseline_year = 2022,
                                  .ag_land_perserved = 1,
                                  .selected_ctu = "all",
                                  .scenario = "alt",
                                  # # manure
                                  # .manure_start_year = 2028,
                                  # .manure_handling = 0.0,
                                  # fertilizer
                                  .smart_fertilizer_start_year = 2028,
                                  .smart_fertilizer_current = 0.0,
                                  .smart_fertilizer_goal = 0.0,
                                  # crops
                                  .cover_crops_start_year = 2028,
                                  .cover_crops_current = 0.0,
                                  .cover_crops_goal = 0.0,
                                  .no_till_start_year = 2028,
                                  .no_till_current = 0.0,
                                  .no_till_goal = 0.0,
                                  # general start year for cover crops and no till
                                  .regen_ag_start_year = 2028
) {

  livestock_tb <- filter_ctu(livestock_tb, .selected_ctu = .selected_ctu)
  fertilizer_tb <- filter_ctu(fertilizer_tb, .selected_ctu = .selected_ctu)
  crops_tb <-filter_ctu(crops_tb, .selected_ctu = .selected_ctu)
  area_tb <- filter_ctu(area_tb, .selected_ctu = .selected_ctu)

  #convert input cropland area (acres to sq km)

  cropland_decrease_2050_km <- cropland_decrease_2050 * 0.00404686

  # check area reduction doesn't exceed area
  if(cropland_decrease_2050_km >
    area_tb %>% filter(inventory_year == 2050) %>%
    pull(area)) cli::cli_abort("Cropland area decrease exceeds current cropland area")


  # l_names <- c(
  #   # manure
  #   "manure_management_start_year",
  #   "manure_management",
  #   # fertilizer
  #   "smart_fertilizer_start_year",
  #   "smart_fertilizer_application",
  #   # crops
  #   "cover_crops_start_year",
  #   "cover_crops",
  #   "no_till_start_year",
  #   "no_till_agriculture"
  # )
  #
  # l_vals <- list(
  #   # manure
  #   .manure_start_year,
  #   .manure_handling,
  #   # fertilizer
  #   .smart_fertilizer_start_year ,
  #   .smart_fertilizer,
  #   # crops
  #   .cover_crops_start_year,
  #   .cover_crops,
  #   .no_till_start_year,
  #   .no_till
  # )
  #
  # purrr::map2(l_names, l_vals, check_inputs)


    manure <-
      calculate_manure_emissions(
        manure_caf = ghg.ccap::agriculture_manure_caf,
        .selected_ctu = .selected_ctu
      )

    cropland <-
      calculate_cropland_emissions(
        fertilizer_tb = fertilizer_tb,
        crops_tb = crops_tb,
        area_tb = area_tb,
        .baseline_year = .baseline_year,
        .cropland_decrease_2050 = cropland_decrease_2050_km,
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
        .no_till_goal = .no_till_goal,
        .regen_ag_start_year = .regen_ag_start_year
      )

  #browser()

  agriculture_module_output <- bind_rows(manure %>%
                                           mutate(geog_id = unique(cropland$geog_id)),
                                         cropland)


  return(agriculture_module_output)
}
