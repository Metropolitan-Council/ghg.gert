#' @title Calculate Land Cover by City/Township
#' @family land use
#'
#' @description  takes the output of the function
#'      `calc_land_cover_by_land_use()` and
#'      calculates the total land cover (hectares) by type for each community.
#'
#' @inheritParams calc_land_cover_by_land_use
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' calc_land_cover_by_city(
#'   tb = land_use_data,
#'   .urban_form_scenario = "bau"
#' )
#' }
calc_land_cover_by_city <- function(tb,
                                    .urban_form_scenario) {
  calc_land_cover_by_land_use(
    tb = tb,
    .urban_form_scenario = .urban_form_scenario
  ) %>%
    group_by(ctu_name, year, land_cover_description_2) %>%
    summarise(land_cover_hectares = sum(land_cover_land_use_hectares))
}
