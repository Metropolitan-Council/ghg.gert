#' @title Calculate Land Cover by City/Township
#' @family land_use_module
#'
#' @description `calc_land_cover_by_city()` takes the output of the function 'calc_land_cover_by_land_use()' and
#' calculates the total land cover (hectares) by type for each community.
#'
#' @inheritParams calc_land_cover_by_land_use
#'
#' @return
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ghg.sp::calc_land_cover_by_city(
#'     tb = land_use_data,
#'     .luse_scen = "compact_dev_with_drs",
#'     .scenario = "bau")
#' }
calc_land_cover_by_city <- function(tb,
                                    .luse_scen,
                                    .scenario) {
  calc_land_cover_by_land_use(tb = tb,
                              .luse_scen = .luse_scen,
                              .scenario = .scenario) %>%
    group_by(ctu_name, year, land_cover_description_2) %>%
    summarise(land_cover_hectares = sum(land_cover_land_use_hectares))
}
