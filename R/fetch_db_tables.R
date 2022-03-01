#' Fetch scenario planning data tables
#'
#' @param uid character, your network id.
#'     Default is `getOption("councilR.uid")`. For example, `"mc\\rotenle"`
#' @param pwd character, your network password.
#'     Default is `getOption("councilR.pwd")`. For example, `"my_password"`
#' @param serv character, database server.
#'     Default is `"dbsqlcl11t.test.local,65414"` (the test database).
#' @param db character, database name. Default is `"CD_Emissions"`
#' @param module character, which module tables to pull. One of `"1"`, `"2"`,
#'     `"3"`, `"metro_demographic"`, or `"state_demographic"`.
#'
#' @description WARNING: Function error may results in RStudio crash.
#'
#' @note To make connection seamless, add `councilR.uid` and `councilR.pwd` to your `.Rprofile`. Edit your `.Rprofile` as such
#'
#'  ```
#'  options(
#'      ...,
#'      councilR.uid = "mc\\myuid",
#'      councilR.pwd = keyring::key_get("MetC"
#'  )
#'  ```
#'  See more details in [`{councilR}` documentation](https://github.com/Metropolitan-Council/councilR/blob/main/vignettes/Options.Rmd).
#'
#' @return a list of tables from the CD_Emissions database. List length depends
#'     `module` parameter.
#' @export
#'
#' @examples
#' \dontrun{
#' library(councilR)
#'
#' # set options if you haven't already
#' options(
#'   councilR.uid = "mc\\myuid",
#'   councilR.pwd = "mypwd"
#' )
#'
#' mod_1_tables <- fetch_db_tables(module = "1")
#' mod_2_tables <- fetch_db_tables(module = "2")
#' mod_3_tables <- fetch_db_tables(module = "3")
#'
#' # use purrr::map() to fetch all available tables
#'
#' library(purrr)
#'
#' purrr::map(
#'   c(
#'     "1", "2", "3",
#'     "metro_demographic",
#'     "state_demographic"
#'   ),
#'   function(x) {
#'     fetch_db_tables(module = x)
#'   }
#' )
#' }
#'
#' @importFrom DBI dbCanConnect dbGetQuery dbConnect dbDisconnect
#' @importFrom odbc odbc
#' @importFrom purrr map
#' @importFrom utils osVersion
fetch_db_tables <- function(uid = getOption("councilR.uid"),
                            pwd = getOption("councilR.pwd"),
                            module,
                            local = TRUE,
                            serv = "dbsqlcl11t.test.local,65414",
                            db = "CD_Emissions") {
  # browser()
  # decide which driver to use based on OS

  if (local == FALSE) {
    stop("Non-local isn't ready yet!")
  }

  drv <- if (grepl("mac", osVersion)) {
    "FreeTDS"
  } else {
    "SQL Server"
  }

  # check that DB connection works
  if (
    DBI::dbCanConnect(
      odbc::odbc(),
      Driver = drv,
      Database = db,
      Uid = uid,
      Pwd = pwd,
      Server = serv
    ) == FALSE) {
    stop("Database failed to connect")
  }

  if (module %in% c(
    "1",
    "land_use"
  )) {
    dbname <- "metro_sp_mod_1."

    tables <- c(
      "land_cover_types",
      "ctu_land_use_2016_land_cover",
      "land_use_2016_types",
      "vw_land_use_by_cover_type",
      "vw_ctu_forecast",
      "vw_ctu_land_use_hectares",
      "scenario_parameters",
      "vw_ctu_land_use_2016_land_cover"
    )
  } else if (module %in% c(
    "2",
    "building_energy"
  )) {
    dbname <- "metro_sp_mod_2."

    tables <- c(
      # "vw_ctu_forecast",
      "vw_ztrax_building_sqft",
      "vw_ztrax_sqft_summary_ctu",
      "ztrax_sqft_summary_county"
    )
  } else if (module %in% c(
    "3",
    "transportation"
  )) {
    dbname <- "metro_sp_mod_3."


    tables <- c(
      "aeo_factor",
      "aeo_scenario",
      "cost_factor",
      "ghg_factor",
      "mode",
      "pass_transpo",
      "sources",
      "variables"
    )
  } else if (module == "metro_demographic") {
    dbname <- "metro_demographic."

    tables <- c(
      "vw_ctu_county",
      "vw_ctu_population",
      "vw_qcew_ctu",
      "vw_housing_stock_ctu",
      "vw_emp_forecast_ctu",
      "vw_led_industry_county"
    )
  } else if (module == "state_demographic") {
    dbname <- "state_demographic."

    tables <- c(
      "county"
    )
  } else {
    stop("No matching `module`")
  }

  conn <- DBI::dbConnect(odbc::odbc(),
                         Driver = drv,
                         Database = db,
                         Uid = uid,
                         Pwd = pwd,
                         Server = serv
  )

  db_sp_tables <- purrr::map(
    tables,
    function(x) {
      DBI::dbGetQuery(
        conn,
        paste0("SELECT * FROM ", dbname, x)
      )
    }
  )

  names(db_sp_tables) <- tables

  DBI::dbDisconnect(conn)

  return(db_sp_tables)
}
