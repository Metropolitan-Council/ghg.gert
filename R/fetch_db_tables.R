#' Fetch scenario planning data tables
#'
#' @param uid character, your network id.
#'     Default is `getOption("councilR.uid")`. For example, `"mc\\rotenle"`
#' @param pwd character, your network password.
#'     Default is `getOption("councilR.pwd")`. For example, `"my_password"`
#' @param serv character, database server.
#'     Default is `"dbsqlcl11t.test.local,65414"` (the test database).
#' @param db character, database name. Default is `"CD_Emissions"`
#' @param module character, which module tables to pull. One of `"mod_1"`, `"mod_2"`,
#'     `"mod_3"`,` "metro_demos"`,
#'     `"state_demos"`, `"metro_energy"`, `"state_energy"`, or `"all"`
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
#' mod_1_tables <- fetch_db_tables(module = "mod_1")
#' mod_2_tables <- fetch_db_tables(module = "mod_2")
#' mod_3_tables <- fetch_db_tables(module = "mod_3")
#'
#' # or fetch all tables
#'
#' all <- fetch_db_tables(module= "all")
#'
#' }
#'
#' @importFrom DBI dbCanConnect dbGetQuery dbConnect dbDisconnect
#' @importFrom odbc odbc
#' @importFrom purrr map flatten
#' @importFrom utils osVersion
fetch_db_tables <- function(uid = getOption("councilR.uid"),
                            pwd = getOption("councilR.pwd"),
                            module = c("mod_1", "mod_2", "mod_3",
                                       "metro_demos", "state_demos",
                                       "metro_energy", "state_energy",
                                       "all"),
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


  tables_to_fetch <- if(module == "all"){
    purrr::flatten(db_tables)
  } else {
    db_tables[[module]]
  }

  if(length(tables_to_fetch) == 0){
    stop("No matching module name")
  }

  conn <- DBI::dbConnect(odbc::odbc(),
                         Driver = drv,
                         Database = db,
                         Uid = uid,
                         Pwd = pwd,
                         Server = serv
  )

  db_sp_tables <- purrr::map(
    tables_to_fetch,
    function(x) {
      DBI::dbGetQuery(
        conn,
        paste0("SELECT * FROM ", x)
      )
    }
  )

  names(db_sp_tables) <- names(tables_to_fetch)

  DBI::dbDisconnect(conn)

  return(db_sp_tables)
}
