#' Fetch scenario planning data tables
#'
#' @param uid character, your network id. For example, `"mc\\rotenle"`
#' @param pwd character, your network password. For example, `"my_password"`
#' @param serv character, database server.
#'     Default is `"dbsqlcl11t.test.local,65414"` (the test database).
#' @param db character, datbase name. Default is `"CD_Emissions"`
#'
#' @description WARNING: Function error may results in RStudio crash.
#'
#' @return a list of tables from the CD_Emissions database
#' @export
#'
#' @importFrom DBI dbCanConnect dbGetQuery dbConnect dbDisconnect
#' @importFrom odbc odbc
#' @importFrom purrr map
#' @importFrom utils osVersion
fetch_db_tables <- function(uid,
                            pwd,
                            serv = "dbsqlcl11t.test.local,65414",
                            db = "CD_Emissions") {
  # browser()
  # decide which driver to use based on OS
  drv <- if (grepl("mac", osVersion)) {
    "FreeTDS"
  } else {
    "SQL Server"
  }

  # check that DB connection works
  if (DBI::dbCanConnect(odbc::odbc(),
    Driver = drv,
    Database = db,
    Uid = uid,
    Pwd = pwd,
    Server = serv
  )) {
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
          paste0("SELECT * FROM metro_sp_mod_3.", x)
        )
      }
    )

    names(db_sp_tables) <- tables

    DBI::dbDisconnect(conn)

    return(db_sp_tables)
  } else {
    stop("Database failed to connect")
  }
}
