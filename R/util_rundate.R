#' update_rundate
#' Updates the rundate db tables
#' @param package a
#' @param date_extraction date when the data file was extracted
#' @param date_results the last date of the results
#' @param date_run the date when the analysis was run
#' @export
update_rundate <- function(package, date_extraction, date_results, date_run) {
  # date_extraction = date when the data file was extracted
  # date_results = the last date of the results
  # date_run = the date when the analysis was run
  field_types <- c(
    "package" = "TEXT",
    "date_extraction" = "DATE",
    "date_results" = "DATE",
    "date_run" = "DATE"
  )

  keys <- c(
    "package"
  )

  rundate <- schema$new(
    db_config = config$db_config,
    db_table = "rundate",
    db_field_types = field_types,
    db_load_folder = "/xtmp/",
    keys = keys,
    check_fields_match = TRUE
  )

  rundate$db_connect()

  to_upload <- data.table(
    package = package,
    date_extraction = as.Date(date_extraction),
    date_results = as.Date(date_results),
    date_run = as.Date(date_run)
  )

  rundate$db_upsert_load_data_infile(to_upload)
}

#' get_rundate
#' Gets the rundate db table
#' @export
get_rundate <- function() {
  conn <- get_db_connection()
  use_db(conn, "sykdomspuls")
  # on.exit(DBI::dbDisconnect(conn))

  if (!DBI::dbExistsTable(conn, "rundate")) {
    update_rundate(
      package = "xxxxx123456_fake",
      date_extraction = lubridate::today(),
      date_results = lubridate::today(),
      date_run = lubridate::today()
    )
  }

  temp <- dplyr::tbl(conn, "rundate") %>%
    dplyr::collect() %>%
    fd::latin1_to_utf8()
  return(temp)
}

#' greater_than_rundate
#' Checks to see if the rundate exists fora particular package
#' @param pkg Package
#' @export
exists_rundate <- function(pkg) {
  rd <- get_rundate()
  if (pkg %in% rd$package) {
    return(TRUE)
  }
  return(FALSE)
}
