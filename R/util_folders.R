
#' Dashboard folders
#'
#' This function finds folders according to the dashboard philosophy
#' @param inside where it is inside
#' @param ... an optional path/file
#' @param package The name of the package
#' @export
path <- function(inside = "data_raw", ..., package = config$package) {
  fs::path("/", inside, package, ...)
}

#' Results folder
#'
#' This function gets the right folder for results
#' @param results_folder_name name of the results folder
#' @param date the date of extraction
#' @export
results_folder <- function(results_folder_name, date) {
  fd::path(
    "results",
    results_folder_name,
    date
  )
}

#' Create latest folder
#'
#' This function copies results_folder/date til results_folder/latest
#' @param results_folder_name name of the results folder
#' @param date the date of extraction
#' @export
create_latest_folder <- function(results_folder_name, date) {
  from_folder <- results_folder(results_folder_name, date)
  to_folder <- fd::path("results", results_folder_name, "latest")
  processx::run("cp", c("-rT", from_folder, to_folder))
}
