#' This function gets the right folder for results
#' @param results_folder_name name of the results folder
#' @param date the date of extraction
#' @export
path <- function(type="output", results_folder_name, date, ...) {
  fs::path("/",
    type,
    results_folder_name,
    date,
    ...
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
