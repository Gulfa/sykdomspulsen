#' This function gets the right folder for results
#' @param type input, output, or archive
#' @param tag Name of the second level file/folder
#' @param ... Third level and beyond
#' @export
path <- function(type="output", tag, ...) {
  stopifnot(type %in% c("input","output","archive"))

  fs::path("/",
    type,
    tag,
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
  from_folder <- path("output", results_folder_name, date)
  to_folder <- path("output", results_folder_name, "latest")
  processx::run("cp", c("-rT", from_folder, to_folder))
}
