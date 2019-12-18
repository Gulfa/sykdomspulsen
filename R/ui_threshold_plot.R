
#' analyse_simple
#'
#' Get and clean MSIS data from msis.no 
#'
#' @import ggplot2
#' 
#' @export
ui_create_threshold_plot <- R6::R6Class(
  "ui_create_threshold_plot",
  inherit = TaskBase,
  portable = FALSE,
  cloneable = FALSE,
  list(
    run = function(data, filename, folder, location_code){
      q <- ggplot(data) + geom_col(aes(x=date, y=n)) + geom_line(aes(x=date, y=n_threshold_0)) +
        fhiplot::theme_fhi_lines()
      date <- "2019-12-17"
      folder <- fd::path(inside="results", glue::glue(folder), package="ui")
      fs::dir_create(folder)
      ggsave(fs::path("/", folder, glue::glue(filename)), plot=q, width=8, height=5)
    })
)
  
