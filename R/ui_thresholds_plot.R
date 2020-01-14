
#' analyse_simple
#'
#' Get and clean MSIS data from msis.no 
#'
#' @import ggplot2
#' 
#' @export
UI_create_threshold_plot <-  function(data, argset, schema){
  folder <- argset$folder
  filename <- argset$filename
  q <- ggplot(data$data) + geom_col(aes(x=date, y=n)) + geom_line(aes(x=date, y=n_threshold_0)) +
    fhiplot::theme_fhi_lines()
  folder <- fd::path(inside="results", do.call(glue::glue, c(folder, argset)), package="ui")
  fs::dir_create(folder)
  date <- "2018-02-32"
  ggsave(fs::path("/", folder, do.call(glue::glue, c(filename, argset))),
         plot=q, width=8, height=5)
}
