
#' analyse_simple
#'
#' Get and clean MSIS data from msis.no 
#'
#' @import ggplot2
#' 
#' @export
UICreateThresholdPlot <- R6::R6Class(
  "UICreateThresholdPlot",
  inherit = TaskBase,
  portable = FALSE,
  cloneable = FALSE,
  list(
    run = function(data, data_plan, analysis_plan){
      folder <- analysis_plan$folder
      filename <- analysis_plan$filename
      q <- ggplot(data$data) + geom_col(aes(x=date, y=n)) + geom_line(aes(x=date, y=n_threshold_0)) +
        fhiplot::theme_fhi_lines()
      date <- "2019-12-17"
      folder <- fd::path(inside="results", do.call(glue::glue, c(folder, analysis_plan)),package="ui")
      fs::dir_create(folder)
      ggsave(fs::path("/", folder, do.call(glue::glue, c(filename, analysis_plan))),
                                          plot=q, width=8, height=5)
    })
)
  
