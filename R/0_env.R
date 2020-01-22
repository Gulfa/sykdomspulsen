#' Flags/values to be used in the 'dashboards' scene
#' @export config
config <- new.env()

config$computer_name <- "x"
config$production_name <- "smhb"
config$testing_name <- c("linux", "test", "temp")

config$is_production <- FALSE
config$is_testing <- FALSE
config$is_dev <- FALSE

config$is_initialized <- FALSE

#' Environment to store db connections
#' @export connections
connections <- new.env()
