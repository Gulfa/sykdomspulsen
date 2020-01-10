#' Flags/values to be used in the 'dashboards' scene
#' @export config
config <- new.env()

config$name_computer <- "x"
config$name_production <- "smhb"
config$name_testing <- c("linux", "test", "temp")

config$is_production <- FALSE
config$is_testing <- FALSE
config$is_dev <- FALSE

config$is_initialized <- FALSE

#' Environment to store db connections
#' @export connections
connections <- new.env()
