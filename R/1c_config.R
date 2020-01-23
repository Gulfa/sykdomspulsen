set_config <- function() {
  progressr::handlers(progressr::progress_handler(
     format = "[:bar] :current/:total (:percent) in :elapsedfull, eta: :eta",
     clear = FALSE
  ))

  set_computer_name()
  set_computer_type()
  set_dev_options()
  set_border()
  set_db()
  set_tasks()

  config$smallMunicips <- c(
    "municip1151",
    "municip1835",
    "municip1252",
    "municip1739"
  )
  config$AGES <- list(
    "Totalt" = c(0:105),
    "0-4" = c(0:4),
    "5-14" = c(5:14),
    "15-19" = c(15:19),
    "20-29" = c(20:29),
    "30-64" = c(30:64),
    "65+" = c(65:105)
  )

  # if(!foreach::getDoParRegistered()){
  #   future::plan(future::sequential)
  #   foreach::registerDoSEQ()
  # }
}

set_computer_name <- function() {
  if (file.exists("/tmp/computer")) {
    con <- file("/tmp/computer", "r")
    computer_name <- readLines(con, n = 1)
    close(con)
  } else {
    computer_name <- "NO_NAME_FOUND"
  }
  Sys.setenv(COMPUTER = computer_name)
  config$computer_name <- computer_name
}

set_computer_type <- function() {
  if (config$computer_name %in% config$production_name) {
    config$is_production <- TRUE
  } else if (config$computer_name %in% config$name_testing) {
    config$is_testing <- TRUE
  } else {
    config$is_dev <- TRUE
  }
}

set_dev_options <- function(){
  # if(config$computer_name == "gunr"){
  #   options(error = function() traceback())
  # }
}

set_border <- function() {
  if (config$is_production) {
    config$border <- 2020
  } else {
    config$border <- 2020
  }
}


