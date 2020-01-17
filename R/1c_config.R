set_config <- function() {
  progressr::handlers(progressr::progress_handler(
     format = "[:bar] :current/:total (:percent) in :elapsedfull, eta: :eta",
     clear = FALSE
  ))

  set_computer_name()
  set_computer_type()
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
}

set_computer_name <- function() {
  if (file.exists("/tmp/computer")) {
    con <- file("/tmp/computer", "r")
    name_computer <- readLines(con, n = 1)
    close(con)
  } else {
    name_computer <- "NO_NAME_FOUND"
  }
  Sys.setenv(COMPUTER = name_computer)
  config$name_computer <- name_computer
}

set_computer_type <- function() {
  if (config$name_computer %in% config$name_production) {
    config$is_production <- TRUE
  } else if (config$name_computer %in% config$name_testing) {
    config$is_testing <- TRUE
  } else {
    config$is_dev <- TRUE
  }
}

set_border <- function() {
  if (config$is_production) {
    config$border <- 2020
  } else {
    config$border <- 2020
  }
}
