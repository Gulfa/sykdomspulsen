#' run_with_data
#'
#' Fetches data from DB with appropriate filters
#' Runs analysis and writes results back to DB
#'
#' @export
run_with_data <- function(r6_func, task_name, by_location=TRUE){

  x_filter <- task_config$filter
  data <- fd::tbl(task_config$db_table) %>%
    dplyr::filter(!!!x_filter) %>%
    dplyr::collect() %>%
    fd::latin1_to_utf8()

  if(by_location){
    for(loc in unique(data[, location_code])){
      analysis_config[["location_code"]] <- loc
      analysis_config[["data"]] <- data[location_code == loc]
      r6_func(data=data[location_code==loc])
      res <- do.call( analysis_func, task_config$args)
      write_to_db(res, table)
    }
  }else{
    do.call(analysis_func, task_config$args)
  }
}

run_task <- function(task_name, log=TRUE){

  task <- get(tc(task_name)$r6_func)$new(task_name = task_name)

  if(log == FALSE | task$can_run()){
    print(glue::glue("Running task {task_name}"))
    if(tc(task_name)[["type"]] %in% c("data", "analysis_function")){
      task$run()
    } else {
      run_with_data(
        task$run,
        task_name = task_name
        )
    }

    if(log){
      fd::update_rundate(
        package = task_name,
        date_results = lubridate::today(),
        date_extraction = lubridate::today(),
        date_run = lubridate::today()

      )
    }
  }else{
    print(glue::glue("Not runng {task_name}"))
  }

}


write_to_db <- function(res, table){
  if(!is.null(res)){
    res[["schema"]]$db_config <- config$db_config
    res[["schema"]]$db_connect()
    results_data <- res[["results"]]
    results_data[, source:=table]
    res[["schema"]]$db_upsert_load_data_infile(results_data)
  }
}


get_list <- function(l, key, default=NULL){
  if(key %in% names(l)){
    return(l[[key]])
  }else{
    return(default)
  }
}

#' Task Base Class
#' @import R6
#' @export
TaskBase <- R6::R6Class(
  "TaskBase",
  portable = FALSE,
  cloneable = TRUE,
  public = list(
    task_name = list(),
    initialize = function(task_name) {
      task_name <<- task_name
    },
    task_config = function(){
      tc(task_name)
    },
    can_run = function(){
      rundates <- fd::get_rundate()
      last_run_date <- head(rundates[package == task_name]$date_run, 1)
      curr_date <- lubridate::today()
      dependencies <- c()
      for(dependency in get_list(tc(task_name), "dependencies",default=c())){
        dep_run_date <- rundates[package==dependency]
        if(nrow(dep_run_date) == 0){
          return(FALSE)
        }
        if(length(last_run_date) >0){
          if(last_run_date >= dep_run_date[1, date_run]){
            return(FALSE)
          }
        }
      }
      if(length(last_run_date) == 0){
        return(TRUE)
      }
      if(curr_date <= last_run_date){
          return(FALSE)
      }


      return(TRUE)
    },
    run_with_catch = function() {
      print(class(self)[1])
      tryCatch({
        self$run()
      },
      error = function(e) {
        if (fd::config$is_production) {
          fd::msg("ERROR", slack = T)
          fd::msg(e, slack = T)
        } else {
          stop(e)
        }
      }
      )
    },
    run = function() {
      stop("run must be implemented")
    }
  )
)
