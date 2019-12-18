#' run_with_data
#'
#' Fetches data from DB with appropriate filters
#' Runs analysis and writes results back to DB
#'
#' @export
run_with_data <- function(table, filter, analysis_func,analysis_config, by_location=TRUE){

  data <- fd::tbl(table) %>% dplyr::filter(!!!filter) %>% dplyr::collect()
  setDT(data)
  if(by_location){
    for(loc in unique(data[, location_code])){
      analysis_config[["location_code"]] <- loc
      analysis_config[["data"]] <- data[location_code == loc]
      res <- do.call( analysis_func, analysis_config)
      write_to_db(res, table)
    }
  }else{
    do.call(analysis_func, analysis_config)

  }
  


}

run_task <- function(task_name, log=TRUE){
  config <- get_config()

  task_config <- config[["tasks"]][[task_name]]

  task <- get(task_config$func)$new()

  if(log == FALSE | task$can_run(task_name, task_config)){
    print(glue::glue("Running task {task_name}"))  
    if(task_config[["type"]] == "data" | task_config[["type"]] == "analysis_function"){
      func <- get(task_config$func)
      do.call(task$run, task_config[["args"]])
      
    }else{
      run_with_data(task_config[["db_table"]], task_config[["filter"]],
                    task$run, task_config[["args"]])
    }
    
    if(log){
      fd::update_rundate(
        package = task,
        date_results = rundate[package == "sykdomspuls"]$date_results,
        date_run = lubridate::today()
      )
    }
  }

}


write_to_db <- function(res, table){
  if(!is.null(res)){
    res[["schema"]]$db_config <- get_config()[["db_config"]]
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
  list(
    can_run = function(task, config){
      rundates <- fd::get_rundate()
      last_run_date <- head(rundates[package == task]$date_run, 1)
      curr_date <- lubridate::today()
      if(length(last_run_date)> 0){
        if(curr_date < last_run_date){
          return(FALSE)
        }
      }
      
      dependencies <- c()
      for(dependency in get_list(config, "dependencies",default=c())){
        dep_run_date <- rundates[package==dependency]
        if(nrow(dep_run_date) == 0){
          return(FALSE)
        }
        if(last_run_date >= dep_run_date$run_date){
          return(FALSE)
        }
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
    run_all = function() {
      stop("run_all must be implemented")
    }
  )
)
