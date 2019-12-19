task_config <- function(task_name){
  config$tasks[[task_name]]
}

task_plan <- function(task_name){
  task_config(task_name)$plan_func()
}

task_plan_data_length <- function(task_name){
  return(length(task_plan(task_name)))
}

task_plan_data <- function(task_name, index_data){
  tp <- task_plan(task_name)[[index_data]]
  data <- list()
  for(i in 1:length(tp$plan_data)){
    x_data <- tp$plan_data[[i]]$data
    x_db_table <- tp$plan_data[[i]]$db_table
    x_filter <- tp$plan_data[[i]]$filter

    if(is.na(x_filter)){
      d <- fd::tbl(x_db_table) %>%
        dplyr::collect() %>%
        fd::latin1_to_utf8()
    } else {
      d <- fd::tbl(x_db_table) %>%
        dplyr::filter(!!!rlang::parse_exprs(x_filter)) %>%
        dplyr::collect() %>%
        fd::latin1_to_utf8()
    }

    data[[x_data]] <- d
  }

  return(data)
}

task_plan_analysis_length <- function(task_name, index_data){
  return(length(task_plan(task_name)[[index_data]]$plan_analysis))
}
task_plan_analysis <- function(task_name, index_data, index_analysis){
  tpa <- task_plan(task_name)[[index_data]]$plan_analysis[[index_analysis]]

  return(tpa)
}

task_plan_data_analysis <- function(task_name){
  num_data <- task_plan_data_length(task_name)
  retval <- vector("list", length=num_data)
  for(i in seq_along(retval)){
    num_analysis <- task_plan_analysis_length(task_name, index_data = i)
    retval[[i]] <- data.frame(index_data=i, index_analysis=1:num_analysis)
  }
  retval <- rbindlist(retval)
  return(retval)
}

#' run_with_data
#'
#' Fetches data from DB with appropriate filters
#' Runs analysis and writes results back to DB
#'
#' @export
run_with_data <- function(r6_func, task_name, by_location=TRUE){
  plan <- task_config(task_name)$plan_func()
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
    r6_func
    do.call(analysis_func, task_config$args)
  }
}

run_task <- function(task_name, log=TRUE){
  task <- get(task_config(task_name)$r6_func)$new(task_name = task_name)
  output_schema <- task_config(task_name)$output_schema
  if(!is.null(output_schema)){
    #fd::drop_table(output_schema$db_table)
    output_schema$db_connect()
  }

  if(log == FALSE | task$can_run()){
    print(glue::glue("Running task {task_name}"))
    if(is.null(task_config(task_name)$plan_func)){
      task$run()
    } else {
      tpda <- task_plan_data_analysis(task_name)
      pb <- fhi::txt_progress_bar(max=nrow(tpda))
      i <- 0
      for(index_data in 1:task_plan_data_length(task_name)){
        data <- task_plan_data(task_name, index_data)
        for(index_analysis in 1:task_plan_analysis_length(task_name, index_data)){
          task$run(data=data, index_data, index_analysis)

          if(interactive()) utils::setTxtProgressBar(pb, i)
          i <- i + 1
        }
      }
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

