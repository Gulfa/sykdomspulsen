#' Shortcut to task
#' @param task_name Name of the task
#' @param index_plan Not used
#' @param index_argset Not used
#' @export
tm_shortcut_task <- function(task_name, index_plan = NULL, index_argset = NULL){
  config$tasks$task_get(task_name)
}

#' Shortcut to plan within task
#' @param task_name Name of the task
#' @param index_plan Plan within task
#' @param index_argset Not used
#' @export
tm_shortcut_plan <- function(task_name, index_plan = 1, index_argset = NULL){
  tm_shortcut_task(task_name = task_name)$list_plan[[index_plan]]
}

#' Shortcut to data within plan within task
#' @param task_name Name of the task
#' @param index_plan Plan within task
#' @param index_argset Not used
#' @export
tm_shortcut_data <- function(task_name, index_plan = 1, index_argset = NULL){
  tm_shortcut_plan(
    task_name = task_name,
    index_plan = index_plan
  )$data_get()
}

#' Shortcut to argset within plan within task
#' @param task_name Name of the task
#' @param index_plan Plan within task
#' @param index_argset Argset within plan
#' @export
tm_shortcut_argset <- function(task_name, index_plan = 1, index_argset = 1){
  tm_shortcut_plan(
    task_name = task_name,
    index_plan = index_plan
  )$argset_get(index_argset)
}

#' Shortcut to schema within task
#' @param task_name Name of the task
#' @param index_plan Not used
#' @param index_argset Not used
#' @export
tm_shortcut_schema <- function(task_name, index_plan = NULL, index_argset = NULL){
  tm_shortcut_task(
    task_name = task_name
  )$schema
}

# data <- config$tasks$task_get("analysis_normomo")$list_plan[[1]]$data_get()
# argset <- config$tasks$task_get("analysis_normomo")$list_plan[[1]]$argset_get(1)
# schema <- config$tasks$task_get("analysis_normomo")$schema



task_config <- function(task_name){
  config$tasks[[task_name]]
}


plan_from_config <- function(config){
  if(config$type == "data"){
    return(list(
      list(
        plan_data=list(),
        plan_analysis=list(
          get_list(config, "plans", list())
        )
      )
      )
      )

  }
  if(config$type == "analysis" | config$type == "ui"){
    table_name <- config$db_table
    plan <- list()
    i <- 1
    filters <- list()
    for(t in names(config$for_each)){
      if(config$for_each[t] == "all"){
        filter <- get_list(config, "filter", default="")
        table <- fd::tbl(table_name)
        if(filter != ""){
          table <- table %>% dplyr::filter(!!!rlang::parse_exprs(filter))
        }
        options <- table %>% dplyr::distinct(!!as.symbol(t)) %>%
          dplyr::collect() %>% dplyr::pull(!!as.symbol(t))
      }else{
        options <- config$for_each[[t]]
      }
      filters[[t]] <- options
    }

    filters <- do.call(tidyr::crossing, filters)
    for(i in 1:nrow(filters)){
      plan[[i]] <- list(
        plan_analysis=list(
          get_list(config, "plans", list())

        )
      )

      fs <- c()
      for(n in names(filters)){
        plan[[i]]$plan_analysis[[1]][[n]] = filters[i,n]
        fs <- c(fs, glue::glue("{n}=='{filters[i,n]}'"))
      }
      extra_filter <- get_list(config, "filter", default="")
      f <- paste(fs, collapse=" & ")
      if(extra_filter != ""){
        f = paste(f, extra_filter, sep=" & ")
      }
      plan[[i]][["plan_data"]] <- list(
        list(
          db_table=table_name,
          filter=f,
          data="data"
        )
      )
    }
  return(plan)
  }

}

get_analysis_plan <- function(task_name){
  config <- task_config(task_name)
  analysis_plan <- get_list(config, "plan_func", default=plan_from_config)(config)
  return(analysis_plan)
}

task_plan_data_length <- function(task_name){
  return(length(get_analysis_plan(task_name)))
}

get_data_analysis <- function(tp){
  data <- list()
  for(i in seq_along(tp$plan_data)){
    x_data <- tp$plan_data[[i]]$data
    x_db_table <- tp$plan_data[[i]]$db_table
    x_filter <- tp$plan_data[[i]]$filter
    print(x_filter)
    print(x_db_table)
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

length_analysis_plan <- function(analysis_plan){

  l <- 0
  for(data_i in analysis_plan){
    l <- l + length(data_i$plan_analysis)
  }
  return(l)
}

run_task <- function(task_name, log=TRUE){
  print(task_config(task_name))
  task <- get(task_config(task_name)$r6_func)$new(task_name = task_name)

  ## if(!is.null(task_config(task_name)$output_schema)){
  ##   #fd::drop_table(task_config(task_name)$output_schema$db_table)
  ##   task_config(task_name)$output_schema$db_connect()
  ## }

  if(log == FALSE | task$can_run()){
    print(glue::glue("Running task {task_name}"))

    analysis_plan <- get_analysis_plan(task_name)
    #print(analysis_plan)
    pb <- fhi::txt_progress_bar(max=length_analysis_plan(analysis_plan))
    i <- 0
    for(index_data in 1:length(analysis_plan)){
      data <- get_data_analysis(analysis_plan[[index_data]])
      analysis_plan_analyses <- get_list(analysis_plan[[index_data]], "plan_analysis", default=list())
      for(index_analysis in seq_along(analysis_plan_analyses)){
        task$run(
          data=data,
          analysis_plan[[index_data]]$plan_data,
          analysis_plan_analyses[[index_analysis]]
        )
        if(interactive()) utils::setTxtProgressBar(pb, i)
        i <- i + 1
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


write_to_db <- function(results=NULL, schema=NULL, source_table=NULL){
  schema$db_config <- config$db_config
  schema$db_connect()
  results[, source:=source_table]
  schema$db_upsert_load_data_infile(results)
}


get_list <- function(l, key, default=NULL){
  if(key %in% names(l)){
    return(l[[key]])
  }else{
    return(default)
  }
}

