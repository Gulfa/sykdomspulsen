#' Schedule
#' @import R6
#' @export
Schedule <- R6::R6Class(
  "Schedule",
  portable = FALSE,
  cloneable = TRUE,
  public = list(
    list_task = list(),
    initialize = function() {
    },
    task_add = function(
      task_name = NULL,
      type = NULL,
      r6 = NULL,
      list_plan = NULL,
      fn_plan = NULL,
      schema = NULL
    ){
      list_task[[task_name]] <<- list(
        task_name = task_name,
        type = type,
        r6 = r6,
        list_plan = list_plan,
        fn_plan = fn_plan,
        schema = schema
      )
    },
    list_plan_get = function(task_name){
      if(is.null(list_task[[task_name]]$list_plan) & is.null(list_task[[task_name]]$fn_plan)){
        retval <- list(x=1)
      } else if(!is.null(list_task[[task_name]]$list_plan) & is.null(list_task[[task_name]]$fn_plan)){
        retval <- list_task[[task_name]]$list_plan
      } else if(is.null(list_task[[task_name]]$list_plan) & !is.null(list_task[[task_name]]$fn_plan)){
        retval <- list_task[[task_name]]$fn_plan()
      }
      return(retval)
    },
    task_get = function(task_name){
      retval <- list()
      retval$action <- get(list_task[[task_name]]$r6)$new(task_name = task_name)
      retval$type <- list_task[[task_name]]$type
      retval$list_plan <- list_plan_get(task_name)
      retval$schema <- list()
      for(i in seq_along(list_task[[task_name]]$schema)){
        nam <- names(list_task[[task_name]]$schema)[i]
        sch <- list_task[[task_name]]$schema
        retval$schema[[nam]] <- config$schema[[sch]]
      }

      return(retval)
    },
    action_run = function(task_name, plan_index){

    },
    task_num_actions = function(task_name){
      task <- task_get(task_name)
      i <- 0
      for(i in seq_along(task$list_plan)){
        i <- i + task$list_plan[[i]]$len()
      }
      return(i)
    },
    task_run = function(task_name, log=TRUE){
      # task <- config$schedule$task_get("analysis_normomo")
      print(task_name)
      task <- task_get(task_name)

      if(log == FALSE | task$action$can_run()){
        print(glue::glue("Running task {task_name}"))

        pb <- fhi::txt_progress_bar(max=task_num_actions(task_name))
        pi <- 0
        for(i in seq_along(task$list_plan)){
          data <- task$list_plan[[i]]$data_get()
          for(j in task$list_plan[[i]]$x_seq_along()){
            arg <- task$list_plan[[i]]$analysis_get(j)$arg
            task$action$run(
              data = data,
              arg = arg,
              schema = task$schema
            )
            if(interactive()) utils::setTxtProgressBar(pb, pi)
            pi <- pi + 1
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
    },
    run = function() {
      stop("run must be implemented")
    }
  )
)


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
    table <- config$db_table
    plan <- list()
    i <- 1
    filters <- list()
    for(t in names(config$for_each)){
      if(config$for_each[t] == "all"){
        options <- fd::tbl(table) %>% dplyr::distinct(!!as.symbol(t)) %>%
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
          db_table=table,
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

