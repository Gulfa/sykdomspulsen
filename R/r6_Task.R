#' task_from_config
#' @export
task_from_config <- function(conf){
  task_name <- conf$task_name
  list_plan <- list()
  schema <- conf$schema
  task <- NULL
  if(conf$type == "data"){
    plan <- plnr::Plan$new()
    arguments <- list(fn = get(conf$action),name = task_name)
    if("args" %in% names(conf)){
      arguments <- c(arguments, conf$args)
    }
    do.call(plan$analysis_add, arguments)
    list_plan <- list(plan)
    task <- Task$new(task_name,
             conf$type,
             list_plan,
             schema)
  }
  
  if(conf$type == "analysis" | conf$type == "ui"){
    task <- Task$new(task_name,
                     conf$type,
                     list_plan,
                     schema)
    task$update_func <- function(){
      table_name <- conf$db_table
      plan <- list()
      i <- 1
      filters <- list()
      for(t in names(conf$for_each)){
        if(conf$for_each[t] == "all"){
          filter <- get_list(conf, "filter", default="")
        table <- fd::tbl(table_name)
          if(filter != ""){
            table <- table %>% dplyr::filter(!!!rlang::parse_exprs(filter))
          }
          options <- table %>% dplyr::distinct(!!as.symbol(t)) %>%
          dplyr::collect() %>% dplyr::pull(!!as.symbol(t))
        }else{
          options <- conf$for_each[[t]]
        }
        filters[[t]] <- options
      }
      
      filters <- do.call(tidyr::crossing, filters)
      for(i in 1:nrow(filters)){
        current_plan <- plnr::Plan$new()
        fs <- c()
        arguments <- list(fn = get(conf$action),name = glue::glue("{task_name}{i}"),
                          source_table = table_name,
                          today=Sys.Date())
        for(n in names(filters)){
          arguments[n] = filters[i,n]
          fs <- c(fs, glue::glue("{n}=='{filters[i,n]}'"))
        }
        extra_filter <- get_list(conf, "filter", default="")
        
        filter <- paste(fs, collapse=" & ")
        if(extra_filter != ""){
          filter = paste(filter, extra_filter, sep=" & ")
        }
        current_plan$data_add(name="data", fn = function(){
          if(is.na(filter)){
            d <- fd::tbl(table_name) %>%
              dplyr::collect() %>%
              fd::latin1_to_utf8()
          } else {
            d <- fd::tbl(table_name) %>%
              dplyr::filter(!!!rlang::parse_exprs(filter)) %>%
              dplyr::collect() %>%
              fd::latin1_to_utf8()
          }
          return(d)
        })
       
        if("args" %in% names(conf)){
          arguments <- c(arguments, conf$args)
        }
        do.call(current_plan$analysis_add, arguments)
        list_plan[[i]] <- current_plan
      }
      return(list_plan)
    }
  }
  return(task)
}
#' Task
#'
#' @import R6
#' @export
Task <- R6::R6Class(
  "Task",
  portable = FALSE,
  cloneable = TRUE,
  public = list(
    list_task = list(),
    type = NULL,
    list_plan = list(),
    schema = list(),
    task_name = NULL,
    update_func = NULL,
    initialize = function(task_name,  type, list_plan, schema) {
      self$task_name <- task_name
      self$type <- type
      self$list_plan <- list_plan
      self$schema <- schema
    },
    update_plans = function(){
      if(!is.null(self$update_func)){
        self$list_plan <- self$update_func()
      }
    },
    num_argsets = function(){
      i <- 0
      for(i in seq_along(list_plan)){
        i <- i + length(list_plan[[i]])
      }
      return(i)
    },
    run = function(log=TRUE){
   # task <- config$tasks$task_get("analysis_normomo")
      message(glue::glue("task: {task_name}"))
      if(log == FALSE | can_run()){
        self$update_plans()
        print
        print(glue::glue("Running task {task_name}"))
        
#        pb <- fhi::txt_progress_bar(max=num_argsets()
        #        pi <- 0
        for(i in seq_along(list_plan)){
          list_plan[[i]]$run_all(schema=schema)
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
  can_run = function(){
    rundates <- fd::get_rundate()
    last_run_date <- head(rundates[package == task_name]$date_run, 1)
    curr_date <- lubridate::today()
    dependencies <- c()
    for(dependency in get_list(task_config(task_name), "dependencies",default=c())){
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
  }
  )
)

