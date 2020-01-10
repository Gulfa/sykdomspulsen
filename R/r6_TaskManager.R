#' TaskManager
#'
#' An R6 Action class contains a function called 'run' that takes three arguments:
#' - data (list)
#' - arg (list)
#' - schema (list)
#'
#' An action is (note: we dont explicitly create these):
#' - one R6 Action class
#' - A plnr::Plan that provide
#'   a) data (from the plan -- `data_get`)
#'   b) arguments (from the plan -- `argset_get`)
#' to the R6 action class
#'
#' A task is:
#' - one R6 Action class
#' - A plnr::Plan that provide
#'   a) data (from the plan -- `data_get`)
#'   b) arguments (from the plan -- `argset_get`)
#' to the R6 action class
#'
#' A TaskManager is:
#' - one R6 Action class
#' - A list of plnr::Plan's
#'
#' @import R6
#' @export
TaskManager <- R6::R6Class(
  "TaskManager",
  portable = FALSE,
  cloneable = TRUE,
  public = list(
    list_task = list(),
    initialize = function() {
      # nothing
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
    num_argsets = function(task_name){
      task <- task_get(task_name)
      i <- 0
      for(i in seq_along(task$list_plan)){
        i <- i + task$list_plan[[i]]$len()
      }
      return(i)
    },
    task_run = function(task_name, log=TRUE){
      # task <- config$tasks$task_get("analysis_normomo")
      message(glue::glue("task: {task_name}"))

      task <- task_get(task_name)

      if(log == FALSE | task$action$can_run()){
        print(glue::glue("Running task {task_name}"))

        pb <- fhi::txt_progress_bar(max=task_num_actions(task_name))
        pi <- 0
        for(i in seq_along(task$list_plan)){
          data <- task$list_plan[[i]]$data_get()
          for(j in task$list_plan[[i]]$x_seq_along()){
            argset <- task$list_plan[[i]]$argset_get(j)
            task$action$run(
              data = data,
              argset = argset,
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
