#' shortcut to get available task names
#' @export
get_task_names_tm <- function(){
  names(config$tasks$list_task)
}

#' Shortcut to task
#' @param task_name Name of the task
#' @param index_plan Not used
#' @param index_argset Not used
#' @export
get_task_tm <- function(task_name, index_plan = NULL, index_argset = NULL) {
  config$tasks$get_task(task_name)
}

#' Shortcut to run task
#' @param task_name Name of the task
#' @param index_plan Not used
#' @param index_argset Not used
#' @export
run_task_tm <- function(task_name, index_plan = NULL, index_argset = NULL) {
  task <- get_task_tm(
    task_name = task_name,
    index_plan = index_plan,
    index_argset = index_argset
  )
  task$run()
}

#' Shortcut to plan within task
#' @param task_name Name of the task
#' @param index_plan Not used
#' @param index_argset Not used
#' @export
get_plans_tm <- function(task_name, index_plan = NULL, index_argset = NULL) {
  get_task_tm(task_name = task_name)$plans
}

#' Shortcut to plan within task
#' @param task_name Name of the task
#' @param index_plan Plan within task
#' @param index_argset Not used
#' @export
get_plan_tm <- function(task_name, index_plan = 1, index_argset = NULL) {
  get_task_tm(task_name = task_name)$plans[[index_plan]]
}

#' Shortcut to data within plan within task
#' @param task_name Name of the task
#' @param index_plan Plan within task
#' @param index_argset Not used
#' @export
get_data_tm <- function(task_name, index_plan = 1, index_argset = NULL) {
  get_plan_tm(
    task_name = task_name,
    index_plan = index_plan
  )$get_data()
}

#' Shortcut to argset within plan within task
#' @param task_name Name of the task
#' @param index_plan Plan within task
#' @param index_argset Argset within plan
#' @export
get_argset_tm <- function(task_name, index_plan = 1, index_argset = 1) {
  get_plan_tm(
    task_name = task_name,
    index_plan = index_plan
  )$get_argset(index_argset)
}

#' Shortcut to schema within task
#' @param task_name Name of the task
#' @param index_plan Not used
#' @param index_argset Not used
#' @export
get_schema_tm <- function(task_name, index_plan = NULL, index_argset = NULL) {
  get_task_tm(
    task_name = task_name
  )$schema
}

#'
#'
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
#'   a) data (from the plan -- `get_data`)
#'   b) arguments (from the plan -- `get_argset`)
#' to the R6 action class
#'
#' A task is:
#' - one R6 Action class
#' - A plnr::Plan that provide
#'   a) data (from the plan -- `get_data`)
#'   b) arguments (from the plan -- `get_argset`)
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
    add_task = function(task) {
      list_task[[task$name]] <<- task
    },
    get_task = function(name) {
      list_task[[name]]
    },
    ## list_plan_get = function(task_name){
    ##   if(is.null(list_task[[task_name]]$list_plan) & is.null(list_task[[task_name]]$fn_plan)){
    ##     retval <- list(x=1)
    ##   } else if(!is.null(list_task[[task_name]]$list_plan) & is.null(list_task[[task_name]]$fn_plan)){
    ##     retval <- list_task[[task_name]]$list_plan
    ##   } else if(is.null(list_task[[task_name]]$list_plan) & !is.null(list_task[[task_name]]$fn_plan)){
    ##     retval <- list_task[[task_name]]$fn_plan()
    ##   }
    ##   return(retval)
    ## },


    task_run = function(name, log = TRUE) {
      list_task[[name]]$run(log)
    },

    run = function() {
      stop("run must be implemented")
    }
  )
)
