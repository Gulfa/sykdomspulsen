#' Action Base Class
#' @import R6
#' @export
ActionBase <- R6::R6Class(
  "ActionBase",
  portable = FALSE,
  cloneable = TRUE,
  public = list(
    task_name = list(),
    initialize = function(task_name) {
      task_name <<- task_name
    },
    can_run = function(){
      return(T)
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
