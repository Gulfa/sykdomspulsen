devtools::load_all()
options(error=function()traceback(2))
##Intialise



run_task <- function(task){
  print(glue::glue("Running task {task}"))
  task_config <- config[["tasks"]][[task]]
  
  func <- get(task_config$func)
  do.call(func, task_config[["args"]])
  
  

}

config <- get_config()
args <- commandArgs(trailingOnly = TRUE)

run_task(args[1])
