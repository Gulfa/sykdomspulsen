options(error=function()traceback(2))

##Intialise
devtools::load_all()

args <- commandArgs(trailingOnly = TRUE)

run_task(args[1], log=FALSE)
