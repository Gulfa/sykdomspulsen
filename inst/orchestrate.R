devtools::load_all()

config <- get_config()

for(task in names(config[["tasks"]])){
  run_task(task, log=TRUE)
}
