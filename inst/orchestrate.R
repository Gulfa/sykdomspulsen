devtools::load_all()

for(task in names(config$tasks)){
  run_task(task, log=TRUE)
}
