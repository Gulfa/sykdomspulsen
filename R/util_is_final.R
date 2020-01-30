is_final <-function() {
  today <- lubridate::wday(lubridate::today(), week_start = 1)
  return(today %in% config$production_days)
}
