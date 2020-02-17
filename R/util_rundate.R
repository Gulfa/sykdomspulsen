#' update_rundate
#' Updates the rundate db tables
#' @param task
#' @param date_run the date when the analysis was run
#' @export
update_rundate <- function(task, date_run){
  # date_run = the date when the analysis was run
  to_upload <- data.table(
    task = task,
    date_run = as.Date(date_run)
  )
  config$schema$rundate$db_upsert_load_data_infile(to_upload)
}

#' get_rundate
#' Gets the rundate db table
#' @export
get_rundate <- function(task=NULL) {
  x_task <- task
  if(!is.null(task)){
    temp <- config$schema$rundate$dplyr_tbl() %>%
      dplyr::filter(task == x_task) %>%
      dplyr::collect() %>%
      latin1_to_utf8()
  }else{
    temp <- config$schema$rundate$dplyr_tbl() %>%
      dplyr::collect() %>%
      latin1_to_utf8()
  }    
  return(temp)
}

#' greater_than_rundate
#' Checks to see if the rundate exists fora particular package
#' @param pkg Package
#' @export
exists_rundate <- function(pkg) {
  rd <- get_rundate()
  if (pkg %in% rd$package) {
    return(TRUE)
  }
  return(FALSE)
}
