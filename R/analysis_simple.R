#' analyse_simple
#'
#' Get and clean MSIS data from msis.no
#'
#' @export
analysis_simple <- function(data, argset, schema){
  print(argset)
  # arguments start
  group_by <- argset$group_by
  past_years <-argset$past_years
  location_code <- argset$location_code
  # arguments end
  data <- data.table(data$data)
  if(nrow(data) > 0){
    max_year <- max(data[, year])
    min_year <- min(data[, year])
    
    for(x_year in (min_year+5):max_year){
      past <- data[year < x_year & year >= (x_year - 5) ,
                   .(n_expected=mean(n), n_std=sd(n)), by=.(get(group_by))]
      data[year == x_year, n_expected:=past[, n_expected]]
      data[year == x_year, n_threshold_0:=past[, n_expected + 1.96*n_std]]
      data[,n_status:= "Normal"]
      data[n >= n_threshold_0 & n >= 0,n_status:= "High"]
    }
    data[, source:=argset$source_table]
    schema$output$db_upsert_load_data_infile(data)
  }
}

