#' analyse_simple
#'
#' Get and clean MSIS data from msis.no 
#'
#' @export
analyse_simple <-R6::R6Class(
  "analyse_simple",
  inherit = TaskBase,
  portable = FALSE,
  cloneable = FALSE,
  list(
    run= function(data, group_by="month", past_years=5, location_code=NULL){
      
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
      return(return(list(results=data, schema=simple_schema)))
    }
  )
)

simple_schema <- fd::schema$new(
    db_table = "results_simple",
    db_field_types =  c(
      "tag_outcome" = "TEXT",
      "source" = "TEXT",
      "location_code" = "TEXT",
      "granularity_time" = "TEXT",
      "granularity_geo" = "TEXT",
      "border" = "INTEGER",
      "age" = "TEXT",
      "sex" = "TEXT",
      "date" = "DATE",
      "season" = "TEXT",
      "yrwk" = "TEXT",
      "year" = "INTEGER",
      "week" = "INTEGER",
      "month" = "TEXT",
      "n" = "TEXT",
      "n_expected" = "DOUBLE",
      "n_threshold_0" = "DOUBLE",
      "n_status"= "TEXT"
    ),
    db_load_folder = "/xtmp/",
    keys =  c(
      "tag_outcome",
      "location_code",
      "year",
      "date"
    )
)

