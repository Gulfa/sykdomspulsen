#' get_config
#'
#' gets the config
#' @export

get_config <- function(){
  
  
  config <- list(
    tasks = list(
      msis_data = list(
        type = "data",
        func="get_MSIS_data",
        args = list(
          start_year = 2008,
          end_year=2019
        )
      )
    ),
    db_config= list(
      driver = Sys.getenv("DB_DRIVER", "MySQL"),
      server = Sys.getenv("DB_SERVER", "db") ,
      port = as.integer(Sys.getenv("DB_PORT", 3306)),
      user = Sys.getenv("DB_USER", "root"),
      password = Sys.getenv("DB_PASSWORD", "example"),
      db =  Sys.getenv("DB_DB", "sykdomspuls")
    )
      
  )
  return(config)
  
}
