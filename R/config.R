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
      ),
      msis_simple_analysis_kikhoste = list(
        type="analysis",
        db_table="data_msis",
        func="analyse_simple",
        dependencies=c("msis_data"),
        filter = dplyr::quos(tag_outcome=="Kikhoste"),
        args = list(
          group_by="month",
          past_years=5)
      ),
      ui_threshold_plot_campy = list(
        type="ui",
        db_table="results_simple",
        filter = dplyr::quos(tag_outcome=="Campylobacteriose" & year > 2010 & source == "data_msis"),
        func="ui_create_threshold_plot",
        dependencies=c("msis_simple_analysis_campy"),
        args=list(
          filename="{location_code}.png",
          folder =" campy/{date}"
          )
      ),

      msis_simple_analysis_campy = list(
        type="analysis",
        db_table="data_msis",
        func="analyse_simple",
        dependencies=c("msis_data"),
        filter = dplyr::quos(tag_outcome=="Campylobacteriose"),
        args = list(
          group_by="month",
          past_years=5)
      ),
      ui_threshold_plot_kikhoste = list(
        type="ui",
        db_table="results_simple",
        filter = dplyr::quos(tag_outcome=="Kikhoste" & year > 2010 & source == "data_msis"),
        func="ui_create_threshold_plot",
        dependencies=c("msis_simple_analysis_kikhoste"),
        args=list(
          filename="{location_code}.png",
          folder =" kikhoste/{date}"
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
