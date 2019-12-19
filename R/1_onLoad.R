tc <- function(task_name){
  config$tasks[[task_name]]
}

.onLoad <- function(libname, pkgname) {
  config$db_config <- list(
    driver = Sys.getenv("DB_DRIVER", "MySQL"),
    server = Sys.getenv("DB_SERVER", "db"),
    port = as.integer(Sys.getenv("DB_PORT", 3306)),
    user = Sys.getenv("DB_USER", "root"),
    password = Sys.getenv("DB_PASSWORD", "example"),
    db = Sys.getenv("DB_DB", "sykdomspuls")
  )

  config$tasks <- list(
    data_normomo = list(
      task_name = "data_normomo",
      type = "data",
      r6_func = "r6_data_normomo",
      args = list()
    ),
    analysis_normomo = list(
      task_name = "analysis_normomo",
      type = "analysis",
      r6_func = "r6_analysis_normomo",
      dependencies = c("datar_normomo"),
      args = list()
    ),

    data_msis = list(
      task_name = "data_msis",
      type = "data",
      r6_func = "r6_data_msis",
      args = list(
        start_year = 2008,
        end_year = 2019
      )
    ),
    simple_analysis_msis_kikhoste = list(
      task_name = "simple_analysis_msis_kikhoste",
      type = "analysis",
      db_table = "data_msis",
      r6_func = "analysis_simple",
      dependencies = c("msis_data"),
      filter = dplyr::quos(tag_outcome == "Kikhoste"),
      args = list(
        group_by = "month",
        past_years = 5
      )
    ),
    ui_threshold_plot_campy = list(
      task_name = "ui_threshold_plot_campy",
      type = "ui",
      db_table = "results_simple",
      filter = dplyr::quos(tag_outcome == "Campylobacteriose" & year > 2010 & source == "data_msis"),
      r6_func = "ui_create_threshold_plot",
      dependencies = c("msis_simple_analysis_campy"),
      args = list(
        filename = "{location_code}.png",
        folder = " campy/{date}"
      )
    ),

    msis_simple_analysis_campy = list(
      task_name = "msis_simple_analysis_campy",
      type = "analysis",
      db_table = "data_msis",
      r6_func = "analyse_simple",
      dependencies = c("msis_data"),
      filter = dplyr::quos(tag_outcome == "Campylobacteriose"),
      args = list(
        group_by = "month",
        past_years = 5
      )
    ),
    ui_threshold_plot_kikhoste = list(
      task_name = "ui_threshold_plot_kikhoste",
      type = "ui",
      db_table = "results_simple",
      filter = dplyr::quos(tag_outcome == "Kikhoste" & year > 2010 & source == "data_msis"),
      r6_func = "ui_create_threshold_plot",
      dependencies = c("msis_simple_analysis_kikhoste"),
      args = list(
        filename = "{location_code}.png",
        folder = " kikhoste/{date}"
      )
    )
  )

  invisible()
}
