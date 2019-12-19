set_config <- function() {
  config$db_config <- list(
    driver = Sys.getenv("DB_DRIVER", "MySQL"),
    server = Sys.getenv("DB_SERVER", "db"),
    port = as.integer(Sys.getenv("DB_PORT", 3306)),
    user = Sys.getenv("DB_USER", "root"),
    password = Sys.getenv("DB_PASSWORD", "example"),
    db = Sys.getenv("DB_DB", "sykdomspuls")
  )

  config$schema <- list(
    datar_normomo = fd::schema$new(
      db_config = config$db_config,
      db_table = "datar_normomo",
      db_field_types =  c(
        "uuid" = "TEXT",
        "DoD" = "DATE",
        "DoR" = "DATE",
        "DoB" = "DATE",
        "age" = "INTEGER",
        "location_code" = "TEXT"
      ),
      db_load_folder = "/xtmp/",
      keys =  c(
        "uuid"
      )
    ),
    results_normomo = fd::schema$new(
      db_config = config$db_config,
      db_table = "results_normomo",
      db_field_types =  c(
        "location_code" = "TEXT",
        "age" = "TEXT",
        "date" = "DATE",
        "wk" = "INTEGER",
        "yrwk" = "TEXT",
        "YoDi" = "INTEGER",
        "WoDi" = "INTEGER",
        "Pnb" = "DOUBLE",
        "nb" = "DOUBLE",
        "nbc" = "DOUBLE",
        "UPIb2" = "DOUBLE",
        "UPIb4" = "DOUBLE",
        "UPIc" = "DOUBLE",
        "LPIc" = "DOUBLE",
        "UCIc" = "DOUBLE",
        "LCIc" = "DOUBLE",
        "zscore" = "DOUBLE",
        "excess" = "DOUBLE",
        "thresholdp_0" = "DOUBLE",
        "thresholdp_1" = "DOUBLE",
        "thresholdp_2" = "DOUBLE",
        "excessp" = "DOUBLE",
        "status" = "TEXT"
      ),
      keys = c(
        "location_code",
        "age",
        "yrwk"
      ),
      db_load_folder = "/xtmp/",
      check_fields_match = TRUE
    )
  )

  config$tasks <- list(
    data_normomo = list(
      task_name = "data_normomo",
      type = "data",
      r6_func = "DataNormomo"
    ),
    analysis_normomo = list(
      task_name = "analysis_normomo",
      type = "analysis",
      db_table = "data_msis",
      r6_func = "AnalysisNormomo",
      dependencies = c("datar_normomo"),
      plan_func = analysis_normomo_plan,
      output_schema = config$schema$results_normomo
    ),

    data_msis = list(
      task_name = "data_msis",
      type = "data",
      r6_func = "DataMSIS",
      plan = list(
        start_year = 2008,
        end_year = 2019
      )
    ),
    simple_analysis_msis_kikhoste = list(
      task_name = "simple_analysis_msis_kikhoste",
      type = "analysis",
      db_table = "data_msis",
      r6_func = "AnalysisSimple",
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


}
