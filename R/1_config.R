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
    ),
    msis_data <- fd::schema$new(
      db_config = config$db_config,
      db_table = "data_msis",
      db_field_types =  c(
        "tag_outcome" = "TEXT",
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
        "n" = "INTEGER"
      ),
      db_load_folder = "/xtmp/",
      keys =  c(
        "tag_outcome",
        "location_code",
        "year",
        "date"
      )
    )
  )
  for(i in config$schema){
    i$db_connect()
  }

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
      args = list(
        start_year = 2008,
        end_year = 2019
      )
    ),
    simple_analysis_msis = list(
      task_name = "simple_analysis_msis",
      type = "analysis",
      db_table = "data_msis",
      r6_func = "AnalysisSimple",
      dependencies = c("msis_data"),
      for_each=list("location_code"="all", "tag_outcome"=c("Kikoste", "Campylobacteriose")),
      args = list(
        group_by = "month",
        past_years = 5
      )
    ),
    ui_threshold_plot_msis = list(
      task_name = "ui_threshold_plot_msis",
      type = "ui",
      r6_func = "UICreateThresholdPlot",
      db_table = "results_simple",
      for_each=list("location_code"="all", "tag_outcome"=c("Kikoste", "Campylobacteriose")),
      dependencies = c("msis_simple_analysis"),
      args = list(
        filename = "{location_code}.png",
        folder = " {tag_outcome}/{date}"
      ),
      filter = "year > 2010 & source == 'data_msis'"
    )
  )

  config$schedule <- Schedule$new()
  config$schedule$task_add(
    task_name = "data_normomo",
    type = "data",
    r6= "DataNormomo"
  )

  config$schedule$task_add(
    task_name = "analysis_normomo",
    type = "analysis",
    r6 = "AnalysisNormomo",
    fn_plan = analysis_normomo_plan,
    schema = c("output"="results_normomo")
  )

}
