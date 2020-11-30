set_db <- function(){
  config$db_config <- list(
    driver = Sys.getenv("DB_DRIVER", "MySQL"),
    server = Sys.getenv("DB_SERVER", "db"),
    port = as.integer(Sys.getenv("DB_PORT", 3306)),
    user = Sys.getenv("DB_USER", "root"),
    password = Sys.getenv("DB_PASSWORD", "example"),
    db = Sys.getenv("DB_DB", "sykdomspuls")
  )

  # set schema
  config$schema <- list(
    rundate = schema$new(
      db_config = config$db_config,
      db_table = "rundate",
      db_field_types = c(
        "task" = "TEXT",
        "date_run" = "DATE"
      ),
      db_load_folder = "/xtmp/",
      keys = c(
        "task",
        "date_run"),
      check_fields_match = TRUE
    ),   
    
    datar_normomo = schema$new(
      db_config = config$db_config,
      db_table = "datar_normomo",
      db_field_types =  c(
        "uuid" = "TEXT",
        "DoD" = "DATE",
        "DoR" = "DATE",
        "DoB" = "DATE",
        "age" = "INTEGER",
        "location_code" = "TEXT",
        "date_extracted" = "DATE"
      ),
      db_load_folder = "/xtmp/",
      keys =  c(
        "uuid"
      )
    ),

    data_weather = schema$new(
      db_config = config$db_config,
      db_table = "data_weather",
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "season" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",
        "tg" = "DOUBLE",
        "tx" = "DOUBLE",
        "tn" = "DOUBLE",
        "rr" = "DOUBLE",
        "forecast" = "BOOLEAN",
        "border" = "INTEGER"
      ),
      db_load_folder = "/xtmp/",
      keys =  c(
        "location_code",
        "date"
      )
    ),
    data_veterinary = schema$new(
      db_config = config$db_config,
      db_table = "data_veterinary",
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "season" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",
        "tag" = "TEXT",
        "positive" = "INTEGER",
        "N" = "INTEGER",
        "border" = "INTEGER"
      ),
      db_load_folder = "/xtmp/",
      keys =  c(
        "location_code",
        "date"
      )
    ),
      
    results_normomo_standard = schema$new(
      db_config = config$db_config,
      db_table = "results_normomo_standard",
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "season" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "n_obs" = "INTEGER",

        "ncor_est" = "DOUBLE",
        "ncor_thresholdl0" = "DOUBLE",
        "ncor_thresholdu0" = "DOUBLE",
        "ncor_zscore" = "DOUBLE",
        "ncor_status" = "TEXT",
        "ncor_excess" = "DOUBLE",

        "ncor_baseline_expected" = "DOUBLE",
        "ncor_baseline_thresholdl0" = "DOUBLE",
        "ncor_baseline_thresholdu0" = "DOUBLE",
        "ncor_baseline_thresholdu1" = "DOUBLE",

        "forecast" = "BOOLEAN"
      ),
      keys = c(
        "granularity_time",
        "granularity_geo",
        "location_code",
        "age",
        "sex",
        "year",
        "week",
        "date"
      ),
      db_load_folder = "/xtmp/",
      check_fields_match = TRUE
    ),
    data_msis = schema$new(
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
    ),
    data_norsyss = schema$new(
      db_table = "data_norsyss",
      db_config = config$db_config,
      db_field_types =  c(
        "tag_outcome" = "TEXT",
        "location_code" = "TEXT",
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "border" = "INTEGER",
        "holiday" = "DOUBLE",
        "age" = "TEXT",
        "sex" = "TEXT",
        "date" = "DATE",
        "yrwk" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "month" = "INTEGER",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "n" = "INTEGER",
        "pop" = "INTEGER",
        "consult_with_influenza" = "INTEGER",
        "consult_without_influenza" = "INTEGER"
      ),
      db_load_folder = "/xtmp/",
      keys =  c(
        "tag_outcome",
        "location_code",
        "year",
        "date",
        "age"
      )
    ),
    results_simple = schema$new(
      db_table = "results_simple",
      db_config = config$db_config,
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
    ),
    results_mem = schema$new(
      db_table = "results_mem",
      db_config = config$db_config,
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
        "n" = "INTEGER",
        "n_denominator" = "INTEGER",
        "rp100" = "DOUBLE",
        "rp100_baseline_thresholdu0" = "DOUBLE",
        "rp100_baseline_thresholdu1" = "DOUBLE",
        "rp100_baseline_thresholdu2" = "DOUBLE",
        "rp100_baseline_thresholdu3" = "DOUBLE",
        "rp100_status"= "TEXT"
      ),
      db_load_folder = "/xtmp/",
      keys =  c(
        "tag_outcome",
        "location_code",
        "year",
        "date",
        "age"
      )
    ),
    results_qp = schema$new(
      db_table = "results_qp",
      db_config = config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "tag_outcome" = "TEXT",
        "location_code" = "TEXT",
        "age" = "TEXT",
        "sex" = "TEXT",
        "yrwk" = "TEXT",
        "year" = "DOUBLE",
        "week" = "DOUBLE",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",
        "n" = "INTEGER",
        "n_denominator" = "INTEGER",
        "n_baseline_expected" = "DOUBLE",
        "n_baseline_thresholdu0" = "DOUBLE",
        "n_baseline_thresholdu1" = "DOUBLE",
        "n_baseline_thresholdu2" = "DOUBLE",
        "n_zscore" = "DOUBLE",
        "n_status" = "TEXT",
        "failed" = "TINYINT",
        "source" = "TEXT"
      ),
      db_load_folder = "/xtmp/",
      keys =  c(
        "granularity_time",
        "granularity_geo",
        "tag_outcome",
        "location_code",
        "age",
        "year",
        "week",
        "date"
      )
    ),
    results_pred_oh = schema$new(
      db_table = "results_pred_oh",
      db_config = config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "tag_outcome" = "TEXT",
        "location_code" = "TEXT",
        "age" = "TEXT",
        "sex" = "TEXT",
        "yrwk" = "TEXT",
        "year" = "DOUBLE",
        "week" = "DOUBLE",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",
        "p_thresholdu0" = "DOUBLE",
        "p_thresholdu1" = "DOUBLE",
        "n_est" = "DOUBLE",
        "n_est_std" = "DOUBLE",
        "n_denominator_est" = "DOUBLE",
        "source" = "TEXT"
      ),
      db_load_folder = "/xtmp/",
      keys =  c(
        "granularity_time",
        "granularity_geo",
        "tag_outcome",
        "location_code",
        "age",
        "year",
        "week",
        "date"
      )
    ),
    results_mem_limits = schema$new(
      db_table = "results_mem_limits",
      db_config = config$db_config,
      db_field_types = list(
        "season" = "TEXT",
        "tag_outcome" = "TEXT",
        "age" = "TEXT",
        "location_code" = "TEXT",
        "rp100_baseline_thresholdu0" = "DOUBLE",
        "rp100_baseline_thresholdu1" = "DOUBLE",
        "rp100_baseline_thresholdu2" = "DOUBLE",
        "rp100_baseline_thresholdu3" = "DOUBLE"
      ),
      db_load_folder = "/xtmp/",
      keys = c("season", "tag_outcome", "age", "location_code")
    )
  )
  config$schema$rundate$db_connect()
}

