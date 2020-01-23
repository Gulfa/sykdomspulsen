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
        "x" = "INTEGER",
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
        "x" = "INTEGER",
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
    reuslts_simple = schema$new(
      db_table = "reuslts_simple",
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
      db_table = "mem_results",
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
        "n" = "TEXT",
        "rate" = "DOUBLE",
        "rate_threshold_0" = "DOUBLE",
        "rate_threshold_1" = "DOUBLE",
        "rate_threshold_2" = "DOUBLE",
        "rate_threshold_3" = "DOUBLE",
        "rate_status"= "TEXT"
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
        "x" = "INTEGER",
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
    results_mem_limits = schema$new(
      db_table = "mem_limits_results",
      db_config = config$db_config,
      db_field_types = list(
        "season" = "TEXT",
        "tag_outcome" = "TEXT",
        "age" = "TEXT",
        "location_code" = "TEXT",
        "rate_threshold_0" = "DOUBLE",
        "rate_threshold_1" = "DOUBLE",
        "rate_threshold_2" = "DOUBLE",
        "rate_threshold_3" = "DOUBLE"
      ),
      db_load_folder = "/xtmp/",
      keys = c("season", "tag_outcome", "age", "location_code")
    )
  )

  for(i in config$schema){
    #i$db_connect()
  }
}
