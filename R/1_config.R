set_config <- function() {
  set_computer_name()
  set_computer_type()
  set_border()

  # set DB

  config$db_config <- list(
    driver = Sys.getenv("DB_DRIVER", "MySQL"),
    server = Sys.getenv("DB_SERVER", "db"),
    port = as.integer(Sys.getenv("DB_PORT", 3306)),
    user = Sys.getenv("DB_USER", "root"),
    password = Sys.getenv("DB_PASSWORD", "example"),
    db = Sys.getenv("DB_DB", "sykdomspuls")
  )
  config$AGES <- list(
    "Totalt" = c(0:105),
    "0-4" = c(0:4),
    "5-14" = c(5:14),
    "15-19" = c(15:19),
    "20-29" = c(20:29),
    "30-64" = c(30:64),
    "65+" = c(65:105)
  )
  

  # set schema

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
      db_table = "results_normomo_standard",
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
    data_norsyss = list(
      task_name = "data_norsyss",
      type = "data",
      r6_func = "DataNorSySS",
      args = list(
        syndromes = rbind(
          data.table(
            tag = "gastro",
            syndrome = "gastro",
            contactType = list(c("Legekontakt", "Telefonkontakt"))
          ),
          data.table(
            tag = "influensa",
            syndrome = "influensa",
            contactType = list("Legekontakt")
          ),
           data.table(
             tag = "influensa_all",
            syndrome = "influensa_all",
            contactType = list(c("Legekontakt", "Telefonkontakt"))
          )
        )
      )
    ),
    norsyss_mem_influensa = list(
      task_name = "norsyss_mem_influensa",
      db_table = "data_norsyss",
      type = "analysis",
      dependencies = c("norsyss_data"),
      r6_func = "AnalysisMEM",
      filter = "(granularity_geo=='county' | granularity_geo=='norge') & tag_outcome=='influensa'",
      for_each=list("location_code"="all"),
      args = list(
        age = jsonlite::toJSON(list("Totalt" = c("Totalt"))),
        tag = "influensa",
        weeklyDenominatorFunction = "sum",
        multiplicative_factor = 100,
        denominator = "consult_with_influenza"
        )
    ),
    norsyss_mem_influensa_all = list(
      task_name = "norsyss_mem_influensa_all",
      db_table = "data_norsyss",
      type = "analysis",
      dependencies = c("norsyss_data"),
      r6_func = "AnalysisMEM",
      filter = "(granularity_geo=='county' | granularity_geo=='norge') & tag_outcome=='influensa_all'",
      for_each=list("location_code"="all"),
      args = list(
        age = jsonlite::toJSON(list(
          "0-4" = c("0-4"), "5-14" = c("5-14"),
          "15-64" = c("15-19", "20-29", "30-64"), "65+" = c("65+")
        )),
        tag = "influensa",
        weeklyDenominatorFunction = "sum",
        multiplicative_factor = 100,
        denominator = "consult_with_influenza"
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

  config$tasks <- TaskManager$new()
  config$tasks$task_add(
    task_name = "data_normomo",
    type = "data",
    r6= "DataNormomo"
  )

  config$tasks$task_add(
    task_name = "analysis_normomo",
    type = "analysis",
    r6 = "AnalysisNormomo",
    fn_plan = analysis_normomo_plan,
    schema = c("output"="results_normomo")
  )

}


set_computer_name <- function() {
  if (file.exists("/tmp/computer")) {
    con <- file("/tmp/computer", "r")
    name_computer <- readLines(con, n = 1)
    close(con)
  } else {
    name_computer <- "NO_NAME_FOUND"
  }
  Sys.setenv(COMPUTER = name_computer)
  config$name_computer <- name_computer
}

set_computer_type <- function() {
  if (config$name_computer %in% config$name_production) {
    config$is_production <- TRUE
  } else if (config$name_computer %in% config$name_testing) {
    config$is_testing <- TRUE
  } else {
    config$is_dev <- TRUE
  }
}

set_db <- function() {
  config$db_config <- list(
    driver = Sys.getenv("DB_DRIVER", "MySQL"),
    server = Sys.getenv("DB_SERVER", "db"),
    port = as.integer(Sys.getenv("DB_PORT", 3306)),
    user = Sys.getenv("DB_USER", "root"),
    password = Sys.getenv("DB_PASSWORD", "example"),
    db = Sys.getenv("DB_DB", "sykdomspuls")
  )
}

set_border <- function() {
  if (config$is_production) {
    config$border <- 2020
  } else {
    config$border <- 2020
  }
}
