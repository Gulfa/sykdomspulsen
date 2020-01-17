set_config <- function() {
  progressr::handlers(progressr::progress_handler(
     format = "[:bar] :current/:total (:percent) in :elapsedfull, eta: :eta",
     clear = FALSE
  ))

  set_computer_name()
  set_computer_type()
  set_border()
  set_db()
  config$smallMunicips <- c(
    "municip1151",
    "municip1835",
    "municip1252",
    "municip1739"
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
    datar_normomo = schema$new(
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
    results_normomo = schema$new(
      db_config = config$db_config,
      db_table = "normomo_standard_results",
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
       "date" = "DATE",
       "n" = "INTEGER",
       "n_denominator" = "INTEGER",
       "n_expected" = "DOUBLE",
       "n_thresholdu0" = "DOUBLE",
       "n_thresholdu1" = "DOUBLE",
       "n_thresholdu2" = "DOUBLE",
       "n_zscore" = "DOUBLE",
       "n_status" = "TEXT",
       "cumE1" = "DOUBLE",
       "cumL1" = "DOUBLE",
       "cumU1" = "DOUBLE",
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

  config$tasks <- TaskManager$new()
  config$tasks$add_task(
    task_from_config(list(
      name = "data_normomo",
      type = "data",
      action="data_normomo",
      schema=list(output=config$schema$datar_normomo)
    )
    )
  )
  config$tasks$add_task(
    task_from_config(
      list(
        name = "norsyss_mem_influensa",
        db_table = "data_norsyss",
        type = "analysis",
        dependencies = c("data_norsyss"),
        action = "analysis_mem",
        filter = "(granularity_geo=='county' | granularity_geo=='norge') & tag_outcome=='influensa'",
        for_each=list("location_code"="all"),
        schema=list(output=config$schema$results_mem,
                    output_limits=config$schema$results_mem_limits),
        args = list(
          age = jsonlite::toJSON(list("Totalt" = c("Totalt"))),
          tag = "influensa",
          weeklyDenominatorFunction = "sum",
          multiplicative_factor = 100,
          denominator = "consult_with_influenza"
        )
      )
    )
  )


  config$tasks$add_task(
    task_from_config(
      conf = list(
        name = "norsyss_qp_gastro",
        db_table = "data_norsyss",
        type = "analysis",
        dependencies = c("data_norsyss"),
        action = "analysis_qp",
        filter = "tag_outcome=='gastro'",
        for_each=list("location_code"="all", "age"="all", "sex"="Totalt"),
        schema=list(output=config$schema$results_qp,
                    output_limits=config$schema$results_mem_limits),
        args = list(
          tag="gastro",
          train_length=5,
          years = c(2018,2019),
          weeklyDenominatorFunction = sum,
          denominator= "consult_without_influenza",
          granularity_time="weekly"
        )
      )
    )
  )

  config$tasks$add_task(
    task_from_config(
      list(
        name = "norsyss_mem_influensa_all",
        db_table = "data_norsyss",
        type = "analysis",
        dependencies = c("data_norsyss"),
        action = "analysis_mem",
        filter = "(granularity_geo=='county' | granularity_geo=='norge') & tag_outcome=='influensa_all'",
        for_each=list("location_code"="all"),
        schema=list(output=config$schema$results_mem,
                    output_limits=config$schema$results_mem_limits),
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
      )
    )
  )
  config$tasks$add_task(
    task_from_config(
      list(
        name = "simple_analysis_msis",
        type = "analysis",
        db_table = "data_msis",
        action = "analysis_simple",
        dependencies = c("data_msis"),
        schema=list(output=config$schema$results_simple),
        for_each=list("location_code"="all", "tag_outcome"=c("Kikoste", "Campylobacteriose")),
        args = list(
          group_by = "month",
          past_years = 5
        )
      )
    )
  )
  config$tasks$add_task(
    task_from_config(
      list(
        name = "ui_threshold_plot_msis",
        type = "ui",
        action = "UI_create_threshold_plot",
        db_table = "reuslts_simple",
        schema=NULL,
        for_each=list("location_code"="all", "tag_outcome"=c("Kikoste", "Campylobacteriose")),
        dependencies = c("norsyss_mem_influensa"),
        args = list(
          filename = "{location_code}.png",
          folder = " {tag_outcome}/{today}"
        ),
        filter = "year > 2010 & source == 'data_msis'"
      )
    )
  )
  config$tasks$add_task(
    task_from_config(
      list(
        name = "ui_norsyss_mem_influensa",
        type = "ui",
        action = "ui_mem_plots",
        db_table = "results_mem",
        schema=NULL,
        for_each=list(tag_outcome=c('influensa_all')),
        dependencies = c("norsyss_mem_influensa_all"),
        args = list(
          tag="influensa",
          icpc2="R60",
          contactType="Legekontakt, Telefonkontakt",
          folder_name = "mem_influensa",
          outputs = c("n_doctors_sheet")
        ),
        filter = "source=='data_norsyss'"
      )
    )
  )

  config$tasks$add_task(
    task_from_config(
      list(
        name = "ui_norsyss_mem_influensa_all",
        type = "ui",
        action = "ui_mem_plots",
        db_table = "results_mem",
        schema=NULL,
        for_each=list(tag_outcome=c('influensa')),
        dependencies = c("simple_analysis_msis"),
        args = list(
          tag="influensa",
          icpc2="R60",
          contactType="Legekontakt",
          folder_name = "mem_influensa",
          outputs = c("charts", "county_sheet", "region_sheet", "norway_sheet")
        ),
        filter = "source=='data_norsyss'"
      )
    )
  )
  config$tasks$add_task(
    task_from_config(
      list(
        name = "data_msis",
        type = "data",
        action = "data_msis",
        schema=list(output=config$schema$data_msis),
        args = list(
          start_year = 2008,
          end_year = 2019
      )
      )
    )
  )

  config$tasks$add_task(
    task_from_config(
      list(
        name = "data_norsyss",
        type = "data",
        action = "data_norsyss",
        schema=list(output=config$schema$data_norsyss),
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
      )
    )
  )
  config$tasks$add_task(
    Task$new(
      name = "analysis_normomo",
      type = "analysis",
      plans = analysis_normomo_plans(),
      schema = c("output"=config$schema$results_normomo)
    )
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
