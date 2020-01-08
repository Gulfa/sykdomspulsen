set_config <- function() {
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
          )
        )
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

}
