run_mem <- function(input_data, conf, mem_schema, mem_limits_schema, source) {
  ages <- jsonlite::fromJSON(conf$age)
  for (age_group in names(ages)) {
    data_age <- ages[[age_group]]

    data <- data.table()
    for (part_age in data_age) {
      new_data <- input_data[age == part_age]
      data <- rbind(data, new_data)
    }


    return_one <- function(x) {
      return(1)
    }
    data[, yrwk := fhi::isoyearweek(date)]
    data[, year := fhi::isoyear_n(date)]
    data[, week := fhi::isoweek_n(date)]
    data[, season := fhi::season(yrwk)]
    data <- data[, .(
      n = sum(n),
      consult_without_influenza = sum(consult_without_influenza),
      consult_with_influenza = sum(consult_with_influenza),
      pop = sum(pop)
    ),
    by = c("location_code", "tag_outcome", "granularity_time", "granularity_geo",
           "sex", "month", "border", "yrwk", "season", "year", "week")
    ]
 
    data[, denominator:=get(conf$denominator)]

    
    mem_df <- prepare_data_frame(data, mult_factor = conf$multiplicative_factor)
    mem_results <- run_mem_model(mem_df, conf)
    mem_results_db <- mem_results
    mem_results_db[, location_code := "norge"]
    mem_results_db[, age := age_group]
    mem_results_db[, tag_outcome := conf$tag]
    mem_results_db[, rate_threshold_0:=low]
    mem_results_db[, rate_threshold_1:=medium]
    mem_results_db[, rate_threshold_2:=high]
    mem_results_db[, rate_threshold_3:=very_high]

    mem_limits_schema$db_upsert_load_data_infile(mem_results_db)

    data[mem_results, on = "season", low := low]
    data[mem_results, on = "season", medium := medium]
    data[mem_results, on = "season", high := high]
    data[mem_results, on = "season", very_high := very_high]

    data[, age := age_group]
    data[, tag := conf$tag]
    data[, rate := n / denominator * conf$multiplicative_factor]
    data[fhidata::days, on = "yrwk", date := mon]

    data[, rate_status := as.character(NA)]
    data[rate < low, rate_status := "verylow"]
    data[rate >= low & rate < medium, rate_status := "low"]
    data[rate >= medium & rate < high, rate_status := "medium"]
    data[rate >= high & rate < very_high, rate_status := "high"]
    data[rate >= very_high, rate_status := "veryhigh"]

    data[, rate_threshold_0:=low]
    data[, rate_threshold_1:=medium]
    data[, rate_threshold_2:=high]
    data[, rate_threshold_3:=very_high]
    data[, source:=source]
    data[, x := fhi::x(week)]
    mem_schema$db_upsert_load_data_infile(data)
  }
}



prepare_data_frame <- function(data, mult_factor = 100) {
  useful_data <- data[week %in% c(1:20, 40:52)]
  useful_data[, x := fhi::x(week)]
  useful_data[, rate := n / denominator * mult_factor]
  out <- dcast.data.table(useful_data, x ~ season, value.var = "rate")
  out[, x := NULL]
  out <- data.frame(out)
  names(out) <- stringr::str_replace_all(names(out), "\\.", "/")
  names(out) <- stringr::str_remove(names(out), "X")
  if (is.na(out[1, 1])) out <- out[, -1]

  rownames(out) <- c(40:52, 1:20)
  return(out)
}

next_season <- function(season) {
  last_year <- as.integer(stringr::str_split(season, "/")[[1]][2])
  return(paste(last_year, last_year + 1, sep = "/"))
}

run_mem_model <- function(data, conf) {
  out <- list()

  # We need 5 season to calculate the thresholds so start at 6
  for (i in 6:ncol(data)) {
    col <- next_season(names(data)[i])
    model_data <- data[, names(data)[1:i]]

    model_data <- data[, names(model_data)[!(names(model_data) %in% conf$excludeSeason)]]
    epi <- mem::memmodel(model_data)
    out[[col]] <- c(
      epi$epidemic.thresholds[1],
      epi$epi.intervals[1, 4],
      epi$epi.intervals[2, 4],
      epi$epi.intervals[3, 4]
    )
  }

  mem_results <- data.frame(out)
  mem_results$val <- c("low", "medium", "high", "very_high")
  mem_results <- reshape2::melt(mem_results, id = "val")
  setDT(mem_results)
  mem_results[, season := stringr::str_replace(variable, "\\.", "/")]
  mem_results[, season := stringr::str_remove(season, "X")]
  mem_results <- dcast.data.table(mem_results, season ~ val, value.var = "value")

  return(mem_results)
}



#' Analisis_MEM
#'
#' Run mem analysis
#'
#' @export
AnalysisMEM <-R6::R6Class(
  "AnalysisMEM",
  inherit = TaskBase,
  portable = FALSE,
  cloneable = FALSE,
  list(
    run= function(data, plan_data, plan_analysis){
      # arguments start
      conf <- plan_analysis
      mem_schema$db_connect(config$db_config)
      mem_schema$db_config <- config$db_config
#      mem_schema$db_drop_all_rows()
      mem_limits_schema$db_connect(config$db_config)
      mem_limits_schema$db_config <- config$db_config
 #     mem_limits_schema$db_drop_all_rows()
      run_mem(data$data, conf, mem_schema, mem_limits_schema,plan_data[[1]]$db_table)
    }
  )
)

mem_schema <- fd::schema$new(
    db_table = "results_mem",
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
)

mem_limits_schema <- fd::schema$new(
    db_table = "results_mem_limits",
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
