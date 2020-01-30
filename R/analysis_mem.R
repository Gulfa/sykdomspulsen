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
    #data[, yrwk := fhi::isoyearweek(date)]
    #data[, year := fhi::isoyear_n(date)]
    #data[, week := fhi::isoweek_n(date)]
    #data[, season := fhi::season(yrwk)]
    data <- data[, .(
      n = sum(n),
      consult_without_influenza = sum(consult_without_influenza),
      consult_with_influenza = sum(consult_with_influenza),
      pop = sum(pop)
    ),
    by = c("location_code", "tag_outcome", "granularity_time", "granularity_geo",
           "sex",
           "border", "yrwk", "season", "year", "week")
    ]

    data[, denominator:=get(conf$denominator)]


    mem_df <- prepare_data_frame(data, mult_factor = conf$multiplicative_factor)
    mem_results <- run_mem_model(mem_df, conf)
    mem_results_db <- mem_results
    mem_results_db[, age := age_group]
    mem_results_db[, location_code:=conf$location_code ]
    mem_results_db[, tag_outcome := conf$tag]
    mem_results_db[, rp100_baseline_thresholdu0:=low]
    mem_results_db[, rp100_baseline_thresholdu1:=medium]
    mem_results_db[, rp100_baseline_thresholdu2:=high]
    mem_results_db[, rp100_baseline_thresholdu3:=very_high]

    mem_limits_schema$db_upsert_load_data_infile(mem_results_db)

    data[mem_results, on = "season", low := low]
    data[mem_results, on = "season", medium := medium]
    data[mem_results, on = "season", high := high]
    data[mem_results, on = "season", very_high := very_high]

    data[, age := age_group]
    data[, tag := conf$tag]
    data[, rp100 := n / denominator * conf$multiplicative_factor]
    data[fhidata::days, on = "yrwk", date := mon]

    data[, rp100_status := as.character(NA)]
    data[rp100 < low, rp100_status := "verylow"]
    data[rp100 >= low & rp100 < medium, rp100_status := "low"]
    data[rp100 >= medium & rp100 < high, rp100_status := "medium"]
    data[rp100 >= high & rp100 < very_high, rp100_status := "high"]
    data[rp100 >= very_high, rp100_status := "veryhigh"]
    data[, granularity_time:="week"]
    data[, rp100_baseline_thresholdu0:=low]
    data[, rp100_baseline_thresholdu1:=medium]
    data[, rp100_baseline_thresholdu2:=high]
    data[, rp100_baseline_thresholdu3:=very_high]
    data[, source:=source]
    data[, x := fhi::x(week)]
    data[, n_denominator:=denominator]
   # print(head(data, 40))
    mem_schema$db_upsert_load_data_infile(data)
  }
}



prepare_data_frame <- function(data, mult_factor = 100) {
  useful_data <- data[week %in% c(1:20, 40:52)]
  useful_data[, x := fhi::x(week)]
  useful_data[, rp100 := n / denominator * mult_factor]
  out <- dcast.data.table(useful_data, x ~ season, value.var = "rp100")
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



#' analysis_mem
#'
#' Run mem analysis
#'
#' @export
analysis_mem <-  function(data, argset, schema){
  # arguments start
  conf <- argset
  run_mem(data$data, conf, schema$output, schema$output_limits,argset$source_table)
}



