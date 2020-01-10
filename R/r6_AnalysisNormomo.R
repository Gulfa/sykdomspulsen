#' analyse_simple
#'
#' Get and clean MSIS data from msis.no
#'
#' @export
AnalysisNormomo <-R6::R6Class(
  "AnalysisNormomo",
  inherit = ActionBase,
  portable = FALSE,
  cloneable = FALSE,
  list(
    run = function(data, argset, schema){
      # data <- config$tasks$task_get("analysis_normomo")$list_plan[[1]]$data_get()
      # argset <- config$tasks$task_get("analysis_normomo")$list_plan[[1]]$argset_get(1)
      # schema <- config$tasks$task_get("analysis_normomo")$schema

      # data <- tm_shortcut_data("analysis_normomo", index_plan=1)
      # argset <- tm_shortcut_argset("analysis_normomo", index_plan=1, index_argset = 1)
      # schema <- tm_shortcut_schema("analysis_normomo")
      d <- as.data.frame(data$raw[fhi::isoyear_n(DoR)<=argset$year_end])

      MOMO::SetOpts(
        DoA = max(d$DoR),
        DoPR = as.Date("2012-1-1"),
        WStart = 1,
        WEnd = 52,
        country = arg$location_code,
        source = "FHI",
        MDATA = d,
        HDATA = analysis_normomo_hfile(),
        INPUTDIR = tempdir(),
        WDIR = tempdir(),
        back = 7,
        WWW = 290,
        Ysum = arg$year_end,
        Wsum = 40,
        plotGraphs = FALSE,
        delayVersion = "richard",
        delayFunction = NULL,
        MOMOgroups = arg$momo_groups,
        MOMOmodels = arg$momo_models,
        verbose = FALSE
      )

      MOMO::RunMoMo()

      data_to_save <- rbindlist(MOMO::dataExport$toSave, fill = TRUE)
      data_clean <- clean_exported_momo_data(
        data_to_save,
        location_code = arg$location_code
        )
      schema$output$db_upsert_load_data_infile(data_clean)
    }
  )
)

analysis_normomo_hfile <- function() {
  hfile <- fhidata::norway_dates_holidays[is_holiday == TRUE]
  hfile[, closed := 1]
  hfile[, is_holiday := NULL]
  return(as.data.frame(hfile))
}

analysis_normomo_plan <- function(config){
  location_code <- c("norway",unique(fd::norway_locations()$county_code))
  p_data <- data.table(location_code = location_code)
  p_data[,filter1 := glue::glue(
    "location_code=='{location_code}'",
    location_code=location_code
  )]
  p_data[1, filter1:=NA]
  p_data[, data_uuid := 1:.N]
  p_data[, data1 := "deaths_raw"]
  p_data[, db_table1 := "datar_normomo"]

  p_analysis <- data.table(expand.grid(
    location_code = p_data$location_code,
    year_end = 2012:lubridate::year(lubridate::today()),
    stringsAsFactors = F
  ))

  p_analysis <- merge(
    p_analysis,
    p_data,
    by="location_code"
  )
  p_analysis[, filter1:=NULL]
  p_analysis[, db_table1:=NULL]

  setorder(p_analysis, location_code, -year_end)

  plan <- list()
  for(i in 1:nrow(p_data)){
    pd <- p_data[i]
    pa <- p_analysis[data_uuid == pd$data_uuid]
    plan_data <- list()
    plan_data[[1]] <- list(
      data = pd$data1,
      db_table = pd$db_table1,
      filter = pd$filter1
    )

    if(pd$location_code=="norway"){
      momo_groups <- list(
        "0to4" =  "age >= 0 & age <=4",
        "5to14" = "age >= 5 & age <=14",
        "15to64" = "age >= 15 & age <=64",
        "65P" = "age >= 65 | is.na(age)",
        "Total" = "age >= 0 | is.na(age)"
      )
      momo_models <- c(
          "0to4" = "LINE",
          "5to14" = "LINE",
          "15to64" = "LINE_SIN",
          "65P" = "LINE_SIN",
          "Total" = "LINE_SIN"
        )
    } else {
      momo_groups <- list(
        "Total" = "age >= 0 | is.na(age)"
      )
      momo_models <- c(
        "Total" = "LINE_SIN"
      )
    }

    plan_analysis <- vector("list", length = nrow(pa))
    for(j in 1:nrow(pa)){
      plan_analysis[[j]] <- as.list(pa[j,])
      plan_analysis[[j]]$momo_groups <- momo_groups
      plan_analysis[[j]]$momo_models <- momo_models
    }

    plan[[i]] <- list(
      plan_data= plan_data,
      plan_analysis = plan_analysis
    )
  }
  plan

  return(plan)
}

analysis_normomo_plan <- function(config){
  location_code <- c("norway",unique(fd::norway_locations()$county_code))
  list_plan <- list()
  list_plan[[length(list_plan)+1]] <- plnr::Plan$new()
  list_plan[[length(list_plan)]]$data_add(name = "raw", fn=function(){
    fd::tbl("datar_normomo") %>% dplyr::collect() %>% fd::latin1_to_utf8()
  })
  for(i in 2012:lubridate::year(lubridate::today())){
    list_plan[[length(list_plan)]]$argset_add(
      location_code = "norway",
      year_end = i,
      momo_groups = list(
        "0to4" =  "age >= 0 & age <=4",
        "5to14" = "age >= 5 & age <=14",
        "15to64" = "age >= 15 & age <=64",
        "65P" = "age >= 65 | is.na(age)",
        "Total" = "age >= 0 | is.na(age)"
      ),
      momo_models = c(
        "0to4" = "LINE",
        "5to14" = "LINE",
        "15to64" = "LINE_SIN",
        "65P" = "LINE_SIN",
        "Total" = "LINE_SIN"
      )
    )
  }
  for(j in unique(fd::norway_locations()$county_code)){
    list_plan[[length(list_plan)+1]] <- plnr::Plan$new()
    list_plan[[length(list_plan)]]$data_add(name = "raw", fn=function(){
      fd::tbl("datar_normomo") %>% dplyr::collect() %>% fd::latin1_to_utf8()
    })
    for(i in 2012:lubridate::year(lubridate::today())){
      list_plan[[length(list_plan)]]$argset_add(
        location_code = j,
        year_end = i,
        momo_groups = list(
          "0to4" =  "age >= 0 & age <=4",
          "5to14" = "age >= 5 & age <=14",
          "15to64" = "age >= 15 & age <=64",
          "65P" = "age >= 65 | is.na(age)",
          "Total" = "age >= 0 | is.na(age)"
        ),
        momo_models = c(
          "0to4" = "LINE",
          "5to14" = "LINE",
          "15to64" = "LINE_SIN",
          "65P" = "LINE_SIN",
          "Total" = "LINE_SIN"
        )
      )
    }
  }

  return(list_plan)
}



clean_exported_momo_data <- function(
  data,
  location_code
  ) {

  data <- data[!is.na(Pnb), c("GROUP", "wk", "wk2", "YoDi", "WoDi", "Pnb", "nb", "nbc", "UPIb2", "UPIb4", "UPIc", "LPIc", "UCIc", "LCIc", "zscore"), with = F]

  minCorrectedWeek <- min(data[nbc != nb]$wk)

  # prediction interval
  data[is.na(UPIc) | UPIc < nbc, UPIc := nbc]
  data[is.na(LPIc) | LPIc > nbc, LPIc := nbc]

  # making them slightly wider to hide the real information
  # data[wk >= minCorrectedWeek & UPIc == 0, UPIc := 1]
  # data[wk >= minCorrectedWeek & !is.na(UPIc), UPIc := UPIc + 3]
  # data[wk >= minCorrectedWeek & !is.na(LPIc), LPIc := LPIc - 3]
  data[LPIc < 0, LPIc := 0]

  # prediction interval cant be below the real value!
  data[is.na(LPIc) | LPIc < nb, LPIc := nb]

  # remove prediction intervals before correction
  data[wk < minCorrectedWeek, UPIc := nbc]
  data[wk < minCorrectedWeek, LPIc := nbc]

  data[, excess := nbc - Pnb]

  setnames(data, "wk2", "yrwk")
  setnames(data, "GROUP", "age")
  data[, location_code := location_code]

  data[location_code == "norway", location_code := "norge"]

  # creating pretty thesholds
  data[, thresholdp_0 := Pnb - abs(UPIb2 - Pnb)]
  data[, thresholdp_1 := UPIb2]
  data[, thresholdp_2 := UPIb4]
  data[, excessp := pmax(nbc - thresholdp_1, 0)]
  data[, status := "normal"]
  data[nbc > thresholdp_1, status := "medium"]
  data[nbc > thresholdp_2, status := "high"]
  data[fhidata::days, on = "yrwk", date := sun]

  return(data)
}



