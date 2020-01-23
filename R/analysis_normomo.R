#' analysis_normomo
#'
#' Get and clean MSIS data from msis.no
#'
#' @export
analysis_normomo <-  function(data, argset, schema, ...){
  # tm_update_plans("analysis_normomo")
  # data <- tm_get_data("analysis_normomo", index_plan=2)
  # argset <- tm_get_argset("analysis_normomo", index_plan=2, index_argset = 1)
  # schema <- tm_get_schema("analysis_normomo")

  fs::dir_create(argset$wdir)

  d <- as.data.frame(data$raw[fhi::isoyear_n(DoR)<=argset$year_end])
  MOMO::SetOpts(
    DoA = argset$date_extracted,
    DoPR = as.Date("2012-1-1"),
    WStart = 1,
    WEnd = 52,
    country = argset$location_code,
    source = "FHI",
    MDATA = d,
    HDATA = analysis_normomo_hfile(),
    INPUTDIR = tempdir(),
    WDIR = argset$wdir,
    back = 7,
    WWW = 290,
    Ysum = argset$year_end,
    Wsum = 40,
    plotGraphs = FALSE,
    delayVersion = "richard",
    delayFunction = NULL,
    MOMOgroups = argset$momo_groups,
    MOMOmodels = argset$momo_models,
    verbose = FALSE
  )

  tryCatch({
    MOMO::RunMoMo()
  }, error=function(err){
    stop("ERROR HERE")
  })

  if(argset$upload){
    data_dirty <- rbindlist(MOMO::dataExport$toSave, fill = TRUE)
    data_clean <- clean_exported_momo_data(
      data_dirty,
      location_code = argset$location_code
    )
    # only upload
    data_clean <- data_clean[year >= argset$year_start_upload]
    schema$output$db_upsert_load_data_infile(data_clean, verbose = T)
  }
}


analysis_normomo_hfile <- function() {
  hfile <- fhidata::norway_dates_holidays[is_holiday == TRUE]
  hfile[, closed := 1]
  hfile[, is_holiday := NULL]
  return(as.data.frame(hfile))
}

analysis_normomo_plans <- function(){
  val <- tbl("datar_normomo") %>%
    dplyr::summarize(date_extracted=max(date_extracted,na.rm=T)) %>%
    dplyr::collect()
  date_extracted <- val$date_extracted

  list_plan <- list()
  # For SSI
  list_plan[[length(list_plan)+1]] <- plnr::Plan$new()
  list_plan[[length(list_plan)]]$add_data(name = "raw", fn=function(){
    tbl("datar_normomo") %>%
      dplyr::collect() %>%
      latin1_to_utf8()
  })
  list_plan[[length(list_plan)]]$add_analysis(
    fn = analysis_normomo,
    location_code = "norway",
    year_end = fhi::isoyear_n(date_extracted),
    date_extracted = date_extracted,
    wdir = path("output","normomo",lubridate::today(),"ssi"),
    upload = FALSE,
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

  # For FHI
  list_plan[[length(list_plan)+1]] <- plnr::Plan$new()
  list_plan[[length(list_plan)]]$add_data(name = "raw", fn=function(){
    tbl("datar_normomo") %>%
      dplyr::collect() %>%
      latin1_to_utf8()
  })
  for(i in 2012:fhi::isoyear_n(date_extracted)){
    if(i==fhi::isoyear_n(date_extracted)){
      date_extracted_year_specific <- date_extracted
    } else {
      date_extracted_year_specific <- fhidata::days[stringr::str_detect(yrwk, as.character(i))][.N]$sun
    }

    list_plan[[length(list_plan)]]$add_analysis(
      fn = analysis_normomo,
      location_code = "norge",
      year_start_upload = ifelse(i==2012, 2008, i-1),
      year_end = i,
      date_extracted = date_extracted_year_specific,
      wdir = tempdir(),
      upload = TRUE,
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
    list_plan[[length(list_plan)]]$add_data(name = "raw", fn=function(){
      tbl("datar_normomo") %>%
        dplyr::filter(location_code==!!j) %>%
        dplyr::collect() %>%
        latin1_to_utf8()
    })
    for(i in 2012:fhi::isoyear_n(date_extracted)){
      if(i==fhi::isoyear_n(date_extracted)){
        date_extracted_year_specific <- date_extracted
      } else {
        date_extracted_year_specific <- fhidata::days[stringr::str_detect(yrwk, as.character(i))][.N]$sun
      }

      list_plan[[length(list_plan)]]$add_analysis(
        fn = analysis_normomo,
        location_code = j,
        year_start_upload = ifelse(i==2012, 2008, i-1),
        year_end = i,
        date_extracted = date_extracted_year_specific,
        wdir = tempdir(),
        upload = TRUE,
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
  data_dirty,
  location_code
  ) {

  data_dirty <- data_dirty[
    !is.na(Pnb),
    c(
      "GROUP",
      "wk",
      "wk2",
      "YoDi",
      "WoDi",
      "Pnb",
      "nb",
      "nbc",
      "UPIb2",
      "UPIb4",
      "UPIc",
      "LPIc",
      "UCIc",
      "LCIc",
      "zscore"
    ),
    with = F]

  data_dirty[,forecast := nbc != nb]

  # prediction interval
  data_dirty[is.na(UPIc) | UPIc < nbc, UPIc := nbc]
  data_dirty[is.na(LPIc) | LPIc > nbc, LPIc := nbc]
  data_dirty[LPIc < 0, LPIc := 0]
  # prediction interval cant be below the real value!
  data_dirty[is.na(LPIc) | LPIc < nb, LPIc := nb]

  # remove prediction intervals before correction
  data_dirty[forecast==FALSE, UPIc := nbc]
  data_dirty[forecast==FALSE, LPIc := nbc]

  setnames(data_dirty,c("LPIc","UPIc"),c("ncor_thresholdl0","ncor_thresholdu0"))
  setnames(data_dirty, "nb", "n_obs")
  setnames(data_dirty, "nbc", "ncor_est")
  setnames(data_dirty, "Pnb", "ncor_baseline_expected")
  setnames(data_dirty, "wk2", "yrwk")

  data_dirty[,yrwk:=as.character(yrwk)]
  setnames(data_dirty, "YoDi", "year")
  setnames(data_dirty, "WoDi", "week")
  setnames(data_dirty, "zscore", "ncor_zscore")

  data_dirty[, location_code := location_code]
  data_dirty[location_code == "norway", location_code := "norge"]
  data_dirty[, age := dplyr::case_when(
    GROUP == "0to4" ~ "0-4",
    GROUP == "5to14" ~ "5-14",
    GROUP == "15to64" ~ "15-64",
    GROUP == "65P" ~ "65+",
    GROUP == "Total" ~ "totalt"
  )]

  # creating thesholds
  data_dirty[, ncor_baseline_thresholdl0 := ncor_baseline_expected - abs(UPIb2 - ncor_baseline_expected)]
  setnames(data_dirty, "UPIb2", "ncor_baseline_thresholdu0")
  setnames(data_dirty, "UPIb4", "ncor_baseline_thresholdu1")
  data_dirty[, ncor_excess := pmax(ncor_est - ncor_baseline_thresholdu0, 0)]
  data_dirty[, ncor_status := "normal"]
  data_dirty[ncor_est > ncor_baseline_thresholdu0, ncor_status := "medium"]
  data_dirty[ncor_est > ncor_baseline_thresholdu1, ncor_status := "high"]
  data_dirty[fhidata::days, on = "yrwk", date := sun]

  data_dirty[,granularity_time := "day"]
  data_dirty[,granularity_geo := dplyr::case_when(
    location_code == "norge" ~ "nation",
    TRUE ~ "county"
  )]
  data_dirty[,border := config$border]
  data_dirty[,sex:="totalt"]
  data_dirty[,season:=fhi::season(yrwk)]
  data_dirty[,x:=fhi::x(week)]

  data_dirty <- data_dirty[,c(
    "granularity_time",
    "granularity_geo",
    "location_code",
    "border",
    "age",
    "sex",
    "season",
    "year",
    "week",
    "yrwk",
    "x",
    "date",
    "n_obs",
    "ncor_est",
    "ncor_thresholdl0",
    "ncor_thresholdu0",
    "ncor_zscore",
    "ncor_status",
    "ncor_excess",
    "ncor_baseline_expected",
    "ncor_baseline_thresholdl0",
    "ncor_baseline_thresholdu0",
    "ncor_baseline_thresholdu1",
    "forecast"
  )]

  return(data_dirty)
}

