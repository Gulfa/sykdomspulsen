#' analyse_simple
#'
#' Running QP analsys
#'
#' @export
analysis_qp <- function(data, argset, schema){
  # tm_update_plans("norsyss_qp_gastro")
  # data <- tm_get_data("norsyss_qp_gastro")
  # argset <- tm_get_argset("norsyss_qp_gastro")
  # schema <- tm_get_schema("norsyss_qp_gastro")

  # arguments start
  data <- copy(data$data)
 # print(data)

  data[, denominator:=get(argset$denominator)]
  argset$granularity_geo <- data[1, granularity_geo]
  if(argset$granularity_time == "weekly"){
    data <- data[,
                 .(n=sum(n),
                   denominator=argset$weeklyDenominatorFunction(denominator),
                   holiday=mean(holiday)
                  ),
                 by=.(yrwk)]
    data <- data[fhidata::days, on = "yrwk", date := mon]
  }

  years <- argset$years

  for(year in years){
    predict_start <- as.Date(glue::glue("{year}-01-01"))
    predict_end <- as.Date(glue::glue("{year+1}-01-01"))

    min_year_data <- year(min(data[, date]))
    if(min_year_data > year - argset$train_length){
      train_start <- as.Date(glue::glue("{min_year_data}-01-01"))
      train_end <-  as.Date(glue::glue("{min_year_data + argset$train_length}-01-01"))
    }else{
      train_start <- as.Date(glue::glue("{year - argset$train_length}-01-01"))
      train_end <- predict_start
    }
    run_data_train <- data[date >=  train_start & date < train_end]
    run_data_predict <- data[date >= predict_start & date <= predict_end]
    diagnostics <- new_diagnostics_df()
    argset$year_run <- year
    ret <- QuasipoissonTrainPredictData(
      datasetTrain = run_data_train,
      datasetPredict = run_data_predict,
      isDaily = argset$granularity_time == "daily",
      weeklyDenominatorFunction = argset$weeklyDenominatorFunction
    )
    diagnostics <- update_diagnostics(attr(ret, "diagnostics"),argset)

    ret <- clean_post_analysis(ret, argset)
    schema$output$db_upsert_load_data_infile(ret, verbose=F)
  }
}



  #' Adds seasonal week to dataset
#'
#' We often want to graph seasons. This function adds the seasonal week
#' to the dataset \code{data} as the variable \code{x}.
#'
#' @param data A data.table containing the variable \code{week}
#' @return A data.table with the extra variable \code{x}
#' @examples
#' library(data.table)
#' d <- data.table(week = 1:52)
#' AddXToWeekly(d)
#' @import data.table
#' @export AddXToWeekly
AddXToWeekly <- function(data) {
  week <- NULL
  x <- NULL

  data[week >= 30, x := week - 29]
  data[week < 30, x := week + 23]

  return(data)
}

FormatDatasetDaily <- function(data) {
  # variables used in data.table functions in this function
  . <- NULL
  trend <- NULL
  dayOfYear <- NULL
  dayOfWeek <- NULL
  denominator <- NULL
  # end

  data[, trend := (as.numeric(date) - 13000) / 100]
  data[, dayOfYear := data.table::yday(date)]
  data[, dayOfWeek := data.table::wday(date)]
  data[denominator < 1, denominator := 1]

  return(data)
}

FormatDatasetWeekly <- function(
                                data,
                                weeklyDenominatorFunction = sum,
                                by_group = NULL) {
  # variables used in data.table functions in this function
  . <- NULL
  n <- NULL
  denominator <- NULL
  trend <- NULL
  pop <- NULL
  holiday <- NULL
  # end

  data <- data[year >= 2006]
  data[, trend := (as.numeric(date) - 13000) / 100]

  if (is.null(by_group)) {
    by_var <- parse(text = glue::glue("list(year,week)"))
  } else {
    by_var <- parse(text = glue::glue("list(year,week,{by_group})"))
  }
  data <- data[, .(
    n = sum(n),
    denominator = weeklyDenominatorFunction(denominator),
    holiday = mean(holiday),
    trend = mean(trend)
  ),
  by = eval(by_var)
  ]

  data[denominator < 1, denominator := 1]

  return(data)
}


#' Quasipoisson Core Algorithm
#'
#' This is the core algorithm of sykdomspulsen.
#'
#' Description: Applys a surveillance algorithm based on a quasi-poisson regression
#' model to the selected data. The difference from the Farrington algorithm is in how
#' seasonality is accounted for (here it is adjusted for, in Farrington it is removed
#' by design.
#'
#' @param datasetTrain Training data.table
#' @param datasetPredict Prediction data.table
#' @param reweights Number (greater or equal to 0) of residual reweights adjusting for previous outbreaks (default: 1; 1 reweight)
#' @param remove.pandemic.year true/false (default: false; keep 2009 data)
#' @param remove.highcounts Number between 0 and 1 of fraction of high counts to be removed from prediction, to remove impact of earlier outbreaks (default: 0)
#' @param sign.level Significance level for the prediction intervals (default: 0.05)
#' @param isDaily Is it daily data or weekly data?
#' @param v Version (Not in use)
#' @param weeklyDenominatorFunction sum or mean - should the denominator be summed or meaned over time
#' @param uuid uuid
#' @importFrom glm2 glm2
#' @import stats
#' @import data.table
#' @export QuasipoissonTrainPredictData
QuasipoissonTrainPredictData <- function(
                                         datasetTrain,
                                         datasetPredict,
                                         reweights = 1,
                                         remove.pandemic.year = F,
                                         remove.highcounts = 0,
                                         sign.level = 0.05,
                                         isDaily = TRUE,
                                         v = 1,
                                         weeklyDenominatorFunction = sum,
                                         uuid = 1) {
  # variables used in data.table functions in this function
  consult <- NULL
  n <- NULL
  n_expected <- NULL
  n_thresholdu0 <- NULL
  n_thresholdu1 <- NULL
  n_thresholdu2 <- NULL
  n_zscore <- NULL
  cumE1 <- NULL
  cumL1 <- NULL
  cumU1 <- NULL
  failed <- NULL
  revcumE1 <- NULL
  revcumL1 <- NULL
  revcumU1 <- NULL
  denominator <- NULL
  displayDay <- NULL
  # end


  # FUNCTION quasipoisson.algorithm
  #
  # Description: Applys a surveillance algorithm based on a quasi-poisson regression
  # model to the selected data. The difference from the Farrington algorithm is in how
  # seasonality is accounted for (here it is adjusted for, in Farrington it is removed
  # by design.
  #
  # Input (arguments):
  # dataset: data frame with columns for number of cases, covariates and dates per day
  # datecol: name of date-column (default: 'Dato')
  # predinterval: length of prediction interval (default: 30; last 30 days)
  # historical.data.years: number (should be greater or equal to at least 3) of full years of background data (default: 5)
  # mod.pred.window: number (greater or equal to 0) of days window between datasets used for modelling and prediction(default: 90)
  # reweights: number (greater or equal to 0) of residual reweights adjusting for previous outbreaks (default: 1; 1 reweight)
  # remove.pandemic.year: true/false (default: false; keep 2009 data)
  # remove.highcounts: number between 0 and 1 of fraction of high counts to be removed from prediction, to remove impact of earlier outbreaks (default: 0)
  # sign.level: significance level for the prediction intervals (default: 5%)

  datasetTrain[, year := fhi::isoyear_n(date)] # Week-based year, instead of normal year (%Y)
  datasetTrain[, week := fhi::isoweek_n(date)] # Week-based year, instead of normal year (%Y)

  datasetPredict[, year := fhi::isoyear_n(date)] # Week-based year, instead of normal year (%Y)
  datasetPredict[, week := fhi::isoweek_n(date)] # Week-based year, instead of normal year (%Y)

  # SET REGRESSION FORMULA:
  if (isDaily) {
    regformula <- n ~ offset(log(denominator)) + trend + factor(dayOfWeek) + sin(2 * pi * dayOfYear / 366) + cos(2 * pi * dayOfYear / 366) + holiday

    datasetTrain <- FormatDatasetDaily(datasetTrain)
    datasetPredict <- FormatDatasetDaily(datasetPredict)
  } else {
    regformula <- n ~ offset(log(denominator)) + trend + sin(2 * pi * (week - 1) / 52) + cos(2 * pi * (week - 1) / 52) + holiday

    datasetTrain <- FormatDatasetWeekly(datasetTrain, weeklyDenominatorFunction = weeklyDenominatorFunction)
    datasetPredict <- FormatDatasetWeekly(datasetPredict, weeklyDenominatorFunction = weeklyDenominatorFunction)
  }

  if (remove.pandemic.year == T) {
    datasetTrain <- datasetTrain[year != "2009"]
  }

  # If chosen, remove upper given percentage of counts from the prediction:
  if (remove.highcounts > 0) {
    datasetTrain <- datasetTrain[n < quantile(n, (1 - remove.highcounts)), ]
  }

  # FIT QUASI-POISSON REGRESSION MODEL ON THE TRAINING SET:
  normalFunction <- function(regformula, datasetTrain) {
    fit <- glm2::glm2(regformula, data = datasetTrain, family = quasipoisson, na.action = na.omit)
    return(list(fit = fit, failed = !fit$converged))
  }
  exceptionalFunction <- function(err) {
    return(list(fit = NaN, failed = TRUE))
  }
  poisreg <- tryCatch(normalFunction(regformula, datasetTrain), error = exceptionalFunction, warning = exceptionalFunction)
  regression_diagnostics <- new_diagnostics_df(n_row = 1)
  regression_diagnostics$failed <- 1
  if (poisreg$failed) {
    datasetPredict[, n_expected := 0.0]
    datasetPredict[, n_thresholdu0 := 5.0]
    datasetPredict[, n_thresholdu1 := 10.0]
    datasetPredict[, n_thresholdu2 := 15.0]
    datasetPredict[, n_zscore := 0.0]

    datasetPredict[, cumE1 := 0.0]
    datasetPredict[, cumL1 := -5.0]
    datasetPredict[, cumU1 := 5.0]
    datasetPredict[, failed := TRUE]
  } else {
    # REFIT THE REGRESSION USING RESIDUAL WEIGHTS (TO DOWNWEIGHT PREVIOUS OUTBREAKS):
    datasetTrain[, w_i := 1]

    for (i in sort(1:reweights)) {
      dispersion_parameter <- summary(poisreg$fit)$dispersion
      if (i == 0) {
        break
      }
      try({
        anscombe.res <- anscombe.residuals(poisreg$fit, dispersion_parameter)
        anscombe.res[anscombe.res < 1] <- 1 # Alt. 2.58?
        datasetTrain[, w_i := anscombe.res^(-2)] # The weight
        Gamma <- nrow(datasetTrain) / sum(datasetTrain$w_i)
        datasetTrain[, w_i := Gamma * w_i] # Makes sum(w_i) = n
        poisreg$fit <- glm2::glm2(regformula, data = datasetTrain, weights = w_i, family = quasipoisson, na.action = na.omit)
        dispersion_parameter <- summary(poisreg$fit)$dispersion
        regression_diagnostics <- extract_diagnostics(poisreg$fit)
        od <- max(1, sum(poisreg$fit$weights * poisreg$fit$residuals^2) / poisreg$fit$df.r)
      }, TRUE)
    }
    # CALCULATE SIGNAL THRESHOLD (prediction interval from Farrington 1996):
    pred <- predict(poisreg$fit, type = "response", se.fit = T, newdata = datasetPredict)
    datasetPredict[, n_expected := pred$fit]
    datasetPredict[, n_thresholdu0 := FarringtonThreshold(pred, phi = dispersion_parameter, z = 2, skewness.transform = "2/3")]
    datasetPredict[, n_thresholdu1 := FarringtonThreshold(pred, phi = dispersion_parameter, z = 4, skewness.transform = "2/3")]
    datasetPredict[, n_thresholdu2 := FarringtonThreshold(pred, phi = dispersion_parameter, z = 6, skewness.transform = "2/3")]
    datasetPredict[, n_zscore := FarringtonZscore(pred, phi = dispersion_parameter, z = 6, skewness.transform = "2/3", y = n)]

    datasetPredict[, stderr := FarringtonSEinGammaSpace(pred, phi = dispersion_parameter, z = 6, skewness.transform = "2/3")]
    datasetPredict[, cumE1 := n^(2 / 3) - n_expected^(2 / 3)]
    datasetPredict[, cumL1 := (cumE1 - 2 * stderr)^(3 / 2)]
    datasetPredict[, cumU1 := (cumE1 + 2 * stderr)^(3 / 2)]
    datasetPredict[, cumE1 := cumE1^(3 / 2)]

    datasetPredict[, revcumE1 := n_expected^(2 / 3) - n^(2 / 3)]
    datasetPredict[, revcumL1 := (revcumE1 - 2 * stderr)^(3 / 2)]
    datasetPredict[, revcumU1 := (revcumE1 + 2 * stderr)^(3 / 2)]
    datasetPredict[, revcumE1 := revcumE1^(3 / 2)]

    datasetPredict[is.nan(cumE1), cumE1 := -revcumE1]
    datasetPredict[is.nan(cumL1), cumL1 := -revcumU1]
    datasetPredict[is.nan(cumU1), cumU1 := -revcumL1]
    datasetPredict[, revcumE1 := NULL]
    datasetPredict[, revcumL1 := NULL]
    datasetPredict[, revcumU1 := NULL]
    datasetPredict[, stderr := NULL]
    datasetPredict[, failed := FALSE]
  }

  datasetPredict <- AddXToWeekly(datasetPredict)
  datasetPredict[, yrwk := paste0(year, "-", formatC(week, flag = "0", width = 2))]

  if (!isDaily) {
    datasetPredict[fhidata::days, on = "yrwk", date := sun]
  }
  datasetPredict[, uuid := uuid]

  retval <- datasetPredict#[, VARS$REQ_RESULTS_BASIC, with = F]
  attr(retval, "diagnostics") <- regression_diagnostics
  return(retval)
}


#' Calculate Farrington SE in gamma space
#' @param pred Point estimate
#' @param phi Dispersion
#' @param alpha Not used
#' @param z Not used
#' @param skewness.transform "none"/"1/2","2/3"
#' @export FarringtonSEinGammaSpace
FarringtonSEinGammaSpace <- function(pred, phi, alpha = NULL, z = NULL, skewness.transform = "none") {
  mu0 <- pred$fit
  tau <- phi + (pred$se.fit^2) / mu0
  switch(skewness.transform, none = {
    se <- sqrt(mu0 * tau)
    exponent <- 1
  }, `1/2` = {
    se <- sqrt(1 / 4 * tau)
    exponent <- 1 / 2
  }, `2/3` = {
    se <- sqrt(4 / 9 * mu0^(1 / 3) * tau)
    exponent <- 2 / 3
  }, {
    stop("No proper exponent in algo.farrington.threshold.")
  })

  return(se)
}

#' Calculate Farrington threshold
#' @param pred Point estimate
#' @param phi Dispersion
#' @param alpha Alpha (e.g 0.05)
#' @param z Similar to \code{alpha} (e.g. 1.96)
#' @param skewness.transform "none"/"1/2","2/3"
#' @export FarringtonThreshold
FarringtonThreshold <- function(pred, phi, alpha = NULL, z = NULL, skewness.transform = "none") {
  mu0 <- pred$fit
  tau <- phi + (pred$se.fit^2) / mu0
  switch(skewness.transform, none = {
    se <- sqrt(mu0 * tau)
    exponent <- 1
  }, `1/2` = {
    se <- sqrt(1 / 4 * tau)
    exponent <- 1 / 2
  }, `2/3` = {
    se <- sqrt(4 / 9 * mu0^(1 / 3) * tau)
    exponent <- 2 / 3
  }, {
    stop("No proper exponent in algo.farrington.threshold.")
  })
  if (is.null(z)) z <- qnorm(1 - alpha / 2)
  lu <- (mu0^exponent + z *
    se)^(1 / exponent)

  return(lu)
}

#' Farrington Z score
#' @param pred Point estimate
#' @param phi Dispersion
#' @param alpha Alpha (e.g 0.05)
#' @param z Similar to \code{alpha} (e.g. 1.96)
#' @param skewness.transform "none"/"1/2","2/3"
#' @param y Observation
#' @export FarringtonZscore
FarringtonZscore <- function(pred, phi, alpha = NULL, z = NULL, skewness.transform = "none", y) {
  mu0 <- pred$fit
  tau <- phi + (pred$se.fit^2) / mu0
  switch(skewness.transform, none = {
    se <- sqrt(mu0 * tau)
    exponent <- 1
  }, `1/2` = {
    se <- sqrt(1 / 4 * tau)
    exponent <- 1 / 2
  }, `2/3` = {
    se <- sqrt(4 / 9 * mu0^(1 / 3) * tau)
    exponent <- 2 / 3
  }, {
    stop("No proper exponent in algo.farrington.threshold.")
  })
  if (is.null(z)) z <- qnorm(1 - alpha / 2)
  zscore <- (y^exponent - mu0^exponent) / se

  return(zscore)
}


#' Normal <= threshold2
#' threshold2 < Medium <= threshold4
#' threshold4 < High
#'
#' @param data A data.table containing the variables \code{n}, \code{threshold2}, and \code{threshold4}
#' @import data.table
#' @export DetermineStatus
DetermineStatus <- function(data) {
  n_status <- NULL
  n <- NULL
  n_thresholdu0 <- NULL
  n_thresholdu1 <- NULL

  # create "normal", "medium", "high" categories
  data[, n_status := "Normal"]
  data[n > 1 & n > n_thresholdu0, n_status := "Medium"]
  data[n > 1 & n > n_thresholdu1, n_status := "High"]
}

#' clean_post_analysis
#' @param res a
#' @param stack a
#' @export clean_post_analysis
clean_post_analysis <- function(res, argset) {
  ## res <- res[!is.na(threshold2) & !is.infinite(threshold2)]

  res[, age:=argset$age]
  res[, sex:=argset$sex]
  res[, source:=argset$source_table]
  res[, n_denominator:=denominator]
  res[, tag_outcome:=argset$tag]
  res[, granularity_time:=argset$granularity_time]
  res[, granularity_geo:=argset$granularity_geo]
  res[, location_code:=argset$location_code]


  ## res[stack, on = "uuid", age := age]
  ## res[stack, on = "uuid", type := tag]
  ## res[stack, on = "uuid", tag := tag]
  ## res[stack, on = "uuid", location_code := location_code]
  ## res[stack, on = "uuid", file := file]
  ## res[stack, on = "uuid", granularity_time := granularity_time]
  ## res[stack, on = "uuid", granularity_geo := granularity_geo]
  ## res[stack, on = "uuid", v := v]

  # make threshold2 minimum of 2.5 and threshold4 minimum of 3
  res[n_thresholdu0 < 2.5, n_thresholdu0 := 2.5]
  res[n_thresholdu1 < 3, n_thresholdu1 := 3]

  # create "normal", "medium", "high" categories
  DetermineStatus(res)

  # adding in extra information
  # add location name

  # cleaning on small municipalities
  res[location_code %in% config$smallMunicips & age != "Totalt", n := 0 ]
  res[location_code %in% config$smallMunicips & age != "Totalt", threshold2 := 5 ]
  res[location_code %in% config$smallMunicips & age != "Totalt", threshold4 := 10 ]

  return(res)
}
