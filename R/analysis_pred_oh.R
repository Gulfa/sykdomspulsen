
#' analysis_pred_oh
#'
#' Run pred_oh 
#'
#' @export
analysis_pred_oh <-  function(data, argset, schema){
  # arguments

  argset <- tm_get_argset("analysis_pred_oh", index_plan=1, index_argset = 1)

  schema <- tm_get_schema("analysis_pred_oh")
   
   
  data <- prepare_data(argset, schema)
  setDT(data)
  
#  print(head(data))
  model = RegModel$new(n ~ trend +
                           sin(2 * pi * (week - 1) / 52) + cos(2 * pi * (week - 1) / 52) +
                           location_f + 
                           holiday + percent_positive_prev*rain_prev*temp_prev +
                           offset(log(denominator)))
  
  data[, age:=as.character(age)]
  model$add_data(c("age"),data)

  last_week <- max(unique(data[, yrwk]))
  n_week <- length(unique(data[, yrwk]))

  holidays <- fhidata::norway_dates_holidays
  holidays[, yrwk := format.Date(date, "%G-%V")]


  limits <- list()
  denoms <- list()
  groups <-unique(data[, .(location_code, age)])
  for(i in 1:nrow(groups)){
    l <- groups[i]

    predict_data <- data.frame(
      date = week_to_date(next_week(last_week)),
      denominator=10,
      holiday=mean(holidays[yrwk == next_week(last_week), is_holiday])
    )
    train_data <-  data %>% dplyr::right_join(l, by=c("location_code", "age"))
    setDT(train_data)
    train_data[, date:=date.x]

    train_data_denom <- copy(train_data)

    train_data_denom[, n:=denominator]
    train_data_denom[, denominator:=pop]

    predict_data_denom <- data.table(
      date = week_to_date(next_week(last_week)),
      denominator=train_data[yrwk==last_week, pop],
      holiday=mean(holidays[yrwk == next_week(last_week), is_holiday])
    )
    
    denom_results <- QuasipoissonTrainPredictData(train_data_denom, predict_data_denom, isDaily=F)
    denoms[[i]] <- data.table(
      denominator=denom_results$n_baseline_expected,
      location_code=l[, location_code],
      age=l[, age]
    )
    

    predict_data <- data.table(
      date = week_to_date(next_week(last_week)),
      denominator=denom_results$n_baseline_expected,
      holiday=mean(holidays[yrwk == next_week(last_week), is_holiday])
    )
    p <- QuasipoissonTrainPredictData(train_data, predict_data, isDaily=F)
    limits[[length(limits) + 1]] <- data.frame(
      location_code=l[, location_code],
      age=l[, age],
      n_baseline_thresholdu0=p[, n_baseline_thresholdu0],
      n_baseline_thresholdu1=p[, n_baseline_thresholdu1]
    )
  }
  limits <- rbindlist(limits)
  denoms <- rbindlist(denoms)

  
  
  predict_data <- data.frame(
    week = as.integer(next_week(last_week, format="%V")),
    trend = max(data$trend) + 7/100,
    holiday=mean(holidays[yrwk == last_week, is_holiday]),
    percent_positive_prev=data[yrwk==last_week, percent_positive],
    location_f=data[yrwk==last_week, location_f],
    location_code=data[yrwk==last_week, location_code],
    rain_prev=data[yrwk==last_week, rain],
    temp_prev=data[yrwk==last_week, temp],
    age=as.character(data[yrwk==last_week, age]),
    denominator=denoms[data[yrwk==last_week], on=c("location_code", "age")]$denominator
  )
    
  print(class(predict_data$age))
  preds <- model$predict(c(n_week +1), predict_data, N=1000)


  comb <- preds[limits, on=c("location_code", "age")]
  predictions <- as.matrix(comb  %>% dplyr::select(dplyr::starts_with("V", ignore.case=FALSE)))
  comb[, p_thresholdu0:=rowMeans(predictions > n_baseline_thresholdu0)]
  comb[, p_thresholdu1:=rowMeans(predictions > n_baseline_thresholdu1)]
  comb[, n_est:=rowMeans(predictions)]
  comb[, n_est_std:=matrixStats::rowSds(predictions)]
  
  res <- comb[, .(location_code, age, p_thresholdu0, p_thresholdu1, n_est, n_est_std)]
  res[, granularity_time:="weekly"]
  res[, granularity_geo:="municip"]
  res[, tag_outcome:="gastro"]
  res[, yrwk:=next_week(last_week)]
  res[, date:=week_to_date(yrwk)]
  res[, week := as.numeric(format.Date(date, "%V"))]
  res[, year := as.numeric(format.Date(date, "%G"))]
  res[, month := as.numeric(format.Date(date, "%m"))]
  res[, season := fhi::season(yrwk)]
  res[, x := fhi::x(week)]
  res[, n_denominator_est:= predict_data$denominator]
  res[, sex:="Totalt"]
  res[, source:="data_norsyss"]
  print(nrow(res))

  schema$output$db_upsert_load_data_infile(res)  

}


week_to_date <- function(wkyr){
    return(ISOweek::ISOweek2date(paste(gsub("-", "-W", wkyr),"-1", sep="")))
  }
prev_week <- function(wkyr){
  date <- ISOweek::ISOweek2date(paste(gsub("-", "-W", wkyr),"-1", sep=""))
  new_date <- date - 7
  return(format.Date(new_date, "%G-%V"))
 
}
next_week <- function(wkyr, format="%G-%V"){
  date <- ISOweek::ISOweek2date(paste(gsub("-", "-W", wkyr),"-1", sep=""))
  new_date <- date + 7
  return(format.Date(new_date, format))

}
prepare_data <- function(argset, schema){

  human_tag <- argset$human_tag
  animal_tag <- argset$animal_tag
  human_data <- schema$input_human$dplyr_tbl() %>% dplyr::filter(
    granularity_time == "day" &
    granularity_geo == "municip" &
    tag_outcome == human_tag
  ) %>% dplyr::collect()
  setDT(human_data)
  human_data[, denominator:=get(argset$denominator)]
  human_data <- human_data[,
                           .(n=sum(n),
                             denominator=sum(denominator),
                             pop = mean(pop),
                             holiday=mean(holiday, na.rm=TRUE)
                 ),
               by=.(yrwk, location_code, age)]
  human_data <- human_data[fhidata::days, on = "yrwk", date := sun]
  print(head(human_data))
  animal_data <- schema$input_animal$dplyr_tbl() %>% dplyr::filter(
    granularity_time == "weekly" &
    granularity_geo == "municip" &
    tag == animal_tag
  ) %>% dplyr::collect()
  setDT(animal_data)
  animal_data[, percent_positive:=positive/N*100]
  animal_data[, prev_yrwk:=prev_week(yrwk)]
  
  weather_data <- schema$input_weather$dplyr_tbl() %>% dplyr::filter(
    granularity_time == "daily" &
      granularity_geo == "municip" 
  ) %>% dplyr::group_by(yrwk, location_code) %>%
    dplyr::summarize(temp=mean(tg, na.rm=TRUE), rain=sum(rr, na.rm=TRUE)) %>% dplyr::collect()
  setDT(weather_data)
  weather_data[, prev_yrwk:=prev_week(yrwk)]
  
  data <- human_data %>%
    dplyr::left_join(animal_data, by=c("yrwk"="yrwk", "location_code"="location_code")) %>%
    dplyr::left_join(animal_data %>% dplyr::select(percent_positive_prev=percent_positive, N_prev=N, location_code, prev_yrwk), by=c("yrwk"="prev_yrwk", "location_code"="location_code")) %>%
    dplyr::left_join(weather_data, by=c("yrwk"="yrwk", "location_code"="location_code"))%>%
    dplyr::left_join(weather_data %>% dplyr::select(rain_prev=rain, temp_prev=temp, location_code, prev_yrwk), by=c("yrwk"="prev_yrwk", "location_code"="location_code"))


  setDT(data)
  locs <- unique(data[!is.na(N_prev), location_code])
  data <- data[location_code %in% locs]
  data[is.na(N_prev), N_prev:=0]
  data[is.na(percent_positive_prev), percent_positive_prev :=0]
  data[is.na(N), N:=0]
  data[is.na(percent_positive), percent_positive :=0]
  data[, trend:=(as.numeric(date.x) - 13000) / 100]
  data[, location_f := factor(location_code)]
  
  return(data)
  

}

RegModel <-  R6::R6Class(
  "RegModel",
  public=list(
    data = NULL,
    regformula = NULL,
    groups = NULL,
    max_time = NULL,
    model_fit = NULL,
    pre = NULL,
    initialize = function(regformula, pre=NULL){
      if(is.null(pre)){
        pre <- function(x){return(x)}
      }
     
      self$pre <- pre
      self$regformula <- regformula
      self$model_fit <- list()


    },
    add_data = function(groups, data){
      print("add_data")
      self$groups <- groups
      weeks <- unique(data[, yrwk])
      self$max_time <- length(weeks)
      i <- 1
      self$data <- data
      self$data[, week_number:=week_number_from_yrwk(yrwk)]
    },
    predict = function(predict_times, pred_data, N=1000){
      
      last_history_time <- predict_times[1] - 1
      unique_groups <- self$data %>% dplyr::distinct(!!!syms(self$groups))
      out <- list()
      for(group in 1:nrow(unique_groups)){

        group_data <- self$data  %>% dplyr::right_join(unique_groups[group], by=self$groups)
        hist_data <- group_data  %>% dplyr::filter( week_number %in% 1:last_history_time)
        setDT(hist_data)
        setDT(group_data)

        hist_data[denominator==0, denominator:=1]
        mod <- glm2::glm2(self$regformula, data = hist_data,
                          family = quasipoisson, na.action = na.omit)
        # if(predict_times[length(predict_times)] == (self$max_time-1)){
       #   print(group)
       #   print(summary(mod))
       # }
        self$model_fit[[group]] <- mod
        if(is.null(pred_data)){
          pred_data_group <- group_data %>% filter( week_number %in% predict_times)
        } else{
          pred_data$age <- as.character(pred_data$age)
          pred_data_group <- pred_data %>% dplyr::right_join(unique_groups[group], by=self$groups)
        }
        preds <- predict(mod, pred_data_group, se.fit=TRUE, type="response")
        mu <- preds$fit
        var <- mu*summary(mod)$dispersion + preds$se.fit^2
        disp <- var/mu
        disp[disp <= 1] <- 1+ 1e-7
      
        out_df <- rnbinom(N*length(predict_times)*length(disp), mu=mu, size=mu/(disp-1))
        
        dim(out_df) <- c(length(predict_times)*length(disp), N)
        out_df <- data.table(out_df)
        for(g in self$groups){
          out_df <- dplyr::mutate(out_df, !!g := unique_groups[group, get(g)])
        }

        if(!("location_code" %in% self$groups)){
          out_df <- dplyr::mutate(out_df, location_code:=pred_data_group %>% dplyr::pull(location_code))
        }
        out_df <- dplyr::mutate(out_df, time = predict_times)
        x_values <- group_data %>% dplyr::filter( week_number %in% predict_times) %>% dplyr::pull(n)
        if(length(x_values) != length(predict_times)*length(disp)){
          x_values <- c(x_values, rep(NA, length(predict_times)*length(disp) - length(x_values)))
        }
        out_df <- dplyr::mutate(out_df, value = x_values)
        out[[group]] <- out_df
      }
      return(rbindlist(out))
    }
  )
)


SimpleModel <-  R6::R6Class(
  "SimpleModel",
  public=list(
    data = NULL,
    groups = NULL,
    max_time = NULL,
    initialize = function(){
    },
    add_data = function(groups, data){
      self$groups <- groups
      weeks <- unique(data[, yrwk])
      self$max_time <- length(weeks)
      i <- 1
      self$data <- data
      self$data[, week_number:=week_number_from_yrwk(yrwk)]

    },
    predict = function(predict_times, predict_data, N=1000){
      last_history_time <- predict_times[1] - 1
      unique_groups <- self$data %>% dplyr::distinct(!!!syms(self$groups))
      out <- list()
      for(group in 1:nrow(unique_groups)){
#        print(unique_groups[group])
        group_data <- self$data  %>% right_join(unique_groups[group], by=self$groups)
        hist_data <- group_data  %>% filter( week_number %in% 1:last_history_time)
        setDT(hist_data)
        mean_incidence <- mean(hist_data[, n])
        out_df <- rpois(N*length(predict_times), mean_incidence * matrix(1, length(predict_times),N))
        dim(out_df) <- c(length(predict_times), N)
        out_df <- data.table(out_df)
        for(g in self$groups){
          out_df <- dplyr::mutate(out_df, !!g := unique_groups[group, get(g)])
        }
        out_df <- dplyr::mutate(out_df, time = predict_times)
        x_values <- group_data %>% filter( week_number %in% predict_times) %>% dplyr::pull(n)
        if(length(x_values) != length(predict_times)){
          x_values <- c(x_values, rep(NA, length(predict_times) -length(x_values)))
        }
        out_df <- dplyr::mutate(out_df, value = x_values)
        out[[group]] <- out_df

        
      }
      return(rbindlist(out))
    }
  )
)

week_number_from_yrwk <- function(wkyrs){
  weeks <- unique(wkyrs)

  map_list <- list()
  i = 1
  for(w in weeks){
    map_list[[w]] <- i
    i = i+1
  }

  return(unlist(sapply(wkyrs, function(w) map_list[w], simplify=TRUE)))
  
}



BSTSModel <-  R6::R6Class(
  "RegModel",
  public=list(
    data = NULL,
    regformula = NULL,
    groups = NULL,
    max_time = NULL,
    pre = NULL,
    season = NULL,
    trend = NULL,
    initialize = function(regformula, trend, season, pre=NULL){
      if(is.null(pre)){
        pre <- function(x){return(x)}
      }
     # self$pre <- pre
      self$regformula <- regformula
      self$season <- season
      self$trend <- trend

    },
    add_data = function(groups, data){
      self$data <- data
      self$groups <- groups
      self$max_time <- nrow(data) / nrow(data %>% dplyr::distinct(!!!syms(groups)))

    },
    predict = function(predict_times, pred_data, N=1000){
      last_history_time <- predict_times[1] - 1
      unique_groups <- self$data %>% dplyr::distinct(!!!syms(self$groups))
      out <- list()
      for(group in 1:nrow(unique_groups)){
        group_data <- self$data  %>% dplyr::right_join(unique_groups[group], by=self$groups)
        hist_data <- group_data  %>% dplyr::slice(1:last_history_time)
        setDT(hist_data)
        setDT(group_data)

        ss <- list()
        if(self$trend == "local_linear_trend"){
          ss <- AddLocalLinearTrend(list(), hist_data$n)
        }else if( self$trend == "semilocal_linear_trend"){
          ss <- AddSemilocalLinearTrend(list(), hist_data$n)
        }

        if(!is.null(self$season)){
          ss <- AddSeasonal(ss, hist_data$n, nseasons = self$season)
        }

        
        mod <- bsts(self$regformula,
                    state.specification = ss,
                    data=hist_data,
                    niter = N + 200,
                    ping=0)

        if(is.null(pred_data)){
          pred_data <- group_data[predict_times]
        }

        #pred_data <- create_pred_data(predict_times, unique_groups[group])
        preds <- predict(mod, newdata=pred_data, horizon=nrow(pred_data), burn=200)
        out_df <-t(preds$distribution)
        out_df <- data.table(out_df)
        for(g in self$groups){
          out_df <- dplyr::mutate(out_df, !!g := unique_groups[group, get(g)])
        }
        out_df <- dplyr::mutate(out_df, time = predict_times)
        x_values <- group_data %>% dplyr::slice(predict_times) %>% dplyr::pull(n)
        if(length(x_values) != length(predict_times)){
          x_values <- c(x_values, rep(NA, length(predict_times) -length(x_values)))
        }
        out_df <- dplyr::mutate(out_df, value = x_values)
        out[[group]] <- out_df
      }
      return(rbindlist(out))
    }
  )
)

