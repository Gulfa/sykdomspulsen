get_county_municip <- function(){
  handle <- handle("http://www.msis.no/StandardRapport.aspx")
  initial <- GET(handle=handle)
  initial_data <- html_form(content(initial))[[1]]$fields
  fylke <- 2996:3017

  out <- list()

  r<- POST(handle=handle,
           body=list(
             "__VIEWSTATE"=initial_data[["__VIEWSTATE"]]$value,
             "__EVENTTARGET"="m_ctrlStandardRapportList",
             m_ctrlStandardRapportList=1,
             m_ctrlYearList=2019,
             "__VIEWSTATEGENERATOR"="D56CFAB9",
             "__EVENTVALIDATION"=initial_data[["__EVENTVALIDATION"]]$value,
             "__EVENTARGUMENT"= "",
             "__LASTFOCUS"=""
             ))
  initial_data <- html_form(content(r))[[1]]$fields
  i <- 1
  counties <- html_form(content(r))[[1]][["fields"]][["m_ctrlFylke"]][["options"]]
  for(name in names(counties)){
    code <- counties[[name]]
    r<- POST(handle=handle,
             body=list(
               "__VIEWSTATE"=initial_data[["__VIEWSTATE"]]$value,
               "__EVENTTARGET"="m_ctrlFylke",
               m_ctrlStandardRapportList=1,
               m_ctrlYearList=2019,
               m_ctrlFylke=code,
               "__VIEWSTATEGENERATOR"="D56CFAB9",
               "__EVENTVALIDATION"=initial_data[["__EVENTVALIDATION"]]$value,
               "__EVENTARGUMENT"= "",
               "__LASTFOCUS"=""
             ))
    municips <- html_form(content(r))[[1]][["fields"]][["m_ctrlKommuner"]][["options"]]
    #print(municips)
    for(m_name in names(municips)){
      #print(m_name)
      #print(municips[m_name])
      out[[i]] <- list(municip=m_name[1], municip_code = municips[[m_name]],
                       county=name[1], county_code=code)
      i = i + 1

    }

  }
  return(out)
}


get_data <- function(x_year, x_county, x_municip){

    handle <- handle("http://www.msis.no/StandardRapport.aspx")
    initial <- GET(handle=handle)
    initial_data <- html_form(content(initial))[[1]]$fields


    intermediate <- POST(handle=handle,
         body=list(
             "__VIEWSTATE"=initial_data[["__VIEWSTATE"]]$value,
          "__EVENTTARGET"="m_ctrlStandardRapportList",
             m_ctrlStandardRapportList=1,
             m_ctrlYearList=2019,
             "__VIEWSTATEGENERATOR"="D56CFAB9",
             "__EVENTVALIDATION"=initial_data[["__EVENTVALIDATION"]]$value,
             "__EVENTARGUMENT"= "",
              "__LASTFOCUS"=""
             ))
    intermediate_data <- html_form(content(intermediate))[[1]]$fields

    intermediate <- POST(handle=handle,
         body=list(
             "__VIEWSTATE"=intermediate_data[["__VIEWSTATE"]]$value,

             "__EVENTTARGET"="m_ctrlFylke",
             m_ctrlStandardRapportList=1,
             m_ctrlYearList=2019,
             m_ctrlFylke=x_county,
             "__VIEWSTATEGENERATOR"="D56CFAB9",
             "__EVENTVALIDATION"=intermediate_data[["__EVENTVALIDATION"]]$value,
             "__EVENTARGUMENT"= "",
              "__LASTFOCUS"=""
             ))
    intermediate_data <- html_form(content(intermediate))[[1]]$fields

    r <- POST(handle=handle,
         body=list(
             "__VIEWSTATE"=intermediate_data[["__VIEWSTATE"]]$value,
             m_ctrlYearList=x_year,
             m_ctrlStandardRapportList=1,
             m_ctrlFylke=x_county,
             m_ctrlKommuner=x_municip,
             m_ctrlMakeTable="Lag Tabell",
             "__VIEWSTATEGENERATOR"="D56CFAB9",
             "__EVENTVALIDATION"=intermediate_data[["__EVENTVALIDATION"]]$value
             )
          )
    a <- XML::readHTMLTable(content(r, "text"))$m_ucReportGrid_m_ctrlRapportGrid
    data <- tidyr::gather(a, "month", "n", -Sykdom)
    setDT(data)
    data[, year:=x_year]
    data[, municip_code:=x_municip]
    data[, county_code:=x_county]
    return(data)
}


clean_data <- function(data){
    months = c("Januar",
             "Februar",
             "Mars",
             "April",
             "Mai",
             "Juni",
             "Juli",
             "August",
             "September",
             "Oktober",
             "November",
             "Desember")

    data[, n:=as.integer(n)]
    data[is.na(n), n:=0]
    data[, date:=as.Date(ISOdate(year,match(month, months), 1))]
    data <- data[!is.na(Sykdom)]
    return(data)
}

#' get_MSIS_data
#'
#' Get and clean MSIS data from msis.no
#'
#' @import httr
#' @import data.table
#' @import rvest
#'
#' @export
DataMSIS <- R6::R6Class(
  "DataMSIS",
  inherit = TaskBase,
  portable = FALSE,
  cloneable = FALSE,
  list(
    run = function(data, analysis){
      # arguments start
      start_year <- analysis$start_year
      end_year <- analysis$end_year
      # arguments end
      municips <- get_county_municip()[1:10]
      data_list <- list()
      i = 1
      for(m in municips){
        for(year in start_year:end_year){
          new_data <- get_data(year, as.integer(m["county_code"]), as.integer(m["municip_code"]))
      new_data[,county:=m["county"]]
          new_data[,municip:=m["municip"]]
          data_list[[i]] <- new_data
          i = i +1
        }
      }
      data <- rbindlist(data_list)
      cleaned_data <- clean_data(data)
      with_loc <- cleaned_data[fd::norway_locations()[, .(location_code=municip_code, municip_name)], on=c("municip"="municip_name")]
      with_loc[, tag_outcome :=Sykdom]
      with_loc <- with_loc[!is.na(tag_outcome),]
      with_loc[, granularity_time:="month"]
      with_loc[, granularity_geo:="municip"]
      with_loc[, border:=fd::config$border]
      with_loc[, age:="Totalt"]
      with_loc[, sex:="Totalt"]
      with_loc[, year:=lubridate::year(date)]
      config$schema$msis_data$db_connect(config$db_config)
      out_data <- with_loc[, .(tag_outcome,
                               location_code,
                               granularity_time,
                               granularity_geo,
                               border,
                               month,
                               year,
                               age,
                               sex,
                               date,
                               n)]
      config$schema$msis_data$db_drop_all_rows()
      config$schema$msis_data$db_load_data_infile(out_data)
    }
  )
)




