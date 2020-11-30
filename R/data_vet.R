#' get_NorSySS_data
#'
#' Get and clean from file
#'
#'
#' @import data.table
#'
#' @export
data_veterinary <- function(data, argset, schema){
  # argset <- tm_get_argset("data_norsyss")
  # schema <- tm_get_schema("data_norsyss")

  file <-path("input", "veterinary", "campylobacter.csv")

  data_ve <- fread(file)
  
  data_ve[, positive:=kjennelse=="Påvist"]
  data_ve[, week:= as.numeric(gsub("^.*-W", "", ISOweek)), ]
  data_ve[, yrwk:= gsub("W", "", ISOweek)]
  data_ve[, location:=paste0("municip", stringr::str_pad(komnr, 4, side="left", pad="0"))]
  campylobacter_data <- data_ve[, .(N=sum(freq), positive=sum(positive*freq)), by=.(week, yrwk, location, year=aar)]
  campylobacter_data<- campylobacter_data %>% dplyr::left_join(fd::norway_municip_merging(), by=c("year"="year","location"="municip_code_original"))
  setDT(campylobacter_data)

  campylobacter_data[, location_code:=location]
  campylobacter_data[, tag:="campylobacter"]
  campylobacter_data[, granularity_time:="weekly"]
  campylobacter_data[, granularity_geo:="municip"]
  campylobacter_data[, granularity_geo:="municip"]
  campylobacter_data[, date:=ISOweek::ISOweek2date(paste(gsub("-", "-W", yrwk),"-1", sep=""))]

  campylobacter_data[, month := as.numeric(format.Date(date, "%m"))]
  campylobacter_data[, season := fhi::season(yrwk)]
  campylobacter_data[, x := fhi::x(week)]
  campylobacter_data[, border:=config$border]

  schema$output$db_upsert_load_data_infile(campylobacter_data, verbose=F)
}

