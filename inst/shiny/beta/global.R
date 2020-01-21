library(pool)

db_config <- list(
  driver = Sys.getenv("DB_DRIVER", "MySQL"),
  server = Sys.getenv("DB_SERVER", "db"),
  port = as.integer(Sys.getenv("DB_PORT", 3306)),
  user = Sys.getenv("DB_USER", "root"),
  password = Sys.getenv("DB_PASSWORD", "example"),
  db = Sys.getenv("DB_DB", "sykdomspuls")
)

if(db_config$driver %in% c("ODBC Driver 17 for SQL Server")){
  pool <- dbPool(
    drv = odbc::odbc(),
    driver = db_config$driver,
    server = db_config$server,
    port = db_config$port,
    uid = db_config$user,
    Pwd = db_config$password
  )
} else {
  pool <- dbPool(
    drv = odbc::odbc(),
    driver = db_config$driver,
    server = db_config$server,
    port = db_config$port,
    user = db_config$user,
    password = db_config$password,
    encoding = "utf8"
  )
}
DBI::dbExecute(pool, glue::glue({"USE {db_config$db};"}))

#
# # FUNCTIONS
# Getlocation_name <- function(location) {
#   location_name <- "Norge"
#   locationHTML <- "Norge"
#
#   if (location != "Norge") {
#     location_name <- fd::norway_locations_long()[location_code==location]$location_name
#   }
#
#   return(location_name)
# }
#
# GetCols <- function(){
#   retval <- rev(fhiplot::warning_color)
#   names(retval) <- NULL
#   #retval <- c('#fc8d59','#ffffbf','#91cf60')
#   return(retval)
# }
#
# GLOBAL <- new.env(parent = emptyenv())
# val <- pool %>% dplyr::tbl("spuls_standard_results") %>%
#   dplyr::summarize(date=max(date,na.rm=T)) %>%
#   dplyr::collect() %>%
#   fd::latin1_to_utf8()
#
# GLOBAL$dateMax <- val$date
# GLOBAL$dateMinRestrictedRecent <- GLOBAL$dateMax - 365
# GLOBAL$dateMinRestrictedLine <- GLOBAL$dateMax - 365 * 15
#
# GLOBAL$outbreaksyrwk <- GLOBAL$weeklyyrwk <- rev(fhidata::days[yrwk<=fhi::isoyearweek(GLOBAL$dateMax)]$yrwk)[1:20]
#
# vals <- unique(fd::norway_locations()[,c("county_code","county_name")])
# GLOBAL$weeklyCounties <- c("Norge", vals$county_code)
# names(GLOBAL$weeklyCounties) <- c("Norge", vals$county_name)
#
# CONFIG_OLD <- sykdomspuls::ConvertConfigForAPI()
# GLOBAL$weeklyTypes <- GLOBAL$dailyTypes <- CONFIG_OLD$SYNDROMES[CONFIG_OLD$SYNDROMES %in% sykdomspuls::CONFIG$STANDARD[websiteInternal == TRUE]$tag]
# GLOBAL$weeklyAges <- GLOBAL$dailyAges <- CONFIG_OLD$AGES
#
# vals <- fd::norway_locations_long()[location_code!="norway"]
# vals[fd::norway_locations(),on="location_code==municip_code",county_code:=county_code]
# vals[is.na(county_code),county_code:=location_code]
# vals[location_code=="norge",location_code:="Norge"]
# vals[location_code=="norge",county_code:="Norge"]
#
# GLOBAL$municipToCounty <- vals
#
# GLOBAL$weeklyValues <- c(
#   "Konsultasjoner" = "consults",
#   "1 uke eksess" = "excess1"
# )

