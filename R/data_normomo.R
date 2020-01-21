data_normomo_internal <- function(){
  if(config$is_production){
    data_grab <- glue::glue(
      'get -r "ut" /data_raw/normomo/\n',
      'rm ut/*'
    )
    data_grab_txt <- tempfile()
    cat(data_grab, file = data_grab_txt)

    cmd <- glue::glue(
      'sshpass -p{Sys.getenv("NORMOMO_EVRY_PW")} ',
      'sftp -o StrictHostKeyChecking=no -oBatchMode=no -b {data_grab_txt} {Sys.getenv("NORMOMO_EVRY_USER")}; ',
      'mv /data_raw/normomo/ut/* /data_raw/normomo/; ',
      'rmdir /data_raw/normomo/ut'
    )
    system(cmd)
  }

  files <- fs::dir_ls(fd::path("data_raw", package="normomo"), regexp="FHIDOD2_[0-9]+.txt$")
  file <- max(files)
  d <- data.table::fread(file)

  d[, DoD := as.Date(as.character(DODS_DATO), format = "%Y%m%d")]
  d[, DoR := as.Date(as.character(ENDR_DATO), format = "%Y%m%d")]
  d[, DoB := as.Date(as.character(FDATO_YYYYMMDD), format = "%Y%m%d")]
  d[, age := floor(as.numeric(difftime(DoD, DoB, units = "days")) / 365.25)]
  d[is.na(DoR), DoR := DoD + 1]
  d[DoR >= "2015-09-03", DoR := DoR + 1]

  d[, year := as.numeric(stringr::str_sub(DODS_DATO, 1, 4))]
  d[, county_code := paste0("county", formatC(FYLKE, width = 2, flag = "0"))]

  d <- merge(
    d,
    norway_county_merging(),
    by.x = c("county_code", "year"),
    by.y = c("county_code_original", "year"),
    all.x = T,
    allow.cartesian = TRUE
  )

  d[is.na(weighting), weighting := 1]
  d[, x := 1:.N]
  set.seed(4)
  d[, keep := sample(c(TRUE, FALSE), 1, replace = T, prob = c(weighting, 1 - weighting)), by = x]

  d <- d[keep == TRUE]

  d[, county_code := NULL]
  d[, FYLKE := NULL]
  d[, weighting := NULL]
  d[, x := NULL]
  d[, keep := NULL]
  d[, year:=NULL]
  d[, STATUS:=NULL]
  d[, DODS_DATO := NULL]
  d[, FDATO_YYYYMMDD:=NULL]
  d[, KJONN := NULL]
  d[, ENDR_DATO := NULL]
  d[, LEVERINGSDATO := NULL]

  setnames(d, "county_code_current", "location_code")

  d[,uuid:=1:.N]


}

#' DataNormomo
#'
#' Get and clean NorMOMO data
#'
#'  @import data.table
#'
#' @export
data_normomo <- function(data, argset, schema){
  # data <- tm_get_data("data_normomo")
  # argset <- tm_get_argset("data_normomo")
  # schema <- tm_get_schema("data_normomo")

  d <- data_normomo_internal()
  schema$output$db_drop_table()
  schema$output$db_connect()
  schema$output$db_drop_constraint()
  schema$output$db_load_data_infile(d)
  schema$output$db_add_constraint()
}




