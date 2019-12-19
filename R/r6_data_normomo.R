data_normomo_get <- function(){
  if(fd::config$is_production){
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
    fd::norway_county_merging(),
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

#' get_MSIS_data
#'
#' Get and clean MSIS data from msis.no
#'
#' @import httr
#' @import data.table
#' @import rvest
#'
#' @export
r6_data_normomo <- R6::R6Class(
  "r6_data_normomo",
  inherit = TaskBase,
  portable = FALSE,
  cloneable = FALSE,
  list(
    run = function(){

      d <- data_normomo_get()

      fd::drop_table(datar_normomo_schema$db_table)
      datar_normomo_schema$db_connect(config$db_config)
      datar_normomo_schema$db_load_data_infile(d)
    }
  )
)

datar_normomo_schema <- fd::schema$new(
    db_table = "datar_normomo",
    db_field_types =  c(
      "uuid" = "TEXT",
      "DoD" = "DATE",
      "DoR" = "DATE",
      "DoB" = "DATE",
      "age" = "INTEGER",
      "location_code" = "TEXT"
    ),
    db_load_folder = "/xtmp/",
    keys =  c(
      "uuid"
    )
)


