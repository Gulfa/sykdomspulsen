thredds_collapse_one_day <- function(d) {
  setDT(d)
  setnames(d, c("Var1", "Var2"), c("row", "col"))
  d[fd::senorge(), on = c("row", "col"), location_code := location_code]
  d[fd::senorge(), on = c("row", "col"), year := year]
  d <- d[!is.na(location_code)]
  d[fd::norway_municip_merging(),
    on = c(
      "location_code==municip_code_original",
      "year==year"
    ),
    location_code_current := municip_code_current
  ]
  res <- d[, .(
    value = mean(value, na.rm = T)
  ), keyby = .(location_code_current)]

  skeleton <- fd::norway_locations()[, c("municip_code")]
  skeleton[res, on = "municip_code==location_code_current", value := value]
  setnames(skeleton, "municip_code", "location_code")
  setorder(skeleton, location_code)

  skeleton[, value := zoo::na.locf(value)]

  return(skeleton)
}

thredds_get_data_internal <- function(nc, dates) {
  res <- vector("list", length = length(dates))
  for (i in seq_along(res)) {
    tg <- ncdf4::ncvar_get(nc, "tg", start = c(1, 1, i), count = c(nc$dim$X$len, nc$dim$Y$len, 1))
    d <- reshape2::melt(tg)
    temp <- thredds_collapse_one_day(d)
    setnames(temp, "value", "tg")
    retval <- temp

    if ("tx" %in% names(nc$var)) {
      tx <- ncdf4::ncvar_get(nc, "tx", start = c(1, 1, i), count = c(nc$dim$X$len, nc$dim$Y$len, 1))
      d <- reshape2::melt(tx)
      temp <- thredds_collapse_one_day(d)
      retval[temp, on = "location_code", tx := value]
    } else {
      retval[, tx := as.numeric(NA)]
    }

    if ("tn" %in% names(nc$var)) {
      tn <- ncdf4::ncvar_get(nc, "tn", start = c(1, 1, i), count = c(nc$dim$X$len, nc$dim$Y$len, 1))
      d <- reshape2::melt(tn)
      temp <- thredds_collapse_one_day(d)
      retval[temp, on = "location_code", tn := value]
    } else {
      retval[, tn := as.numeric(NA)]
    }

    if ("rr" %in% names(nc$var)) {
      rr <- ncdf4::ncvar_get(nc, "rr", start = c(1, 1, i), count = c(nc$dim$X$len, nc$dim$Y$len, 1))
      d <- reshape2::melt(rr)
      temp <- thredds_collapse_one_day(d)
      retval[temp, on = "location_code", rr := value]
    } else {
      retval[, rr := as.numeric(NA)]
    }

    res[[i]] <- retval
    res[[i]][, date := dates[i]]
  }

  res <- rbindlist(res)

  return(res)
}

thredds_get_data <- function(year = NULL, date = NULL) {
  if (is.null(year) & is.null(date)) {
    stop("year AND date cannot be NULL")
  }
  if (!is.null(year) & !is.null(date)) {
    stop("year AND date cannot both be NOT NULL")
  }

  temp_dir <- fhi::temp_dir()
  if (!is.null(year)) {
    file <- glue::glue("seNorge2018_{year}.nc")
    url <- glue::glue("https://thredds.met.no/thredds/fileServer/senorge/seNorge_2018/Archive/{file}")
  } else {
    date <- stringr::str_remove_all(date, "-")
    file <- glue::glue("seNorge2018_{date}.nc")
    url <- glue::glue("https://thredds.met.no/thredds/fileServer/senorge/seNorge_2018/Latest/{file}")
  }
  temp_file <- fs::path(temp_dir, file)

  on.exit(fs::file_delete(temp_file))

  utils::download.file(
    url,
    temp_file
  )

  nc <- ncdf4::nc_open(temp_file)
  dates <- as.Date("1900-01-01") + ncdf4::ncvar_get(nc, "time")
  res <- thredds_get_data_internal(nc = nc, dates = dates)
  ncdf4::nc_close(nc)

  if (!is.null(year) && year == 2019) {
    file <- "seNorge2018_20190101_20190623.nc"
    url <- glue::glue("https://thredds.met.no/thredds/fileServer/senorge/seNorge_2018/Archive/{file}")
    temp_file2 <- fs::path(temp_dir, file)

    on.exit(fs::file_delete(temp_file2))

    utils::download.file(
      url,
      temp_file2
    )

    nc <- ncdf4::nc_open(temp_file2)
    dates <- as.Date("1900-01-01") + ncdf4::ncvar_get(nc, "time")
    res2 <- thredds_get_data_internal(nc = nc, dates = dates)
    ncdf4::nc_close(nc)

    res[res2, on = c("location_code", "date"), remove := 1]
    res <- res[is.na(remove)]
    res[, remove := NULL]

    res <- rbind(res2, res)
  }

  setcolorder(res, c("date", "location_code", "tg", "tx", "tn", "rr"))
  res[, forecast := FALSE]

  res[, border := config$border]

  return(res)
}

thredds_get_forecast_internal <- function(x_loc) {
  if (!x_loc %in% fd::norway_map_municips()$location_code) stop("not valid location")
  pos <- fd::norway_map_municips()[
    location_code == x_loc,
    .(
      lon = round(mean(long), 2),
      lat = round(mean(lat), 2)
    )
  ]

  a <- httr::GET(glue::glue("https://api.met.no/weatherapi/locationforecastlts/1.3/?lat={pos$lat}&lon={pos$lon}"), httr::content_type_xml())
  a <- xml2::read_xml(a$content)
  # xml2::write_xml(a, "/git/test.xml")
  baz <- xml2::xml_find_all(a, ".//maxTemperature")
  res <- vector("list", length = length(baz))
  for (i in seq_along(baz)) {
    parent <- xml2::xml_parent(baz[[i]])
    grandparent <- xml2::xml_parent(parent)
    time_from <- xml2::xml_attr(grandparent, "from")
    time_to <- xml2::xml_attr(grandparent, "to")
    x <- xml2::xml_find_all(parent, ".//minTemperature")
    temp_min <- xml2::xml_attr(x, "value")
    x <- xml2::xml_find_all(parent, ".//maxTemperature")
    temp_max <- xml2::xml_attr(x, "value")
    x <- xml2::xml_find_all(parent, ".//precipitation")
    precip <- xml2::xml_attr(x, "value")

    res[[i]] <- data.frame(
      time_from = as.character(time_from),
      time_to = as.character(time_to),
      tx = as.numeric(temp_max),
      tn = as.numeric(temp_min),
      rr = as.numeric(precip)
    )
  }
  res <- rbindlist(res)
  res <- res[stringr::str_sub(time_from, 12, 13) %in% c("00", "06", "12", "18")]
  res[, date := as.Date(stringr::str_sub(time_from, 1, 10))]
  res[, N := .N, by = date]
  res <- res[N == 4]
  res <- res[, .(
    tg = NA,
    tx = max(tx),
    tn = min(tn),
    rr = sum(rr)
  ),
  keyby = .(date)
  ]
  res[, forecast := TRUE]
  res[, location_code := x_loc]

  return(res)
}

thredds_get_forecast <- function() {
  res <- vector("list", length = nrow(fd::norway_locations()))
  pb <- fhi::txt_progress_bar(max = length(res))
  for (i in seq_along(res)) {
    utils::setTxtProgressBar(pb, i)
    res[[i]] <- thredds_get_forecast_internal(x_loc = fd::norway_locations()$municip_code[i])
  }
  res <- rbindlist(res)

  res[, border := config$border]

  return(res)
}

#' data_weather
#' Updates the weather db tables
#' @export
#'
#' 
data_weather <- function(data, argset, schema) {

  weather <- schema$output

  val <- weather$dplyr_tbl() %>%
    dplyr::filter(forecast == 0) %>%
    dplyr::summarize(last_date = max(date, na.rm = T)) %>%
    dplyr::collect() %>%
    latin1_to_utf8()

  download_dates <- NULL
  download_years <- NULL

  if (is.na(val$last_date)) {
    download_years <- 2000:lubridate::year(lubridate::today())
  } else {
    last_date <- lubridate::today() - 1

    if (val$last_date >= last_date) {
      # do nothing
    } else if (val$last_date >= (last_date - 28)) {
      download_dates <- seq.Date(val$last_date, last_date, by = 1)
      download_dates <- as.character(download_dates)
    } else {
      download_years <- lubridate::year(val$last_date):lubridate::year(lubridate::today())
    }
  }

  if (!is.null(download_dates)) {
    for (i in download_dates) {
      msg(glue::glue("Downloading weather for {i}"))
      d <- thredds_get_data(date = i)
      d <- add_date_info(d)
      weather$db_upsert_load_data_infile(d)
    }
  }

  if (!is.null(download_years)) {
    for (i in download_years) {
      msg(glue::glue("Downloading weather for {i}"))
      d <- thredds_get_data(year = i)
      d <- add_date_info(d)
      weather$db_upsert_load_data_infile(d)
    }
  }

  if (!is.null(download_dates) | !is.null(download_years)) {
    d <- thredds_get_forecast()
    d <- add_date_info(d)
    weather$db_upsert_load_data_infile(d)
  }

  val <- weather$dplyr_tbl() %>%
    dplyr::summarize(last_date = max(date, na.rm = T)) %>%
    dplyr::collect() %>%
    latin1_to_utf8()
}


add_date_info <- function(da){
  dates <- unique(da[, "date", with = F])
  dates[, datex := date]
  dates[, yrwk := format.Date(datex, "%G-%V")] # Week-based year, instead of normal year (%Y)
  dates[, week := as.numeric(format.Date(datex, "%V"))]
  dates[, year := as.numeric(format.Date(date, "%G"))]
  dates[, month := as.numeric(format.Date(date, "%m"))]
  dates[, season := fhi::season(yrwk)]
  dates[, x := fhi::x(week)]
  da <- merge(da, dates, by = "date")
  da[, granularity_time:="daily"]
  da[, granularity_geo:="municip"]
  return(da)
}
