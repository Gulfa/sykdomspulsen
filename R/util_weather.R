#' get_weather
#' Gets the weather, population weighted at county and national levels
#' @param impute_missing Do you want missing data imputed?
#' @export
get_weather <- function(impute_missing = FALSE) {
  
  temp <- config$schema$data_weather$dplyr_tbl() %>%
    dplyr::collect() %>%
    fd::latin1_to_utf8()

  if (impute_missing) {
    fit <- lme4::lmer(tx ~ tg + (1 | location_code), data = temp)
    temp[, tx_pred := stats::predict(fit, newdata = temp)]
    temp[is.na(tx) & !is.na(tx_pred), tx := tx_pred]
    temp[, tx_pred := NULL]

    fit <- lme4::lmer(tn ~ tg + (1 | location_code), data = temp)
    temp[, tn_pred := stats::predict(fit, newdata = temp)]
    temp[is.na(tn) & !is.na(tn_pred), tn := tn_pred]
    temp[, tn_pred := NULL]

    fit <- lme4::lmer(tg ~ tx + tn + (1 | location_code), data = temp)
    temp[, tg_pred := stats::predict(fit, newdata = temp)]
    temp[is.na(tg) & !is.na(tg_pred), tg := tg_pred]
    temp[, tg_pred := NULL]
  }

  temp[, year := data.table::year(date)]
  to_merge <- fd::norway_fixing_merged_municips()[, c(
    "municip_code_original",
    "municip_code_current",
    "year",
    "border_start",
    "weighting"
  )]
  temp <- merge(
    temp,
    to_merge,
    by.x = c("location_code", "year", "border"),
    by.y = c("municip_code_original", "year", "border_start"),
    all.x = T
  )

  temp <- temp[, .(
    tg = mean(tg * weighting),
    tx = mean(tx * weighting),
    tn = mean(tn * weighting),
    rr = mean(rr * weighting),
    forecast = max(forecast)
  ), keyby = .(
    location_code = municip_code_current,
    date
  )]

  pop <- fd::norway_population()[, .(
    pop = sum(pop)
  ), keyby = .(location_code, year)]

  temp[, year := fhi::isoyear_n(date)]
  temp[pop, on = c("location_code", "year"), pop := pop]
  temp <- temp[!is.na(pop)]

  temp[fd::norway_locations(),
    on = "location_code==municip_code",
    county_code := county_code
  ]
  temp_county <- temp[year >= 2006, .(
    tg = sum(tg * pop) / sum(pop),
    tx = sum(tx * pop) / sum(pop),
    tn = sum(tn * pop) / sum(pop),
    rr = sum(rr * pop) / sum(pop),
    forecast = max(forecast)
  ), keyby = .(county_code, date)]
  setnames(temp_county, "county_code", "location_code")

  temp_national <- temp[year >= 2006, .(
    tg = sum(tg * pop) / sum(pop),
    tx = sum(tx * pop) / sum(pop),
    tn = sum(tn * pop) / sum(pop),
    rr = sum(rr * pop) / sum(pop),
    forecast = max(forecast)
  ), keyby = .(date)]
  temp_national[, location_code := "norge"]

  temp[, year := NULL]
  temp[, pop := NULL]
  temp[, county_code := NULL]
  temp <- rbind(temp, temp_county, temp_national)

  temp[, yrwk := fhi::isoyearweek(date)]

  temp[, forecast := as.logical(forecast)]

  return(temp)
}
