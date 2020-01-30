
#' Calculate completeness
#'
#' @param location location to calculate for
#' @param x_year to calculate for
#' @param x_granularity_time granularity_time
#' @param table db table
#'
#' @import data.table
#'
#' @export
calculate_completeness <- function(location, x_year, x_granularity_time = "weekly", table = NULL) {
  if (is.null(table)) {
    table <- fd::tbl("results_qp")
  }

  results <- table %>%
    dplyr::filter(location_code == location &
      year == x_year &
      granularity_time == x_granularity_time &
      tag_outcome == "consult_without_influenza" &
      age == "Totalt") %>%
    dplyr::collect()
  setDT(results)
  results[, completeness := n / threshold0 * 100]
  results[ completeness > 100, completeness := 100]

  return(results)
}


#' Calculate Confidence Interval
#'
#' @param data the data to include
#' @param last_weeks if we only inlcude uncertainty for the last weeks
#'
#' @import data.table
#'
#' @export
calculate_confidence_interval <- function(data, last_weeks = NULL) {
  setDT(data)
  table <- fd::tbl("results_qp")

  N <- 10
  if (!is.null(last_weeks)) {
    N <- last_weeks
  }
  cat(file=stderr(), nrow(data))
  N <- min(nrow(data), N)
  location <- data[1, location_code]
  yrwks <- tail(data[, yrwk], N)
  x_granularity_time <- data[1, granularity_time]
  x_age <- data[1, age]

  results <- table %>%
    dplyr::filter(location_code == location &
      yrwk %in% yrwks &
      granularity_time == x_granularity_time &
      tag_outcome == "consult_without_influenza" &
      age == x_age) %>%
    dplyr::collect()
  setDT(results)
  results[, completeness := n / n_baseline_expected]
  results[ completeness > 1, completeness := 1]
  population <- results[, n] / pmax(results[, completeness], 1e-5)
  cis <- list()
  for (i in 1:N) {
    data_i <- nrow(data) - N + i
    denom <- data[data_i, n_denominator]
    pop <- population[i]

    if (!is.na(pop) && pop > 0) {
      if (pop < 100000) {
        if (pop != denom) {
          CI <- samplingbook::Sprop(m = data[data_i, n], n = denom, N = population[i])$ci$exact
        } else {
          CI <- c(data[data_i, n] / denom, data[data_i, n] / denom)
        }
        cis[[i]] <- list(
          yrwk = data[data_i, yrwk], phat = data[data_i, n] / denom,
          low_p = CI[1], high_p = CI[2],
          low_n = CI[1] * denom, high_n = CI[2] * denom
        )
      } else {
        CI <- asbio::ci.p(phat = data[data_i, n] / denom, n = denom, N = population[i], summarized = T, fpc = T)$ci
        cis[[i]] <- list(
          yrwk = data[data_i, yrwk], phat = CI[1],
          low_p = CI[2], high_p = CI[3],
          low_n = denom * CI[2], high_n = denom * CI[3]
        )
      }
    } else {
      cis[[i]] <- list(yrwk = data[data_i, yrwk], phat = 0, low_p = 0, high_p = 0, low_n = 0, high_n = 5)
    }
  }

  cis <- rbindlist(cis)
  cis[, completeness := results[, completeness]]
  cis[low_n < 0, low_n := 0]
  ret <- cis[data, on = "yrwk"]

  return(ret)
}
