#'
#' @export
ui_archive_results <- function(data, argset, schema){
  latest_year_query <- schema$input$dplyr_tbl() %>%
                               dplyr::summarise(max(year, na.rm = TRUE)) %>%
                               dplyr::collect()
  latest_year <- latest_year_query[[1]]
  lower <- latest_year - argset$years
  data <- schema$input$dplyr_tbl() %>%
                  dplyr::filter(year > lower) %>%
                  dplyr::collect()
  setDT(data)
  fs::dir_create(path("archive", argset$folder, argset$today))
  saveRDS(data, file = path("archive", argset$folder, argset$today, "latest_data.RDS"))
}
