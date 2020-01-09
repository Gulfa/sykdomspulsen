#' get_location_name
#' @import data.table
#' @param location_code a location code
#' @export
get_location_name <- function(location_code) {
  location_name <- NULL
  return(norway_locations_long()[location_code, on = "location_code", location_name])
}
