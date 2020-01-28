#' get_location_name
#' @import data.table
#' @param location_code a location code
#' @export
get_location_name <- function(location_code) {
  location_name <- NULL
  return(norway_locations_long()[location_code, on = "location_code", location_name])
}

#' norway_locations
#' @export
norway_locations <- function () 
{
    if (config$border == 2019) {
        return(fhidata::norway_locations_b2019)
    }
    else if (config$border == 2020) {
        return(fhidata::norway_locations_b2020)
    }
}

#'get_county_code
#' 
#' @export
get_county_code <- function(municip){

  return(paste0("county", substr(municip, 8, 9)))
}

#'get_municips_same_county
#' 
#' @export
get_municips_same_county <- function(municip){
  x_county_code <- paste0("county", substr(municip, 8, 9))
  return(norway_locations()[county_code == x_county_code, municip_code])

}
#' get_municips_county
#' 
#' @export
get_municips_county <- function(x_county_code){
  return(norway_locations()[county_code == x_county_code, municip_code])

}
