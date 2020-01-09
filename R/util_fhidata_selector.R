#' norway_locations
#' @export
norway_locations <- function() {
  if (config$border == 2019) {
    return(fhidata::norway_locations_b2019)
  } else if (config$border == 2020) {
    return(fhidata::norway_locations_b2020)
  }
}

#' norway_county_merging
#' @export
norway_county_merging <- function() {
  if (config$border == 2019) {
    return(fhidata::norway_county_merging_b2019)
  } else if (config$border == 2020) {
    return(fhidata::norway_county_merging_b2020)
  }
}

#' norway_municip_merging
#' @export
norway_municip_merging <- function() {
  if (config$border == 2019) {
    return(fhidata::norway_municip_merging_b2019)
  } else if (config$border == 2020) {
    return(fhidata::norway_municip_merging_b2020)
  }
}

#' norway_fixing_merged_municips
#' @export
norway_fixing_merged_municips <- function() {
  return(fhidata::norway_fixing_merged_municips[border_end == config$border])
}

#' norway_locations
#' @export
norway_locations <- function() {
  if (config$border == 2019) {
    return(fhidata::norway_locations_b2019)
  } else if (config$border == 2020) {
    return(fhidata::norway_locations_b2020)
  }
}

#' norway_locations_long
#' @export
norway_locations_long <- function() {
  if (config$border == 2019) {
    return(fhidata::norway_locations_long_b2019)
  } else if (config$border == 2020) {
    return(fhidata::norway_locations_long_b2020)
  }
}

#' norway_population
#' @export
norway_population <- function() {
  if (config$border == 2019) {
    return(fhidata::norway_population_b2019)
  } else if (config$border == 2020) {
    return(fhidata::norway_population_b2020)
  }
}

#' norway_map_counties
#' @export
norway_map_counties <- function() {
  if (config$border == 2019) {
    return(fhidata::norway_map_counties_b2019)
  } else if (config$border == 2020) {
    return(fhidata::norway_map_counties_b2020)
  }
}

#' norway_map_municips
#' @export
norway_map_municips <- function() {
  if (config$border == 2019) {
    return(fhidata::norway_map_municips_b2019)
  } else if (config$border == 2020) {
    return(fhidata::norway_map_municips_b2020)
  }
}

#' norway_map_counties_label_positions
#' @export
norway_map_counties_label_positions <- function() {
  if (config$border == 2019) {
    return(fhidata::norway_map_counties_label_positions_b2019)
  } else if (config$border == 2020) {
    return(fhidata::norway_map_counties_label_positions_b2020)
  }
}

#' senorge
#' @export
senorge <- function() {
  if (config$border == 2019) {
    return(fhidata::senorge_b2019)
  } else if (config$border == 2020) {
    return(fhidata::senorge_b2020)
  }
}
