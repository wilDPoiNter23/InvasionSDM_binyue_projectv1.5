# Measure polygon components only; line and point contacts have zero area.
common_polygon_area <- function(x) {
  x <- sf::st_geometry(x)
  if (!length(x)) return(0)
  x <- x[!sf::st_is_empty(x) & sf::st_dimension(x) == 2L]
  if (!length(x)) return(0)
  polygon <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
  polygon <- polygon[!sf::st_is_empty(polygon)]
  if (!length(polygon)) return(0)
  value <- as.numeric(sf::st_area(polygon))
  if (any(!is.finite(value)) || any(value < 0)) stop("Invalid polygon area")
  sum(value)
}

# Form disjoint zones in the coordinate system used for exact cell fractions.
common_native_partition <- function(city, reclaimed, buffered_city, all_cities) {
  reclaimed <- suppressWarnings(sf::st_intersection(reclaimed, city))
  nonreclaimed <- suppressWarnings(sf::st_difference(city, reclaimed))
  ring <- suppressWarnings(sf::st_difference(buffered_city, all_cities))
  pieces <- lapply(list(ring, nonreclaimed, reclaimed), function(x) {
    x <- x[!sf::st_is_empty(x) & sf::st_dimension(x) == 2L]
    if (!length(x)) stop("EMPTY_NATIVE_ZONE")
    polygon <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
    polygon <- polygon[!sf::st_is_empty(polygon)]
    if (!length(polygon)) stop("EMPTY_NATIVE_ZONE")
    sf::st_union(polygon)[[1]]
  })
  do.call(sf::st_sfc, c(pieces, list(crs=sf::st_crs(city))))
}
