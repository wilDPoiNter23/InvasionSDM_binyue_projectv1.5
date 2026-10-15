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
