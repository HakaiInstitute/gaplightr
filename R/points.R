#' Load points
#'
#' @param path path to a any files that `sf::read_sf` can read
#' @param dem_path a dem path
#' @export
gla_load_points <- function(path, dem_path) {
  points <- sf::read_sf(path)

  if (!all(sf::st_geometry_type(points) == "POINT")) {
    stop("object must contain only POINT geometries")
  }

  if (any(toupper(names(points)) %in% c("LAT", "LON"))) {
    message(paste0("Dropping existing LAT/LON columns and recalculating"))
    points <- points[, !toupper(names(points)) %in% c("LAT", "LON")]
  }

  if (any(toupper(names(points)) %in% c("X_METERS", "Y_METERS"))) {
    message(paste0(
      "Dropping existing X_METERS/Y_METERS columns and recalculating"
    ))
    points <- points[, !toupper(names(points)) %in% c("X_METERS", "Y_METERS")]
  }

  # Validate CRS
  pts_crs <- sf::st_crs(points)
  if (is.na(pts_crs)) {
    stop(
      "Input shapefile has no CRS defined. Please assign a CRS before loading."
    )
  }

  # Check if CRS is projected (not geographic)
  if (sf::st_is_longlat(points)) {
    stop(
      "Input shapefile is in geographic coordinates (lat/lon).\n",
      "This function requires projected coordinates in meters.\n",
      "Current CRS: ",
      pts_crs$input
    )
  }

  # Load DEM and validate CRS matches points
  dem <- terra::rast(dem_path)
  dem_crs <- sf::st_crs(terra::crs(dem))

  validate_crs_match(pts_crs, dem_crs, "Points", "DEM")

  # Extract elevation from DEM (now using dem object we already loaded)
  points$elevation <- terra::extract(dem, points)[[2]]

  # Extract coordinates (now validated to be in meters)
  points$x_meters <- round(sf::st_coordinates(points)[, "X"], 0)
  points$y_meters <- round(sf::st_coordinates(points)[, "Y"], 1)

  check_if_coordinates_are_unique(points)

  # Transform to WGS84 for lat/lon output columns
  coords_wgs84 <- sf::st_transform(points, crs = 4326)
  points$lon <- sf::st_coordinates(coords_wgs84)[, "X"]
  points$lat <- sf::st_coordinates(coords_wgs84)[, "Y"]

  # Move geometry column to end
  move_geom_col_to_end(points)
}
