#' Load points
#'
#' @param x Either a file path to any file that `sf::read_sf` can read, or an
#'   sf object containing point geometries
#' @param dem Either a file path to a DEM raster file, or a `SpatRaster` object
#' @param ... Additional arguments passed to `sf::read_sf()` when `x` is a file path
#' @export
gla_load_points <- function(x, dem, ...) {
  # Handle both file paths and sf objects
  if (inherits(x, "sf")) {
    points <- x
  } else if (is.character(x) && length(x) == 1) {
    points <- sf::read_sf(x, ...)
  } else {
    stop("x must be either a file path (character) or an sf object")
  }

  # Process points with DEM (common logic)
  process_points_internal(points, dem)
}

# Internal helper - no documentation needed
process_points_internal <- function(points, dem) {
  # Validate geometry type
  if (!all(sf::st_geometry_type(points) == "POINT")) {
    stop("object must contain only POINT geometries")
  }

  # Drop existing columns that will be recalculated
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

  # Load DEM if needed (terra::rast for paths, use directly if SpatRaster)
  if (inherits(dem, "SpatRaster")) {
    dem_rast <- dem
  } else {
    dem_rast <- terra::rast(dem)
  }
  dem_crs <- sf::st_crs(terra::crs(dem_rast))

  validate_crs_match(pts_crs, dem_crs, "Points", "DEM")

  # Extract elevation from DEM
  points$elevation <- terra::extract(dem_rast, points)[[2]]

  # Extract coordinates (validated to be in meters)
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
