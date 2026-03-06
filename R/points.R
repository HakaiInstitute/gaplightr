#' Load points
#'
#' @param x Either a file path to any file that `sf::read_sf` can read, or an
#'   sf object containing point geometries
#' @param dem Either a file path to a DEM raster file, or a `SpatRaster` object
#' @param ... Additional arguments passed to `sf::read_sf()` when `x` is a file path
#'
#' @details
#' This function performs strict validation:
#' \itemize{
#'   \item Geometry type must be POINT
#'   \item CRS must be defined and projected (not geographic lat/lon)
#'   \item Point and DEM CRS must match exactly
#'   \item All points must fall within DEM spatial extent
#'   \item All points must have valid elevation values (no NoData cells)
#' }
#'
#' ## Point IDs
#'
#' Every point is assigned a `point_id`, a positive integer used to name all
#' downstream output files (LAS clips, horizon CSVs, fisheye photos). If `x`
#' does not contain a `point_id` column, sequential IDs are assigned
#' automatically (1, 2, 3, ...). To use your own IDs - for example to preserve
#' cached outputs across re-runs, or to match an existing site numbering scheme
#' - include a `point_id` column containing unique positive integers before
#' calling this function.
#'
#' @examples
#' \dontrun{
#'   points <- gla_load_points("stream_points.gpkg", "dem.tif")
#' }
#'
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
  dem_crs <- get_raster_crs(dem_rast)

  validate_crs_match(pts_crs, dem_crs, "Points", "DEM")

  # Validate that all points fall within DEM spatial extent before extraction
  dem_extent <- terra::ext(dem_rast)
  point_coords <- sf::st_coordinates(points)

  # Check which points are outside DEM spatial extent
  outside_x <- point_coords[, "X"] < dem_extent[1] |
    point_coords[, "X"] > dem_extent[2]
  outside_y <- point_coords[, "Y"] < dem_extent[3] |
    point_coords[, "Y"] > dem_extent[4]
  outside_bounds <- outside_x | outside_y

  if (any(outside_bounds)) {
    n_outside <- sum(outside_bounds)
    stop(
      n_outside,
      " point(s) are outside the DEM extent",
      call. = FALSE
    )
  }

  # Extract elevation from DEM
  points$elevation <- terra::extract(dem_rast, points)[[2]]

  # Check for NoData cells inside DEM extent
  na_indices <- which(is.na(points$elevation))
  if (length(na_indices) > 0) {
    stop(
      length(na_indices),
      " point(s) have NA elevation values (NoData cells in DEM)",
      call. = FALSE
    )
  }

  # Assign or validate a stable unique identifier used as the key for all
  # downstream file naming (LAS, horizon CSV, fisheye BMP). If the input already
  # carries a point_id column (e.g. from a previous run or a user-supplied ID),
  # honour it so that cached outputs remain valid across re-loads.
  if ("point_id" %in% names(points)) {
    pid <- points$point_id
    if (!is.numeric(pid)) {
      stop("Existing 'point_id' column must be numeric.", call. = FALSE)
    }
    if (anyNA(pid) || !all(is.finite(pid))) {
      stop(
        "Existing 'point_id' column contains NA or non-finite values.",
        call. = FALSE
      )
    }
    if (any(pid != floor(pid)) || any(pid <= 0)) {
      stop(
        "Existing 'point_id' values must be positive whole numbers.",
        call. = FALSE
      )
    }
    if (anyDuplicated(pid)) {
      stop(
        "Existing 'point_id' column contains duplicate values.",
        call. = FALSE
      )
    }
    message(
      "Using existing point_id column (",
      nrow(points),
      " point(s))."
    )
  } else {
    points$point_id <- seq_len(nrow(points))
    message(
      "Assigning sequential point_id (1 to ",
      nrow(points),
      ")."
    )
  }

  # Extract coordinates (validated to be in meters)
  points$x_meters <- round(sf::st_coordinates(points)[, "X"], 0)
  points$y_meters <- round(sf::st_coordinates(points)[, "Y"], 1)

  # Transform to WGS84 for lat/lon output columns
  coords_wgs84 <- sf::st_transform(points, crs = 4326)
  points$lon <- sf::st_coordinates(coords_wgs84)[, "X"]
  points$lat <- sf::st_coordinates(coords_wgs84)[, "Y"]

  # Move geometry column to end
  move_geom_col_to_end(points)
}
