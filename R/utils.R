# Utility functions for the 50-watersheds-analysis package

#' Validate CRS match between two spatial objects
#'
#' Strict CRS validation to prevent spatial errors. Checks that both objects
#' have defined CRS and that they match exactly.
#'
#' @param obj1_crs CRS of first object (from sf::st_crs())
#' @param obj2_crs CRS of second object (from sf::st_crs())
#' @param obj1_name Name of first object for error message
#' @param obj2_name Name of second object for error message
#'
#' @return NULL (invisibly) if validation passes, otherwise stops with error
validate_crs_match <- function(obj1_crs, obj2_crs, obj1_name, obj2_name) {
  # Check if either CRS is NA/undefined
  if (is.na(obj1_crs)) {
    stop(
      obj1_name,
      " has no CRS defined.\n",
      "Please assign a CRS to your ",
      tolower(obj1_name),
      " before using this function.",
      call. = FALSE
    )
  }

  if (is.na(obj2_crs)) {
    stop(
      obj2_name,
      " has no CRS defined.\n",
      "Please assign a CRS to your ",
      tolower(obj2_name),
      " before using this function.",
      call. = FALSE
    )
  }

  # Check if CRS match
  if (!obj1_crs == obj2_crs) {
    stop(
      "CRS mismatch between ",
      obj1_name,
      " and ",
      obj2_name,
      ".\n",
      "  ",
      obj1_name,
      " CRS: ",
      obj1_crs$input,
      "\n",
      "  ",
      obj2_name,
      " CRS: ",
      obj2_crs$input,
      "\n",
      "Please reproject your data so they match before calling this function.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

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

## Internal helper to parse LAS filenames in format: X_Y.las
## Returns data frame with x_meters, y_meters, and filename columns
parse_las_filenames <- function(las_files) {
  file_info <- data.frame(do.call(
    rbind,
    strsplit(tools::file_path_sans_ext(basename(las_files)), "_")
  ))
  colnames(file_info) <- c("x_meters", "y_meters")
  file_info$las_files <- las_files
  file_info$x_meters <- as.numeric(file_info$x_meters)
  file_info$y_meters <- as.numeric(file_info$y_meters)

  file_info
}

add_las_filename <- function(stream_points, las_files) {
  # Check if las_files column already exists
  if ("las_files" %in% names(stream_points)) {
    # Remove existing column to avoid duplication
    stream_points <- stream_points[,
      !names(stream_points) %in% c("las_files")
    ]
  }
  file_info <- parse_las_filenames(las_files)

  stream_points <- merge(
    stream_points,
    file_info,
    by = c("x_meters", "y_meters")
  )

  class(stream_points) <- c("sf", "tbl_df", "tbl", "data.frame")
  stream_points
}

write_points_gpkg <- function(points, output_dir, prefix = "stream_points") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_file <- file.path(output_dir, paste0(prefix, "_", timestamp, ".gpkg"))

  tryCatch(
    {
      sf::write_sf(points, output_file)
      message("Results saved to ", output_file)
    },
    error = function(e) {
      message("Failed to write points: ", e$message)
    }
  )
}


move_geom_col_to_end <- function(sf_object) {
  geom_col <- attr(sf_object, "sf_column")
  other_cols <- setdiff(names(sf_object), geom_col)
  sf_object[, c(other_cols, geom_col)]
}

validate_required_columns <- function(
  data,
  required_cols,
  data_name = "points",
  hint = NULL
) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    msg <- paste0(
      data_name,
      " must contain columns: ",
      paste(missing_cols, collapse = ", ")
    )
    if (!is.null(hint)) {
      msg <- paste0(msg, ". ", hint)
    }
    stop(msg, call. = FALSE)
  }
  invisible(NULL)
}
