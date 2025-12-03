# Test setup - runs once before all tests
# Provides fixture factory functions for on-demand test data generation

# Define coordinate extents for different CRS
# These are typical coordinate ranges for each region in their respective CRS
get_coordinate_extent <- function(epsg_code) {
  if (epsg_code == 3005) {
    # BC Albers - typical BC coordinates
    list(x_min = 1000000, y_min = 500000)
  } else if (epsg_code == 26912) {
    # UTM Zone 12N - Montana/Idaho coordinates
    # Example: Missoula, MT area
    list(x_min = 700000, y_min = 5150000)
  } else {
    stop(
      "Unsupported CRS: ",
      epsg_code,
      "\nCurrently supports: 3005 (BC Albers), 26912 (UTM Zone 12N)"
    )
  }
}

#' Create minimal DEM test fixture
#'
#' @param crs EPSG code (3005 or 26912)
#' @param nrows Number of rows in raster (default 100)
#' @param ncols Number of columns in raster (default 100)
#' @param output_path Path where DEM should be written
#' @return Path to created DEM file
create_test_dem <- function(
  crs = 3005,
  nrows = 100,
  ncols = 100,
  output_path = NULL
) {
  # Get appropriate coordinates for this CRS
  extent_info <- get_coordinate_extent(crs)

  # Create coordinate grid (10m resolution)
  x <- seq(extent_info$x_min, extent_info$x_min + (ncols - 1) * 10, by = 10)
  y <- seq(extent_info$y_min, extent_info$y_min + (nrows - 1) * 10, by = 10)

  # Create elevation values: mostly flat (100m) with a hill in the middle
  elev_matrix <- matrix(100, nrow = nrows, ncol = ncols)

  # Add a simple hill
  center_x <- ncols / 2
  center_y <- nrows / 2
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      dist <- sqrt((i - center_y)^2 + (j - center_x)^2)
      if (dist < 20) {
        # Gaussian-like hill
        elev_matrix[i, j] <- 100 + 50 * exp(-dist^2 / 200)
      }
    }
  }

  # Create raster
  dem <- terra::rast(
    elev_matrix,
    extent = terra::ext(min(x), max(x), min(y), max(y)),
    crs = paste0("EPSG:", crs)
  )

  names(dem) <- "elevation"

  # Determine output path
  if (is.null(output_path)) {
    output_path <- test_path(
      "testdata",
      paste0("minimal_dem_", crs, ".tif")
    )
  }

  terra::writeRaster(dem, output_path, overwrite = TRUE)

  output_path
}

#' Create minimal LAS test fixture
#'
#' @param crs EPSG code (3005 or 26912)
#' @param n_points Number of points to generate (default 100)
#' @param output_path Path where LAS file should be written
#' @return Path to created LAS file
create_test_las <- function(crs = 3005, n_points = 100, output_path = NULL) {
  set.seed(123)

  # Get appropriate coordinates for this CRS
  extent_info <- get_coordinate_extent(crs)

  # Create points around center of DEM
  center_x <- extent_info$x_min + 500
  center_y <- extent_info$y_min + 500

  # Random points within 50m radius
  angles <- runif(n_points, 0, 2 * pi)
  radii <- runif(n_points, 0, 50)

  x_coords <- center_x + radii * cos(angles)
  y_coords <- center_y + radii * sin(angles)
  z_coords <- runif(n_points, 110, 140) # Heights above ground

  # Create classification (2 = ground, 5 = vegetation)
  classification <- as.integer(sample(
    c(2, 5),
    n_points,
    replace = TRUE,
    prob = c(0.2, 0.8)
  ))

  # Generate consistent return numbers
  number_of_returns <- as.integer(sample(1:3, n_points, replace = TRUE))
  return_number <- sapply(number_of_returns, function(n) sample(1:n, 1))

  # Create LAS object (suppress lidR messages about header)
  las_data <- data.frame(
    X = x_coords,
    Y = y_coords,
    Z = z_coords,
    Classification = classification,
    Intensity = as.integer(runif(n_points, 0, 65535)),
    ReturnNumber = as.integer(return_number),
    NumberOfReturns = as.integer(number_of_returns)
  )

  las <- suppressMessages(lidR::LAS(las_data))
  lidR::projection(las) <- crs # Set CRS using lidR projection

  # Determine output path
  if (is.null(output_path)) {
    las_dir <- test_path("testdata", as.character(crs))

    # Create directory if it doesn't exist
    if (!dir.exists(las_dir)) {
      dir.create(las_dir, recursive = TRUE)
    }

    output_path <- file.path(las_dir, paste0("minimal_plot_", crs, ".las"))
  }

  lidR::writeLAS(las, output_path)

  output_path
}

#' Create minimal point test fixture
#'
#' @param crs EPSG code (3005 or 26912)
#' @param n_points Number of points to generate (default 2)
#' @param output_path Path where points should be written
#' @return Path to created points file
create_test_points <- function(crs = 3005, n_points = 2, output_path = NULL) {
  # Get appropriate coordinates for this CRS
  extent_info <- get_coordinate_extent(crs)

  # Generate points in the middle of our DEM
  x_coords <- seq(
    extent_info$x_min + 500,
    extent_info$x_min + 500 + (n_points - 1) * 20,
    length.out = n_points
  )
  y_coords <- seq(
    extent_info$y_min + 500,
    extent_info$y_min + 500 + (n_points - 1) * 20,
    length.out = n_points
  )

  # Convert to sf object
  points <- sf::st_as_sf(
    data.frame(
      x_meters = x_coords,
      y_meters = y_coords
    ),
    coords = c("x_meters", "y_meters"),
    crs = crs
  )

  # Determine output path
  if (is.null(output_path)) {
    output_path <- test_path(
      "testdata",
      paste0("minimal_stream_network_", crs, ".gpkg")
    )
  }

  sf::st_write(points, output_path, delete_dsn = TRUE, quiet = TRUE)

  output_path
}

#' Create test sf point dataframe for photo processing tests
#'
#' @param fisheye_photo_path Path to fisheye photo (required)
#' @param stream Stream identifier (default "R2D2")
#' @param x_meters X coordinate in meters (default 1022655)
#' @param y_meters Y coordinate in meters (default 574704)
#' @param lat Latitude in degrees (default 50.1876)
#' @param lon Longitude in degrees (default -125.6827)
#' @param elevation Elevation in meters (default 238.44)
#' @param crs CRS code (default 3005)
#' @param ... Additional columns to add to the dataframe
#' @return sf object with point geometry
create_test_photo_points <- function(
  fisheye_photo_path,
  stream = "R2D2",
  x_meters = 1022655,
  y_meters = 574704,
  lat = 50.1876,
  lon = -125.6827,
  elevation = 238.44,
  crs = 3005,
  ...
) {
  # Build base dataframe
  point_data <- data.frame(
    stream = stream,
    x_meters = x_meters,
    y_meters = y_meters,
    lat = lat,
    lon = lon,
    elevation = elevation,
    fisheye_photo_path = fisheye_photo_path,
    ...
  )

  # Convert to sf
  sf::st_as_sf(
    point_data,
    coords = c("x_meters", "y_meters"),
    crs = crs
  )
}
