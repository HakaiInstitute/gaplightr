#!/usr/bin/env Rscript
# Script to create minimal test fixtures for end-to-end testing
# Run this once to generate test data files
#
# Usage:
#   Rscript create_minimal_fixtures.R [--crs EPSG_CODE] [--suffix SUFFIX]
#
# Examples:
#   Rscript tests/testthat/testdata/create_minimal_fixtures.R --crs 3005 --suffix ""          # BC Albers (default)
#   Rscript tests/testthat/testdata/create_minimal_fixtures.R --crs 26912 --suffix "_utm12n"  # Montana/Idaho

library(terra)
library(lidR)
library(sf)

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

# Create minimal DEM (100x100 pixels, simple terrain)
# 10m resolution, projected coordinates
create_minimal_dem <- function(target_crs, suffix) {
  # Create a simple elevation grid: flat with a small hill
  nrows <- 100
  ncols <- 100

  # Get appropriate coordinates for this CRS
  extent_info <- get_coordinate_extent(target_crs)

  # Create coordinate grid
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
  dem <- rast(
    elev_matrix,
    extent = ext(min(x), max(x), min(y), max(y)),
    crs = paste0("EPSG:", target_crs)
  )

  names(dem) <- "elevation"

  output_path <- paste0(
    "tests/testthat/testdata/minimal_dem_",
    target_crs,
    ".tif"
  )
  writeRaster(dem, output_path, overwrite = TRUE)
  cat("Created minimal DEM:", output_path, "\n")

  output_path
}

# Create minimal LAS file (small point cloud)
create_minimal_las <- function(target_crs) {
  set.seed(123)

  # Get appropriate coordinates for this CRS
  extent_info <- get_coordinate_extent(target_crs)

  # Create 100 points around center of DEM
  n_points <- 100
  center_x <- extent_info$x_min + 500
  center_y <- extent_info$y_min + 500

  # Random points within 50m radius
  angles <- runif(n_points, 0, 2 * pi)
  radii <- runif(n_points, 0, 50)

  X <- center_x + radii * cos(angles)
  Y <- center_y + radii * sin(angles)
  Z <- runif(n_points, 110, 140) # Heights above ground

  # Create classification (2 = ground, 5 = vegetation)
  Classification <- as.integer(sample(
    c(2, 5),
    n_points,
    replace = TRUE,
    prob = c(0.2, 0.8)
  ))

  # Create LAS object
  # Generate consistent return numbers
  NumberOfReturns <- as.integer(sample(1:3, n_points, replace = TRUE))
  ReturnNumber <- sapply(NumberOfReturns, function(n) sample(1:n, 1))

  las_data <- data.frame(
    X = X,
    Y = Y,
    Z = Z,
    Classification = Classification,
    Intensity = as.integer(runif(n_points, 0, 65535)),
    ReturnNumber = as.integer(ReturnNumber),
    NumberOfReturns = as.integer(NumberOfReturns)
  )

  las <- LAS(las_data)
  lidR::projection(las) <- target_crs # Set CRS using lidR projection

  las_dir <- file.path("tests/testthat/testdata/", target_crs)

  # Create directory if it doesn't exist
  if (!dir.exists(las_dir)) {
    dir.create(las_dir, recursive = TRUE)
  }

  output_path <- file.path(las_dir, paste0("minimal_plot_", target_crs, ".las"))
  writeLAS(las, output_path)
  cat("Created minimal LAS file:", output_path, "\n")

  output_path
}

# Create minimal stream network (2 points)
create_minimal_stream_network <- function(target_crs) {
  # Get appropriate coordinates for this CRS
  extent_info <- get_coordinate_extent(target_crs)

  # Two points in the middle of our DEM
  # Convert to sf object
  stream_points <- st_as_sf(
    data.frame(
      x_meters = c(extent_info$x_min + 500, extent_info$x_min + 520),
      y_meters = c(extent_info$y_min + 500, extent_info$y_min + 520)
    ),
    coords = c("x_meters", "y_meters"),
    crs = target_crs
  )

  output_path <- paste0(
    "tests/testthat/testdata/minimal_stream_network_",
    target_crs,
    ".gpkg"
  )
  st_write(stream_points, output_path, delete_dsn = TRUE, quiet = TRUE)
  cat("Created minimal stream network:", output_path, "\n")

  output_path
}

# Run all

dem_path <- create_minimal_dem(3005)
las_path <- create_minimal_las(3005)
stream_path <- create_minimal_stream_network(3005)

dem_path <- create_minimal_dem(26912)
las_path <- create_minimal_las(26912)
stream_path <- create_minimal_stream_network(26912)
