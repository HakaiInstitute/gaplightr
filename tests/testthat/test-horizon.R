test_that("gla_extract_horizon_terra validates inputs", {
  # Test error handling for missing/invalid inputs
  dem_rast <- terra::rast(matrix(1:100, 10, 10))

  expect_error(
    gla_extract_horizon_terra(
      dem_rast = "not_a_raster",
      x_meters = 1000,
      y_meters = 2000,
      camera_height_m = 1.37
    ),
    "dem_rast must be a SpatRaster object"
  )

  expect_error(
    gla_extract_horizon_terra(
      dem_rast = dem_rast,
      y_meters = 2000,
      camera_height_m = 1.37
    ),
    "Both x_meters and y_meters must be provided"
  )

  expect_error(
    gla_extract_horizon_terra(
      dem_rast = dem_rast,
      x_meters = "invalid",
      y_meters = 2000,
      camera_height_m = 1.37
    ),
    "x_meters and y_meters must be numeric"
  )
})

test_that("gla_extract_horizon_terra accepts custom parameters", {
  # Create test fixture on-demand
  dem_path <- withr::local_tempfile(fileext = ".tif")
  create_test_dem(crs = 3005, output_path = dem_path)
  dem_rast <- terra::rast(dem_path)

  # Test with custom parameters (use fixture coordinates)
  horizon_custom <- gla_extract_horizon_terra(
    dem_rast = dem_rast,
    x_meters = 1000500,
    y_meters = 500500,
    step = 10, # Larger step
    max_search_distance = 100, # Limited radius
    distance_step = 5, # Larger sampling step
    camera_height_m = 1.37,
    verbose = FALSE
  )

  # Should have fewer rows with step=10
  expect_equal(nrow(horizon_custom), 360 / 10)
  expect_equal(min(horizon_custom$azimuth), 0)
  expect_equal(max(horizon_custom$azimuth), 360 - 10)
})

test_that("higher camera_height_m produces lower horizon angles", {
  # Create test DEM with a hill in the center
  dem_path <- withr::local_tempfile(fileext = ".tif")
  create_test_dem(crs = 3005, output_path = dem_path)
  dem_rast <- terra::rast(dem_path)

  # Observer at edge of DEM looking toward the central hill
  obs_x <- 1000200
  obs_y <- 500500

  # Compute horizon at ground level
  horizon_ground <- gla_extract_horizon_terra(
    dem_rast = dem_rast,
    x_meters = obs_x,
    y_meters = obs_y,
    step = 10,
    camera_height_m = 0
  )

  # Compute horizon at elevated camera (10m above ground)
  horizon_elevated <- gla_extract_horizon_terra(
    dem_rast = dem_rast,
    x_meters = obs_x,
    y_meters = obs_y,
    step = 10,
    camera_height_m = 10
  )

  # Higher camera should see terrain at lower angles (or equal for flat areas).
  # At least one azimuth should show a decrease where the hill is visible.

  angle_diff <- horizon_ground$horizon_height - horizon_elevated$horizon_height
  expect_true(
    any(angle_diff > 0),
    info = "Expected at least one azimuth with lower horizon angle for elevated camera"
  )

  # All differences should be non-negative (elevated camera cannot see higher horizons)
  expect_true(
    all(angle_diff >= -0.001), # small tolerance for floating point
    info = "Elevated camera should not see higher horizon angles"
  )
})

test_that("gla_extract_horizon_terra handles point outside DEM extent", {
  # Create test fixture on-demand
  dem_path <- withr::local_tempfile(fileext = ".tif")
  create_test_dem(crs = 3005, output_path = dem_path)
  dem_rast <- terra::rast(dem_path)

  # Point way outside DEM extent (far from 1000, 2000)
  expect_error(
    gla_extract_horizon_terra(
      dem_rast = dem_rast,
      x_meters = 999999, # Far outside
      y_meters = 999999, # Far outside
      step = 30,
      camera_height_m = 1.37
    ),
    regexp = "outside DEM extent|no data"
  )
})

test_that("gla_extract_horizons validates sf object", {
  # Create test fixture on-demand
  dem_path <- withr::local_tempfile(fileext = ".tif")
  create_test_dem(crs = 3005, output_path = dem_path)

  # Create non-sf object
  points_data <- data.frame(
    x_meters = 1000500,
    y_meters = 500500
  )

  expect_error(
    gla_extract_horizons(
      points = points_data,
      dem_path = dem_path,
      output_dir = withr::local_tempdir()
    ),
    regexp = "must be an sf object"
  )
})


test_that("gla_extract_horizons extracts coordinates from geometry", {
  # Create test fixture on-demand
  dem_path <- withr::local_tempfile(fileext = ".tif")
  create_test_dem(crs = 3005, output_path = dem_path)

  points_data <- data.frame(x_meters = 1000500, y_meters = 500500)

  stream_points <- sf::st_as_sf(
    points_data,
    coords = c("x_meters", "y_meters"),
    crs = 3005
  )
  stream_points$point_id <- 1L

  # Should not error - coordinates extracted from geometry
  expect_no_error(
    stream_points <- gla_extract_horizons(
      points = stream_points,
      dem_path = dem_path,
      output_dir = withr::local_tempdir(),
      step = 30,
      max_search_distance = 1000,
      parallel = FALSE
    )
  )

  # Should have horizon_mask column
  expect_true("horizon_mask" %in% names(stream_points))
})


test_that("gla_extract_horizons handles missing DEM file", {
  dem_path <- "nonexistent_dem.tif"

  # Create minimal points manually
  points_data <- data.frame(
    x_meters = 1000500,
    y_meters = 500500,
    elevation = 105,
    lat = 50.0,
    lon = -125.0
  )

  stream_points <- sf::st_as_sf(
    points_data,
    coords = c("x_meters", "y_meters"),
    crs = 3005
  )
  coords <- sf::st_coordinates(stream_points)
  stream_points$x_meters <- coords[, 1]
  stream_points$y_meters <- coords[, 2]
  stream_points$point_id <- 1L

  # Should error when trying to load DEM
  expect_error(
    gla_extract_horizons(
      points = stream_points,
      dem_path = dem_path,
      output_dir = withr::local_tempdir()
    ),
    regexp = "DEM file not found"
  )
})

test_that("gla_create_fisheye_photos handles missing/invalid LAS files", {
  # Create test fixtures on-demand
  dem_path <- withr::local_tempfile(fileext = ".tif")
  create_test_dem(crs = 3005, output_path = dem_path)

  stream_network_path <- withr::local_tempfile(fileext = ".gpkg")
  create_test_points(
    crs = 3005,
    n_points = 2,
    output_path = stream_network_path
  )

  output_dir <- withr::local_tempdir()

  stream_points <- gla_load_points(
    stream_network_path,
    dem_path
  )

  # Extract horizons first
  stream_points <- gla_extract_horizons(
    stream_points,
    dem_path,
    output_dir = withr::local_tempdir()
  )

  # Set invalid LAS file paths
  stream_points$las_files <- c(
    "nonexistent_file1.las",
    "nonexistent_file2.las"
  )

  expect_error(
    gla_create_fisheye_photos(
      points = stream_points,
      output_dir = output_dir,
      parallel = FALSE
    ),
    regexp = "missing, non-existent, or empty LAS files"
  )
})


test_that("gla_process_fisheye_photos handles corrupted image file", {
  # Create a fake/corrupted BMP file
  corrupted_bmp <- withr::local_tempfile(fileext = ".bmp")
  writeLines("This is not a valid BMP file", corrupted_bmp)

  # Create minimal points with corrupted image path
  points_data <- data.frame(
    x_meters = 1000500,
    y_meters = 500500,
    elevation = 105,
    lat = 50.0,
    lon = -125.0,
    fisheye_photo_path = corrupted_bmp
  )

  stream_points <- sf::st_as_sf(
    points_data,
    coords = c("x_meters", "y_meters"),
    crs = 3005
  )
  coords <- sf::st_coordinates(stream_points)
  stream_points$x_meters <- coords[, 1]
  stream_points$y_meters <- coords[, 2]

  expect_error(
    gla_process_fisheye_photos(
      points = stream_points,
      parallel = FALSE
    ),
    regexp = "missing or corrupted BMP"
  )
})

test_that("gla_process_fisheye_photos validates required columns", {
  # Create points missing required columns
  points_data <- data.frame(
    x_meters = 1000500,
    y_meters = 500500
    # Missing: fisheye_photo_path, lat, lon, elevation
  )

  stream_points <- sf::st_as_sf(
    points_data,
    coords = c("x_meters", "y_meters"),
    crs = 3005
  )

  expect_error(
    gla_process_fisheye_photos(points = stream_points),
    regexp = "must contain columns"
  )
})

test_that("gla_process_fisheye_photos errors on already processed points", {
  test_photo <- test_path(
    "testdata",
    "CP38_ps10_cex0pt3_600dpi_2800px_polar_cairo.bmp"
  )

  # Create points with solar radiation columns already present
  already_processed <- create_test_photo_points(
    fisheye_photo_path = test_photo,
    stream = "CP38",
    canopy_openness_pct = 45.5, # Already processed
    transmitted_global_irradiation_MJm2d = 12.3,
    light_penetration_index = 0.56
  )

  expect_error(
    gla_process_fisheye_photos(points = already_processed, parallel = FALSE),
    regexp = "already processed"
  )
})
