test_that("lidar2hemi handles basic coordinate transformation", {
  # Test point at (1, 0, 0) - on positive X axis
  result <- lidar2hemi(1, 0, 0)
  expect_equal(result$rho, 1)
  expect_equal(result$theta, 0)
  expect_equal(result$phi, pi / 2)
})

test_that("lidar2hemi handles origin correctly", {
  # Origin should give rho=0, but phi will be NaN due to acos(z/0)
  result <- lidar2hemi(0, 0, 0)
  expect_equal(result$rho, 0)
  expect_true(is.nan(result$phi))
})

test_that("lidar2hemi handles zenith point", {
  # Point directly above (0, 0, 1)
  result <- lidar2hemi(0, 0, 1)
  expect_equal(result$rho, 1)
  expect_equal(result$phi, 0) # zenith angle = 0
  expect_equal(result$x, 0)
  expect_equal(result$y, 0)
})

test_that("lidar2hemi handles nadir point", {
  # Point directly below (0, 0, -1)
  result <- lidar2hemi(0, 0, -1)
  expect_equal(result$rho, 1)
  expect_equal(result$phi, pi) # zenith angle = 180°
})

test_that("lidar2hemi preserves distance calculation", {
  # Test various points
  x_vals <- c(3, 4, 0)
  y_vals <- c(4, 0, 5)
  z_vals <- c(0, 3, 0)

  result <- lidar2hemi(x_vals, y_vals, z_vals)
  expected_rho <- sqrt(x_vals^2 + y_vals^2 + z_vals^2)

  expect_equal(result$rho, expected_rho)
})

test_that("lidar2hemi handles negative coordinates", {
  # Test point in negative quadrant
  result <- lidar2hemi(-1, -1, 1)
  expect_equal(result$rho, sqrt(3))
  expect_equal(result$theta, atan2(-1, -1)) # Should be in 3rd quadrant
})

test_that("lidar2hemi returns data frame with correct structure", {
  result <- lidar2hemi(1, 1, 1)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("rho", "theta", "phi", "x", "y"))
  expect_equal(nrow(result), 1)
})

test_that("lidar2hemi handles vector inputs", {
  # Test with multiple points
  result <- lidar2hemi(c(1, 2), c(0, 0), c(0, 0))
  expect_equal(nrow(result), 2)
  expect_equal(result$rho, c(1, 2))
})

test_that("lidar2hemi phi values are in valid range", {
  # phi (zenith angle) should be between 0 and pi
  result <- lidar2hemi(c(1, 0, -1), c(0, 1, 0), c(1, 1, 1))
  expect_true(all(result$phi >= 0 & result$phi <= pi))
})

test_that("lidar2hemi theta values are in valid range", {
  # theta should be between -pi and pi
  result <- lidar2hemi(c(1, -1, 0), c(1, -1, 1), c(0, 0, 0))
  expect_true(all(result$theta >= -pi & result$theta <= pi))
})

test_that("lidar2hemi handles edge cases", {
  # Very small numbers
  result <- lidar2hemi(1e-10, 1e-10, 1e-10)
  expect_true(is.finite(result$rho))

  # Large numbers
  result <- lidar2hemi(1e6, 1e6, 1e6)
  expect_true(is.finite(result$rho))
})

# gla_create_virtual_plots tests ----

test_that("gla_create_virtual_plots validates folder input", {
  # Create test points
  test_points <- sf::st_as_sf(
    data.frame(x = 1000, y = 1000),
    coords = c("x", "y"),
    crs = 3005
  )

  # Non-existent folder should error
  expect_error(
    gla_create_virtual_plots(
      points = test_points,
      folder = "fake_folder_that_does_not_exist",
      plot_radius = 10
    ),
    "folder does not exist"
  )
})

test_that("gla_create_virtual_plots validates points input", {
  # Create temporary directory
  temp_dir <- tempdir()

  # Non-sf object should error
  expect_error(
    gla_create_virtual_plots(
      points = data.frame(x = 1000, y = 1000),
      folder = temp_dir,
      plot_radius = 10
    ),
    "points must be an sf object"
  )

  # NULL should error
  expect_error(
    gla_create_virtual_plots(
      points = NULL,
      folder = temp_dir,
      plot_radius = 10
    ),
    "points must be an sf object"
  )
})

test_that("gla_create_virtual_plots handles empty output gracefully", {
  # Create minimal valid LAS file using fixture generator
  temp_las_dir <- withr::local_tempdir()
  temp_las <- file.path(temp_las_dir, "test.las")
  create_test_las(crs = 3005, n_points = 2, output_path = temp_las)

  # Create test points far outside the LAS extent
  # clip_circle will succeed but create no output files
  test_df <- data.frame(
    stream = "test1",
    x_meters = 999999,
    y_meters = 999999
  )

  test_points <- sf::st_as_sf(
    test_df,
    coords = c("x_meters", "y_meters"),
    crs = 3005
  )
  test_points$x_meters <- test_df$x_meters
  test_points$y_meters <- test_df$y_meters

  temp_output <- withr::local_tempdir()

  # Should handle empty results gracefully
  expect_message(
    result <- gla_create_virtual_plots(
      points = test_points,
      folder = temp_las_dir,
      output_dir = temp_output,
      plot_radius = 5,
      resume = TRUE
    ),
    "No LAS files created"
  )

  # Should still return an sf object with NA values
  expect_s3_class(result, "sf")
  expect_true("las_files" %in% names(result))
  expect_true(all(is.na(result$las_files)))
})

test_that("gla_create_virtual_plots snapshot", {
  # Create minimal test LAS file using fixture generator
  temp_las_dir <- withr::local_tempdir()
  temp_las <- file.path(temp_las_dir, "test.las")
  create_test_las(crs = 3005, n_points = 2, output_path = temp_las)

  test_df <- data.frame(
    stream = "test1",
    x_meters = 1000500,
    y_meters = 500500
  )

  test_points <- sf::st_as_sf(
    test_df,
    coords = c("x_meters", "y_meters"),
    crs = 3005
  )

  # Add x_meters and y_meters back as columns (required by add_las_filename)
  test_points$x_meters <- test_df$x_meters
  test_points$y_meters <- test_df$y_meters

  temp_output <- withr::local_tempdir()

  result <- gla_create_virtual_plots(
    points = test_points,
    folder = temp_las_dir,
    output_dir = temp_output,
    plot_radius = 50
  )

  # Snapshot the structure (use basename for las_files to avoid temp path differences)
  result_snapshot <- sf::st_drop_geometry(result)
  result_snapshot$las_files <- basename(result_snapshot$las_files)

  expect_snapshot(result_snapshot)
})


test_that("Resume logic handles partially created virtual plots", {
  # Create test fixtures on-demand
  dem_path <- withr::local_tempfile(fileext = ".tif")
  create_test_dem(crs = 3005, output_path = dem_path)

  stream_network_path <- withr::local_tempfile(fileext = ".gpkg")
  create_test_points(
    crs = 3005,
    n_points = 2,
    output_path = stream_network_path
  )

  las_dir <- withr::local_tempdir()
  las_path <- file.path(las_dir, "minimal_plot_3005.las")
  create_test_las(crs = 3005, n_points = 100, output_path = las_path)

  output_dir_virtual_plots <- withr::local_tempdir()

  stream_points <- gla_load_points(
    stream_network_path,
    dem_path
  )

  # Create one virtual plot
  stream_points_single <- stream_points[1, ]
  stream_points_single <- gla_create_virtual_plots(
    points = stream_points_single,
    folder = dirname(las_path),
    output_dir = output_dir_virtual_plots,
    plot_radius = 5,
    chunk_size = 0,
    resume = FALSE
  )

  # Now try to create both with resume=TRUE
  stream_points_all <- gla_create_virtual_plots(
    points = stream_points,
    folder = dirname(las_path),
    output_dir = output_dir_virtual_plots,
    plot_radius = 5,
    chunk_size = 0,
    resume = TRUE
  )

  # Should have attempted both points, with one being resumed
  las_files_exist <- !is.na(stream_points_all$las_files) &
    file.exists(stream_points_all$las_files)
  expect_true(any(las_files_exist))
})
