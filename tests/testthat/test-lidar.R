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

test_that("gla_create_virtual_plots creates output files and adds columns", {
  # Create minimal test LAS file
  temp_las_dir <- tempfile(pattern = "las_dir")
  dir.create(temp_las_dir)

  # Create a simple LAS file with minimal points
  las_data <- data.frame(
    X = c(1000, 1001, 1002),
    Y = c(2000, 2001, 2002),
    Z = c(100, 101, 102),
    Classification = as.integer(c(1, 2, 1))
  )

  las <- lidR::LAS(las_data)
  lidR::projection(las) <- 3005  # Set CRS to match test points
  temp_las <- file.path(temp_las_dir, "test.las")
  lidR::writeLAS(las, temp_las)

  # Create test points with x_meters and y_meters columns
  test_df <- data.frame(
    stream = "test1",
    x_meters = 1001,
    y_meters = 2001
  )

  test_points <- sf::st_as_sf(
    test_df,
    coords = c("x_meters", "y_meters"),
    crs = 3005
  )

  # Add x_meters and y_meters back as columns (required by add_las_filename)
  test_points$x_meters <- test_df$x_meters
  test_points$y_meters <- test_df$y_meters

  # Create output directory
  temp_output <- tempfile(pattern = "output_dir")
  dir.create(temp_output)

  # Run function
  result <- gla_create_virtual_plots(
    points = test_points,
    folder = temp_las_dir,
    output_dir = temp_output,
    plot_radius = 5
  )

  # Verify structure
  expect_s3_class(result, "sf")
  expect_true("las_files" %in% names(result))

  # Verify output files were created
  output_files <- list.files(
    temp_output,
    pattern = "\\.las$",
    full.names = TRUE
  )
  expect_gt(length(output_files), 0)

  # Verify las_files column contains valid paths
  expect_true(all(file.exists(result$las_files)))

  # Clean up
  unlink(temp_las_dir, recursive = TRUE)
  unlink(temp_output, recursive = TRUE)
})

test_that("gla_create_virtual_plots resume skips existing files", {
  # Create minimal test LAS file
  temp_las_dir <- tempfile(pattern = "las_dir")
  dir.create(temp_las_dir)

  las_data <- data.frame(
    X = c(1000, 1001, 1002, 1003),
    Y = c(2000, 2001, 2002, 2003),
    Z = c(100, 101, 102, 103),
    Classification = as.integer(c(1, 2, 1, 2))
  )

  las <- lidR::LAS(las_data)
  lidR::projection(las) <- 3005  # Set CRS to match test points
  temp_las <- file.path(temp_las_dir, "test.las")
  lidR::writeLAS(las, temp_las)

  # Create test points - two points
  test_df <- data.frame(
    stream = c("test1", "test2"),
    x_meters = c(1001, 1002),
    y_meters = c(2001, 2002)
  )

  test_points <- sf::st_as_sf(
    test_df,
    coords = c("x_meters", "y_meters"),
    crs = 3005
  )

  test_points$x_meters <- test_df$x_meters
  test_points$y_meters <- test_df$y_meters

  temp_output <- tempfile(pattern = "output_dir")
  dir.create(temp_output)

  # First run - create both plots
  result1 <- gla_create_virtual_plots(
    points = test_points,
    folder = temp_las_dir,
    output_dir = temp_output,
    plot_radius = 5,
    resume = TRUE
  )

  # Verify both files created
  output_files_1 <- list.files(temp_output, pattern = "\\.las$")
  n_files_first_run <- length(output_files_1)
  expect_gt(n_files_first_run, 0)

  # Second run - should skip existing files
  expect_message(
    result2 <- gla_create_virtual_plots(
      points = test_points,
      folder = temp_las_dir,
      output_dir = temp_output,
      plot_radius = 5,
      resume = TRUE
    ),
    "already exist"
  )

  # Verify no new files created
  output_files_2 <- list.files(temp_output, pattern = "\\.las$")
  expect_equal(length(output_files_2), n_files_first_run)

  # Verify result still has correct structure
  expect_s3_class(result2, "sf")
  expect_true("las_files" %in% names(result2))
  expect_equal(nrow(result2), nrow(test_points))

  # Clean up
  unlink(temp_las_dir, recursive = TRUE)
  unlink(temp_output, recursive = TRUE)
})

test_that("gla_create_virtual_plots resume processes only new points", {
  # Create minimal test LAS file
  temp_las_dir <- tempfile(pattern = "las_dir")
  dir.create(temp_las_dir)

  las_data <- data.frame(
    X = c(1000, 1001, 1002, 1003),
    Y = c(2000, 2001, 2002, 2003),
    Z = c(100, 101, 102, 103),
    Classification = as.integer(c(1, 2, 1, 2))
  )

  las <- lidR::LAS(las_data)
  lidR::projection(las) <- 3005  # Set CRS to match test points
  temp_las <- file.path(temp_las_dir, "test.las")
  lidR::writeLAS(las, temp_las)

  # Create test points - start with one point
  test_df_1 <- data.frame(
    stream = "test1",
    x_meters = 1001,
    y_meters = 2001
  )

  test_points_1 <- sf::st_as_sf(
    test_df_1,
    coords = c("x_meters", "y_meters"),
    crs = 3005
  )
  test_points_1$x_meters <- test_df_1$x_meters
  test_points_1$y_meters <- test_df_1$y_meters

  temp_output <- tempfile(pattern = "output_dir")
  dir.create(temp_output)

  # First run - create one plot
  result1 <- gla_create_virtual_plots(
    points = test_points_1,
    folder = temp_las_dir,
    output_dir = temp_output,
    plot_radius = 5,
    resume = TRUE
  )

  n_files_first <- length(list.files(temp_output, pattern = "\\.las$"))

  # Add a second point
  test_df_2 <- data.frame(
    stream = c("test1", "test2"),
    x_meters = c(1001, 1002),
    y_meters = c(2001, 2002)
  )

  test_points_2 <- sf::st_as_sf(
    test_df_2,
    coords = c("x_meters", "y_meters"),
    crs = 3005
  )
  test_points_2$x_meters <- test_df_2$x_meters
  test_points_2$y_meters <- test_df_2$y_meters

  # Second run with two points - should only process the new one
  expect_message(
    result2 <- gla_create_virtual_plots(
      points = test_points_2,
      folder = temp_las_dir,
      output_dir = temp_output,
      plot_radius = 5,
      resume = TRUE
    ),
    "1 already exist"
  )

  # Verify one new file created
  n_files_second <- length(list.files(temp_output, pattern = "\\.las$"))
  expect_gt(n_files_second, n_files_first)

  # Verify result has both points
  expect_equal(nrow(result2), 2)
  expect_true(all(!is.na(result2$las_files)))

  # Clean up
  unlink(temp_las_dir, recursive = TRUE)
  unlink(temp_output, recursive = TRUE)
})

test_that("gla_create_virtual_plots handles empty output gracefully", {
  # Create minimal valid LAS file
  temp_las_dir <- tempfile(pattern = "las_dir")
  dir.create(temp_las_dir)

  las_data <- data.frame(
    X = c(1000, 1001),
    Y = c(2000, 2001),
    Z = c(100, 101),
    Classification = as.integer(c(1, 2))
  )

  las <- lidR::LAS(las_data)
  lidR::projection(las) <- 3005  # Set CRS to match test points
  temp_las <- file.path(temp_las_dir, "test.las")
  lidR::writeLAS(las, temp_las)

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

  temp_output <- tempfile(pattern = "output_dir")
  dir.create(temp_output)

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

  # Clean up
  unlink(temp_las_dir, recursive = TRUE)
  unlink(temp_output, recursive = TRUE)
})

test_that("gla_create_virtual_plots snapshot", {
  # Create minimal test LAS file
  temp_las_dir <- tempfile(pattern = "las_dir")
  dir.create(temp_las_dir)

  las_data <- data.frame(
    X = c(1000, 1001),
    Y = c(2000, 2001),
    Z = c(100, 101),
    Classification = as.integer(c(1, 2))
  )

  las <- lidR::LAS(las_data)
  lidR::projection(las) <- 3005  # Set CRS to match test points
  temp_las <- file.path(temp_las_dir, "test.las")
  lidR::writeLAS(las, temp_las)

  test_df <- data.frame(
    stream = "test1",
    x_meters = 1000,
    y_meters = 2000.5
  )

  test_points <- sf::st_as_sf(
    test_df,
    coords = c("x_meters", "y_meters"),
    crs = 3005
  )

  # Add x_meters and y_meters back as columns (required by add_las_filename)
  test_points$x_meters <- test_df$x_meters
  test_points$y_meters <- test_df$y_meters

  temp_output <- tempfile(pattern = "output_dir")
  dir.create(temp_output)

  result <- gla_create_virtual_plots(
    points = test_points,
    folder = temp_las_dir,
    output_dir = temp_output,
    plot_radius = 5
  )

  # Snapshot the structure (use basename for las_files to avoid temp path differences)
  result_snapshot <- sf::st_drop_geometry(result)
  result_snapshot$las_files <- basename(result_snapshot$las_files)

  expect_snapshot(result_snapshot)

  # Clean up
  unlink(temp_las_dir, recursive = TRUE)
  unlink(temp_output, recursive = TRUE)
})
