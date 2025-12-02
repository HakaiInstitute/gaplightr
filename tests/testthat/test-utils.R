#

# validate_crs_match tests ----

test_that("validate_crs_match passes when CRS match", {
  crs1 <- sf::st_crs(3005)
  crs2 <- sf::st_crs(3005)

  # Should not error
  expect_silent(validate_crs_match(crs1, crs2, "Object1", "Object2"))
})

test_that("validate_crs_match errors when CRS mismatch", {
  crs1 <- sf::st_crs(3005) # BC Albers
  crs2 <- sf::st_crs(32610) # UTM Zone 10N

  expect_error(
    validate_crs_match(crs1, crs2, "Points", "DEM"),
    "CRS mismatch between Points and DEM"
  )
})

test_that("validate_crs_match errors when first CRS is NA", {
  crs1 <- sf::st_crs(NA)
  crs2 <- sf::st_crs(3005)

  expect_error(
    validate_crs_match(crs1, crs2, "Points", "DEM"),
    "Points has no CRS defined"
  )
})

test_that("validate_crs_match errors when second CRS is NA", {
  crs1 <- sf::st_crs(3005)
  crs2 <- sf::st_crs(NA)

  expect_error(
    validate_crs_match(crs1, crs2, "Points", "LiDAR catalog"),
    "LiDAR catalog has no CRS defined"
  )
})

test_that("validate_crs_match errors when both CRS are NA", {
  crs1 <- sf::st_crs(NA)
  crs2 <- sf::st_crs(NA)

  # Should error on first NA check
  expect_error(
    validate_crs_match(crs1, crs2, "Points", "DEM"),
    "Points has no CRS defined"
  )
})


# add_las_filename tests ----

test_that("add_las_filename parses and merges correctly", {
  # Create test stream points with x_meters and y_meters as both columns AND geometry
  test_df <- data.frame(
    x_meters = c(1000, 1001),
    y_meters = c(2000.5, 2001.5)
  )

  stream_points <- sf::st_as_sf(
    test_df,
    coords = c("x_meters", "y_meters"),
    crs = 3005
  )

  # Add x_meters and y_meters back as regular columns
  stream_points$x_meters <- test_df$x_meters
  stream_points$y_meters <- test_df$y_meters

  # Create test LAS file names
  las_files <- c(
    "/path/to/1000_2000.5.las",
    "/path/to/1001_2001.5.las"
  )

  result <- add_las_filename(stream_points, las_files)

  expect_s3_class(result, "sf")
  expect_true("las_files" %in% names(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$las_files, las_files)
})

test_that("add_las_filename snapshot", {
  test_df <- data.frame(
    x_meters = c(1000),
    y_meters = c(2000.5)
  )

  stream_points <- sf::st_as_sf(
    test_df,
    coords = c("x_meters", "y_meters"),
    crs = 3005
  )

  # Add x_meters and y_meters back as regular columns
  stream_points$x_meters <- test_df$x_meters
  stream_points$y_meters <- test_df$y_meters

  las_files <- c("/path/to/1000_2000.5.las")

  result <- add_las_filename(stream_points, las_files)

  # Snapshot structure (use basename for las_files to avoid temp path differences)
  result_snapshot <- sf::st_drop_geometry(result)
  result_snapshot$las_files <- basename(result_snapshot$las_files)

  expect_snapshot(result_snapshot)
})

check_if_coordinates_are_unique <- function(df) {
  if (any(duplicated(df[, c("x_meters", "y_meters")]))) {
    stop(
      "Warning: Some points have identical x_meters and y_meters coordinates.",
      call. = FALSE
    )
  } else {
    message("All points have unique x_meters and y_meters coordinates.")
  }
}
