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


# sshourangle tests ----

test_that("sshourangle handles polar night (cos_ws > 1)", {
  # Arctic winter: 80°N latitude, December 21
  lat_rad <- 80 * deg_to_rad()
  dec_rad <- -23.44 * deg_to_rad() # Winter solstice declination

  result <- sshourangle(lat_rad = lat_rad, solar_declination_rad = dec_rad)

  # Should return: cos_ws, sunrise, sunset
  expect_equal(length(result), 3)

  cos_ws <- result[1]
  sunrise <- result[2]
  sunset <- result[3]

  # cos_ws should be > 1 (polar night condition)
  expect_gt(cos_ws, 1)

  # sunrise and sunset should be NA
  expect_true(is.na(sunrise))
  expect_true(is.na(sunset))
})

test_that("sshourangle handles polar day (cos_ws < -1)", {
  # Arctic summer: 80°N latitude, June 21
  lat_rad <- 80 * deg_to_rad()
  dec_rad <- 23.44 * deg_to_rad() # Summer solstice declination

  result <- sshourangle(lat_rad = lat_rad, solar_declination_rad = dec_rad)

  # Should return: cos_ws, sunrise, sunset
  expect_equal(length(result), 3)

  cos_ws <- result[1]
  sunrise <- result[2]
  sunset <- result[3]

  # cos_ws should be < -1 (polar day condition)
  expect_lt(cos_ws, -1)

  # sunrise should be pi, sunset should be -pi
  expect_equal(sunrise, pi)
  expect_equal(sunset, -pi)
})

test_that("sshourangle handles normal case (-1 <= cos_ws <= 1)", {
  # Mid-latitude: 50°N latitude, equinox
  lat_rad <- 50 * deg_to_rad()
  dec_rad <- 0 # Equinox declination

  result <- sshourangle(lat_rad = lat_rad, solar_declination_rad = dec_rad)

  # Should return: cos_ws, sunrise, sunset
  expect_equal(length(result), 3)

  cos_ws <- result[1]
  sunrise <- result[2]
  sunset <- result[3]

  # cos_ws should be in valid range
  expect_gte(cos_ws, -1)
  expect_lte(cos_ws, 1)

  # sunrise and sunset should be finite, non-NA values
  expect_true(is.finite(sunrise))
  expect_true(is.finite(sunset))

  # sunset should be negative of sunrise
  expect_equal(sunset, -sunrise)

  # At equinox, expect roughly 12 hours of daylight (sunrise ~= pi/2)
  expect_gt(sunrise, 0)
  expect_lt(sunrise, pi)
})


# solpos tests ----

test_that("solpos handles sun directly overhead (SZA = 0)", {
  # Sun directly overhead: equator at solar noon on equinox
  # Solar declination = 0 (equinox), latitude = 0 (equator), hour angle = 0 (noon)
  dec_rad <- 0
  lat_rad <- 0
  ha_rad <- 0

  result <- solpos(
    solar_declination_rad = dec_rad,
    lat_rad = lat_rad,
    hour_angle_rad = ha_rad
  )

  # Should return: sz, se, sa, sa_rot, sa_rot_ccw, x_sun, y_sun
  expect_equal(length(result), 7)

  sz <- result[1] # Solar zenith
  se <- result[2] # Solar elevation
  sa <- result[3] # Solar azimuth
  sa_rot <- result[4] # Solar azimuth rotated
  sa_rot_ccw <- result[5] # Solar azimuth CCW
  x_sun <- result[6] # X coordinate
  y_sun <- result[7] # Y coordinate

  # When sun is overhead, zenith angle should be 0
  expect_equal(sz, 0, tolerance = 1e-10)

  # Solar elevation should be pi/2 (90 degrees)
  expect_equal(se, rad_90(), tolerance = 1e-10)

  # Solar azimuth is undefined when overhead, should be NA
  expect_true(is.na(sa))
  expect_true(is.na(sa_rot))
  expect_true(is.na(sa_rot_ccw))

  # Cartesian coordinates should be (0, 0) when overhead
  expect_equal(x_sun, 0, tolerance = 1e-10)
  expect_equal(y_sun, 0, tolerance = 1e-10)
})
