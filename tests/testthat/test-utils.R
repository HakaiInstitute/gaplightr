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


# point_id collision test ----

test_that("gla_create_virtual_plots assigns distinct files to points whose coordinates round to the same value", {
  # Under the old coordinate-based filename scheme, two points within 0.5m in X
  # or 0.05m in Y would collide on the same filename. With point_id as the key
  # each point always gets its own uniquely-named LAS file.
  dem_path <- withr::local_tempfile(fileext = ".tif")
  create_test_dem(crs = 3005, output_path = dem_path)

  las_dir <- withr::local_tempdir()
  create_test_las(
    crs = 3005,
    n_points = 500,
    output_path = file.path(las_dir, "test.las")
  )

  # Two points whose coordinates round identically: both become 1000500_500500
  # under round(x, 0) / round(y, 1), but have distinct point_ids.
  # Placed at the center of the test LAS distribution (1000500, 500500) so
  # both points fall within the clipped coverage.
  pts <- sf::st_as_sf(
    data.frame(x = c(1000500.1, 1000500.4), y = c(500500.03, 500500.06)),
    coords = c("x", "y"),
    crs = 3005
  )
  pts <- gla_load_points(pts, dem_path)

  result <- gla_create_virtual_plots(
    points = pts,
    folder = las_dir,
    output_dir = withr::local_tempdir(),
    plot_radius = 50,
    resume = FALSE
  )

  # Both points must have valid, distinct LAS files - the old coordinate-based
  # scheme would have collapsed them to a single filename.
  expect_equal(sum(!is.na(result$las_files)), 2)
  expect_equal(length(unique(result$las_files)), 2)
})


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


# angular_bin_idx tests ----

test_that("angular_bin_idx places zero angle in bin 1", {
  expect_equal(angular_bin_idx(0, pi / 2, 9), 1L)
  expect_equal(angular_bin_idx(0, 2 * pi, 8), 1L)
})

test_that("angular_bin_idx caps max angle at n_bins", {
  # Angle exactly at max_rad should be in last bin, not overflow

  expect_equal(angular_bin_idx(pi / 2, pi / 2, 9), 9L)
  expect_equal(angular_bin_idx(2 * pi, 2 * pi, 8), 8L)
})

test_that("angular_bin_idx distributes angles evenly across bins", {
  n_bins <- 9
  max_rad <- pi / 2

  # First bin: [0, pi/18)

  expect_equal(angular_bin_idx(0, max_rad, n_bins), 1L)
  expect_equal(angular_bin_idx(pi / 36, max_rad, n_bins), 1L)

  # Second bin: [pi/18, 2*pi/18)
  expect_equal(angular_bin_idx(pi / 18, max_rad, n_bins), 2L)

  # Last bin: [8*pi/18, pi/2]
  expect_equal(angular_bin_idx(8 * pi / 18, max_rad, n_bins), 9L)
})

test_that("angular_bin_idx handles vector input", {
  angles <- c(0, pi / 4, pi / 2)
  result <- angular_bin_idx(angles, pi / 2, 9)

  expect_equal(length(result), 3)

  expect_equal(result[1], 1L)
  expect_equal(result[2], 5L) # pi/4 is halfway, bin 5 of 9

  expect_equal(result[3], 9L)
})

test_that("angular_bin_idx works for azimuth use case", {
  # 8 azimuth sectors covering 0 to 2*pi
  n_sectors <- 8
  max_rad <- 2 * pi

  # North (0 rad) -> bin 1

  expect_equal(angular_bin_idx(0, max_rad, n_sectors), 1L)

  # East (pi/2 rad) -> bin 3
  expect_equal(angular_bin_idx(pi / 2, max_rad, n_sectors), 3L)

  # South (pi rad) -> bin 5
  expect_equal(angular_bin_idx(pi, max_rad, n_sectors), 5L)

  # West (3*pi/2 rad) -> bin 7
  expect_equal(angular_bin_idx(3 * pi / 2, max_rad, n_sectors), 7L)
})


# convert_to_geographic_azimuth tests ----

test_that("convert_to_geographic_azimuth handles zero (north)", {
  # Zero input should return zero

  expect_equal(convert_to_geographic_azimuth(0), 0)
})

test_that("convert_to_geographic_azimuth handles negative angles (west)", {
  # Negative angles are negated to positive
  expect_equal(convert_to_geographic_azimuth(-pi / 2), pi / 2)
  expect_equal(convert_to_geographic_azimuth(-pi), pi)
  expect_equal(convert_to_geographic_azimuth(-pi / 4), pi / 4)
})

test_that("convert_to_geographic_azimuth handles positive angles (east flip)", {
  # Positive angles are flipped: 2*pi - angle
  expect_equal(convert_to_geographic_azimuth(pi / 2), 2 * pi - pi / 2)
  expect_equal(convert_to_geographic_azimuth(pi), 2 * pi - pi)
  expect_equal(convert_to_geographic_azimuth(pi / 4), 2 * pi - pi / 4)
})

test_that("convert_to_geographic_azimuth handles vector input", {
  input <- c(-pi / 2, 0, pi / 2)
  expected <- c(pi / 2, 0, 2 * pi - pi / 2)

  result <- convert_to_geographic_azimuth(input)

  expect_equal(result, expected)
})
