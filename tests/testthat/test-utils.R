# gla_load_points tests ----

test_that("gla_load_points loads and processes points correctly", {
  # Create minimal test shapefile
  test_points <- sf::st_as_sf(
    data.frame(
      x = c(1022655, 1022700),
      y = c(574704, 574750)
    ),
    coords = c("x", "y"),
    crs = 3005
  )

  # Save to temporary file
  temp_shp <- tempfile(fileext = ".gpkg")
  sf::write_sf(test_points, temp_shp)

  # Create minimal test DEM
  temp_dem <- tempfile(fileext = ".tif")
  r <- terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = 1022600,
    xmax = 1022800,
    ymin = 574650,
    ymax = 574800,
    crs = "EPSG:3005",
    vals = rep(250, 100) # Constant elevation
  )
  terra::writeRaster(r, temp_dem, overwrite = TRUE)

  # Test loading
  result <- gla_load_points(temp_shp, temp_dem)

  # Verify structure
  expect_s3_class(result, "sf")
  expect_true("elevation" %in% names(result))
  expect_true("x_meters" %in% names(result))
  expect_true("y_meters" %in% names(result))
  expect_true("lat" %in% names(result))
  expect_true("lon" %in% names(result))

  # Should load all points
  expect_equal(nrow(result), 2)

  # Clean up
  unlink(temp_shp)
  unlink(temp_dem)
})

test_that("gla_load_points validates input geometry type", {
  # Create non-POINT geometry
  test_lines <- sf::st_as_sf(
    data.frame(id = 1),
    geom = sf::st_sfc(sf::st_linestring(matrix(
      c(0, 0, 1, 1),
      ncol = 2,
      byrow = TRUE
    ))),
    crs = 3005
  )

  temp_shp <- tempfile(fileext = ".gpkg")
  sf::write_sf(test_lines, temp_shp)

  temp_dem <- tempfile(fileext = ".tif")
  r <- terra::rast(nrows = 2, ncols = 2, vals = 1)
  terra::writeRaster(r, temp_dem, overwrite = TRUE)

  expect_error(
    gla_load_points(temp_shp, temp_dem),
    "must contain only POINT geometries"
  )

  unlink(temp_shp)
  unlink(temp_dem)
})

test_that("gla_load_points validates CRS matches between points and DEM", {
  # Create points in EPSG:3005
  test_points <- sf::st_as_sf(
    data.frame(
      x = c(1022655),
      y = c(574704)
    ),
    coords = c("x", "y"),
    crs = 3005
  )

  temp_shp <- tempfile(fileext = ".gpkg")
  sf::write_sf(test_points, temp_shp)

  # Create DEM in different CRS (EPSG:32610 - UTM Zone 10N)
  temp_dem <- tempfile(fileext = ".tif")
  r <- terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = 400000,
    xmax = 500000,
    ymin = 5500000,
    ymax = 5600000,
    crs = "EPSG:32610",
    vals = rep(250, 100)
  )
  terra::writeRaster(r, temp_dem, overwrite = TRUE)

  # Should error about CRS mismatch
  expect_error(
    gla_load_points(temp_shp, temp_dem),
    "CRS mismatch between Points and DEM"
  )

  unlink(temp_shp)
  unlink(temp_dem)
})

# validate_crs_match tests ----

test_that("validate_crs_match passes when CRS match", {
  crs1 <- sf::st_crs(3005)
  crs2 <- sf::st_crs(3005)

  # Should not error
  expect_silent(validate_crs_match(crs1, crs2, "Object1", "Object2"))
})

test_that("validate_crs_match errors when CRS mismatch", {
  crs1 <- sf::st_crs(3005)  # BC Albers
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

test_that("gla_load_points snapshot", {
  # Create minimal test data
  test_points <- sf::st_as_sf(
    data.frame(
      x = c(1022655),
      y = c(574704)
    ),
    coords = c("x", "y"),
    crs = 3005
  )

  temp_shp <- tempfile(fileext = ".gpkg")
  sf::write_sf(test_points, temp_shp)

  temp_dem <- tempfile(fileext = ".tif")
  r <- terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = 1022600,
    xmax = 1022800,
    ymin = 574650,
    ymax = 574800,
    crs = "EPSG:3005",
    vals = rep(250, 100)
  )
  terra::writeRaster(r, temp_dem, overwrite = TRUE)

  result <- gla_load_points(temp_shp, temp_dem)

  # Snapshot the structure (drop geometry for stability)
  expect_snapshot(as.data.frame(sf::st_drop_geometry(result)))

  unlink(temp_shp)
  unlink(temp_dem)
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
