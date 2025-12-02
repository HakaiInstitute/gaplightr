test_that("gla_load_points handles missing stream network file", {
  dem_path <- test_path("testdata", "minimal_dem.tif")
  stream_network_path <- "nonexistent_stream_network.gpkg"

  expect_error(
    gla_load_points(stream_network_path, dem_path),
    regexp = "Cannot open|doesn't seem to exist"
  )
})

test_that("gla_load_points loads all points without filtering", {
  # Create test fixtures on-demand
  dem_path <- withr::local_tempfile(fileext = ".tif")
  create_test_dem(crs = 3005, output_path = dem_path)

  stream_network_path <- withr::local_tempfile(fileext = ".gpkg")
  create_test_points(
    crs = 3005,
    n_points = 2,
    output_path = stream_network_path
  )

  # Load all points
  result <- gla_load_points(stream_network_path, dem_path)

  # Should load all points from file
  expect_s3_class(result, "sf")
  expect_true(nrow(result) >= 1)
})

## gla_load_points tests ----

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
