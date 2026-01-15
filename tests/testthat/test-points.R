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
  # Create test fixtures on-demand
  dem_path <- withr::local_tempfile(fileext = ".tif")
  create_test_dem(crs = 3005, output_path = dem_path)

  stream_network_path <- withr::local_tempfile(fileext = ".gpkg")
  create_test_points(
    crs = 3005,
    n_points = 2,
    output_path = stream_network_path
  )

  # Test loading
  result <- gla_load_points(stream_network_path, dem_path)

  # Verify structure
  expect_s3_class(result, "sf")
  expect_true("elevation" %in% names(result))
  expect_true("x_meters" %in% names(result))
  expect_true("y_meters" %in% names(result))
  expect_true("lat" %in% names(result))
  expect_true("lon" %in% names(result))

  # Should load all points
  expect_equal(nrow(result), 2)
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

  temp_shp <- withr::local_tempfile(fileext = ".gpkg")
  sf::write_sf(test_lines, temp_shp)

  dem_path <- withr::local_tempfile(fileext = ".tif")
  create_test_dem(crs = 3005, output_path = dem_path)

  expect_error(
    gla_load_points(temp_shp, dem_path),
    "must contain only POINT geometries"
  )
})

test_that("gla_load_points validates CRS matches between points and DEM", {
  # Create points in EPSG:3005
  stream_network_path <- withr::local_tempfile(fileext = ".gpkg")
  create_test_points(
    crs = 3005,
    n_points = 1,
    output_path = stream_network_path
  )

  # Create DEM in different CRS (EPSG:32610 - UTM Zone 10N)
  dem_path <- withr::local_tempfile(fileext = ".tif")
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
  terra::writeRaster(r, dem_path, overwrite = TRUE)

  # Should error about CRS mismatch
  expect_error(
    gla_load_points(stream_network_path, dem_path),
    "CRS mismatch between Points and DEM"
  )
})


test_that("gla_load_points accepts sf object directly", {
  # Create test fixtures on-demand
  dem_path <- withr::local_tempfile(fileext = ".tif")
  create_test_dem(crs = 3005, output_path = dem_path)

  # Create sf object directly (not saved to file)
  # Use coordinates within DEM extent: [1000000, 1000990] x [500000, 500990]
  test_points <- sf::st_as_sf(
    data.frame(id = 1:2),
    geom = sf::st_sfc(
      sf::st_point(c(1000400, 500400)),
      sf::st_point(c(1000600, 500600)),
      crs = 3005
    )
  )

  # Pass sf object directly
  result <- gla_load_points(test_points, dem_path)

  # Verify structure
  expect_s3_class(result, "sf")
  expect_true("elevation" %in% names(result))
  expect_true("x_meters" %in% names(result))
  expect_true("y_meters" %in% names(result))
  expect_true("lat" %in% names(result))
  expect_true("lon" %in% names(result))
  expect_equal(nrow(result), 2)
})

test_that("gla_load_points accepts SpatRaster object directly", {
  # Create sf object
  test_points <- sf::st_as_sf(
    data.frame(id = 1:2),
    geom = sf::st_sfc(
      sf::st_point(c(1022655, 574704)),
      sf::st_point(c(1022700, 574750)),
      crs = 3005
    )
  )

  # Create SpatRaster object (not saved to file)
  dem_rast <- terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = 1022600,
    xmax = 1022800,
    ymin = 574650,
    ymax = 574800,
    crs = "EPSG:3005",
    vals = rep(250, 100)
  )

  # Pass SpatRaster directly
  result <- gla_load_points(test_points, dem_rast)

  # Verify structure
  expect_s3_class(result, "sf")
  expect_true("elevation" %in% names(result))
  expect_equal(nrow(result), 2)
})

test_that("gla_load_points errors on invalid input type", {
  dem_path <- withr::local_tempfile(fileext = ".tif")
  create_test_dem(crs = 3005, output_path = dem_path)

  # Should error on non-character, non-sf input for x
  expect_error(
    gla_load_points(123, dem_path),
    "x must be either a file path.*or an sf object"
  )

  # Should error on vector of paths for x
  expect_error(
    gla_load_points(c("path1.gpkg", "path2.gpkg"), dem_path),
    "x must be either a file path.*or an sf object"
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

test_that("gla_load_points errors when points are outside DEM extent", {
  # Create DEM with specific extent
  dem_rast <- terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = 1022600,
    xmax = 1022700,  # 100m wide
    ymin = 574650,
    ymax = 574750,   # 100m tall
    crs = "EPSG:3005",
    vals = rep(250, 100)
  )
  dem_path <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(dem_rast, dem_path, overwrite = TRUE)

  # Create points: one inside, one outside DEM extent
  test_points <- sf::st_as_sf(
    data.frame(id = 1:2),
    geom = sf::st_sfc(
      sf::st_point(c(1022650, 574700)),  # Inside DEM
      sf::st_point(c(1023000, 575000)),  # Outside DEM (300m east, 250m north)
      crs = 3005
    )
  )

  # Should error when points are out-of-bounds
  expect_error(
    gla_load_points(test_points, dem_path),
    regexp = "1 point\\(s\\) are outside the DEM extent"
  )
})

test_that("gla_load_points errors when points are on NoData cells inside extent", {
  # Create DEM with NA values in the middle
  # 10x10 grid = 100 cells, set middle cells (45-55) to NA
  dem_vals <- rep(250, 100)
  dem_vals[45:55] <- NA

  dem_rast <- terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = 1000000,
    xmax = 1000100,  # 100m wide, 10m per cell
    ymin = 500000,
    ymax = 500100,   # 100m tall, 10m per cell
    crs = "EPSG:3005",
    vals = dem_vals
  )
  dem_path <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(dem_rast, dem_path, overwrite = TRUE)

  # Create point on NA cell (cell index 50 = row 5, col 5 = center of DEM)
  # Cell center at: X = 1000000 + 5*10 - 5 = 1000045, Y = 500000 + 5*10 - 5 = 500045
  test_points <- sf::st_as_sf(
    data.frame(id = 1),
    geom = sf::st_sfc(
      sf::st_point(c(1000045, 500045)),  # Center of middle cell (should be NA)
      crs = 3005
    )
  )

  # Should error about NoData cells
  expect_error(
    gla_load_points(test_points, dem_path),
    regexp = "NA elevation values.*NoData"
  )
})

test_that("gla_load_points handles all points outside DEM extent", {
  # Create small DEM
  dem_rast <- terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = 1022600,
    xmax = 1022700,
    ymin = 574650,
    ymax = 574750,
    crs = "EPSG:3005",
    vals = rep(250, 100)
  )
  dem_path <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(dem_rast, dem_path, overwrite = TRUE)

  # Create points all outside DEM
  test_points <- sf::st_as_sf(
    data.frame(id = 1:3),
    geom = sf::st_sfc(
      sf::st_point(c(1023000, 575000)),
      sf::st_point(c(1023100, 575100)),
      sf::st_point(c(1023200, 575200)),
      crs = 3005
    )
  )

  # Should error mentioning multiple points
  expect_error(
    gla_load_points(test_points, dem_path),
    regexp = "3 point\\(s\\) are outside the DEM extent"
  )
})
