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
  dem_path <- test_path("testdata", "minimal_dem_3005.tif")
  stream_network_path <- test_path(
    "testdata",
    "minimal_stream_network_3005.gpkg"
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

  # Should warn about invalid files and then error (no valid files left)
  expect_warning(
    expect_error(
      gla_create_fisheye_photos(
        points = stream_points,
        output_dir = output_dir,
        parallel = FALSE
      ),
      regexp = "No valid LAS files"
    ),
    regexp = "missing or non-existent LAS files"
  )
})

test_that("gla_create_fisheye_photos handles empty LAS file", {
  skip_on_cran()
  skip("Empty LAS files can't be written with lidR::writeLAS")

  # This test is skipped because lidR doesn't allow creating empty LAS files
  # which is actually good behavior - prevents this error case in practice
})

test_that("gla_extract_horizon_terra handles point outside DEM extent", {
  dem_path <- test_path("testdata", "minimal_dem_3005.tif")
  dem_rast <- terra::rast(dem_path)

  # Point way outside DEM extent (far from 1000, 2000)
  expect_error(
    gla_extract_horizon_terra(
      dem_rast = dem_rast,
      x_meters = 999999, # Far outside
      y_meters = 999999, # Far outside
      step = 30
    ),
    regexp = "outside DEM extent|no data"
  )
})

test_that("gla_load_points handles missing stream network file", {
  dem_path <- test_path("testdata", "minimal_dem.tif")
  stream_network_path <- "nonexistent_stream_network.gpkg"

  expect_error(
    gla_load_points(stream_network_path, dem_path),
    regexp = "Cannot open|doesn't seem to exist"
  )
})

test_that("gla_load_points loads all points without filtering", {
  dem_path <- test_path("testdata", "minimal_dem_3005.tif")
  stream_network_path <- test_path(
    "testdata",
    "minimal_stream_network_3005.gpkg"
  )

  # Load all points
  result <- gla_load_points(stream_network_path, dem_path)

  # Should load all points from file
  expect_s3_class(result, "sf")
  expect_true(nrow(result) >= 1)
})

test_that("gla_process_fisheye_photos handles corrupted image file", {
  skip_on_cran()

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

  # Should error when trying to load image
  expect_error(
    gla_process_fisheye_photos(
      points = stream_points,
      parallel = FALSE
    ),
    regexp = "does not appear to be|PNG, BMP, JPEG, or TIFF"
  )
})

test_that("Resume logic handles partially created virtual plots", {
  skip_on_cran()

  dem_path <- test_path("testdata", "minimal_dem_3005.tif")
  stream_network_path <- test_path(
    "testdata",
    "minimal_stream_network_3005.gpkg"
  )
  las_path <- test_path("testdata", "3005", "minimal_plot_3005.las")

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

test_that("gla_create_fisheye_photos validates required columns", {
  output_dir <- withr::local_tempdir()

  # Create points missing required columns
  points_data <- data.frame(
    x_meters = 1000500,
    y_meters = 500500
    # Missing: las_files, horizon_mask
  )

  stream_points <- sf::st_as_sf(
    points_data,
    coords = c("x_meters", "y_meters"),
    crs = 3005
  )

  expect_error(
    gla_create_fisheye_photos(
      points = stream_points,
      output_dir = output_dir
    ),
    regexp = "must contain columns"
  )
})

test_that("gla_extract_horizons validates sf object", {
  dem_path <- test_path("testdata", "minimal_dem_3005.tif")

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
  dem_path <- test_path("testdata", "minimal_dem_3005.tif")

  # Create points WITHOUT lat/lon columns - should work now
  points_data <- data.frame(
    x_meters = 1000500,
    y_meters = 500500
  )

  stream_points <- sf::st_as_sf(
    points_data,
    coords = c("x_meters", "y_meters"),
    crs = 3005
  )

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
  already_processed <- sf::st_as_sf(
    data.frame(
      stream = "CP38",
      x_meters = 1022655,
      y_meters = 574704,
      lat = 50.1876,
      lon = -125.6827,
      elevation = 238.44,
      fisheye_photo_path = test_photo,
      canopy_openness_pct = 45.5,  # Already processed
      transmitted_global_irradiation_MJm2d = 12.3,
      light_penetration_index = 0.56
    ),
    coords = c("x_meters", "y_meters"),
    crs = 3005
  )

  expect_error(
    gla_process_fisheye_photos(points = already_processed, parallel = FALSE),
    regexp = "already processed"
  )
})
