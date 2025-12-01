test_that("gla_extract_horizon_terra matches GRASS reference data", {
  skip_on_ci() # DEM not available in CI
  skip_on_cran() # DEM not available on CRAN

  # Test coordinates for CP38 (in BC Albers EPSG:3005)
  # These correspond to lat/lon: 50.1876, -125.6827
  test_x_meters <- 1022655.4478
  test_y_meters <- 574703.9408
  step <- 5

  # Use dat from setup.R (CP38_horizon_ccw_rgrass.csv already loaded)
  # dat has columns: azimuth, horizon_height, x_msk, y_msk

  # DEM path (relative to project root)
  dem_path <- here::here("data/TRIB08/dem_2m/Salmon_River_DEM_Flattened_2m.tif")
  skip_if(!file.exists(dem_path), "DEM file not found")

  # Load DEM with terra
  dem_rast <- terra::rast(dem_path)

  # Extract horizon with terra
  horizon_terra <- gla_extract_horizon_terra(
    dem_rast = dem_rast,
    x_meters = test_x_meters,
    y_meters = test_y_meters,
    step = step,
    verbose = FALSE
  )

  # Check structure matches GRASS output (dat from setup.R)
  expect_s3_class(horizon_terra, "data.frame")
  expect_equal(nrow(horizon_terra), nrow(dat))
  expect_equal(colnames(horizon_terra), c("azimuth", "horizon_height"))

  # Azimuths should match exactly
  expect_equal(horizon_terra$azimuth, dat$azimuth)

  # Compare horizon angles
  horizon_diff <- horizon_terra$horizon_height - dat$horizon_height

  # Calculate error metrics
  rmse <- sqrt(mean(horizon_diff^2))
  mae <- mean(abs(horizon_diff))
  max_abs_error <- max(abs(horizon_diff))

  # Expect RMSE < 1 degree
  # Note: Terra implementation achieves ~0.2° RMSE for 40% of test points
  # and 1-2° RMSE for remaining 60% due to near-field terrain sensitivity
  # This specific test point (CP38) should be one of the good ones
  expect_lt(rmse, 1.0)

  # Expect MAE < 0.5 degrees
  expect_lt(mae, 0.5)

  # Expect max absolute error < 2 degrees
  expect_lt(max_abs_error, 2.0)

  # Snapshot comparison for debugging - show worst 5 matches
  worst_idx <- order(abs(horizon_diff), decreasing = TRUE)[1:5]
  expect_snapshot({
    list(
      rmse = round(rmse, 4),
      mae = round(mae, 4),
      max_abs_error = round(max_abs_error, 4),
      worst_matches = data.frame(
        azimuth = horizon_terra$azimuth[worst_idx],
        terra = round(horizon_terra$horizon_height[worst_idx], 4),
        grass = round(dat$horizon_height[worst_idx], 4),
        diff = round(horizon_diff[worst_idx], 4)
      )
    )
  })
})

test_that("gla_extract_horizon_terra validates inputs", {
  # Test error handling for missing/invalid inputs
  dem_rast <- terra::rast(matrix(1:100, 10, 10))

  expect_error(
    gla_extract_horizon_terra(
      dem_rast = "not_a_raster",
      x_meters = 1000,
      y_meters = 2000
    ),
    "dem_rast must be a SpatRaster object"
  )

  expect_error(
    gla_extract_horizon_terra(
      dem_rast = dem_rast,
      y_meters = 2000
    ),
    "Both x_meters and y_meters must be provided"
  )

  expect_error(
    gla_extract_horizon_terra(
      dem_rast = dem_rast,
      x_meters = "invalid",
      y_meters = 2000
    ),
    "x_meters and y_meters must be numeric"
  )
})

test_that("gla_extract_horizon_terra accepts custom parameters", {
  skip_on_ci()
  skip_on_cran()

  # DEM path (relative to project root)
  dem_path <- here::here("data/TRIB08/dem_2m/Salmon_River_DEM_Flattened_2m.tif")
  skip_if(!file.exists(dem_path), "DEM file not found")

  dem_rast <- terra::rast(dem_path)

  # Test with custom parameters (CP38 coordinates in BC Albers)
  horizon_custom <- gla_extract_horizon_terra(
    dem_rast = dem_rast,
    x_meters = 1022655,
    y_meters = 574704,
    step = 10, # Larger step
    max_search_distance = 100, # Limited radius
    distance_step = 5, # Larger sampling step
    verbose = FALSE
  )

  # Should have fewer rows with step=10
  expect_equal(nrow(horizon_custom), 360 / 10)
  expect_equal(min(horizon_custom$azimuth), 0)
  expect_equal(max(horizon_custom$azimuth), 360 - 10)
})

test_that("terra horizon extraction works with parallel processing in gla_create_fisheye_photos", {
  skip_on_ci()
  skip_on_cran()

  # Setup paths
  dem_path <- here::here("data/TRIB08/dem_2m/Salmon_River_DEM_Flattened_2m.tif")
  skip_if(!file.exists(dem_path), "DEM file not found")

  las_dir <- here::here("data/TRIB08/virtual_plots")
  skip_if(!dir.exists(las_dir), "Virtual plots directory not found")

  # Create test points (use 2 points to test parallel processing)
  test_points <- sf::st_as_sf(
    data.frame(
      lat = c(50.1876, 50.1880),
      lon = c(-125.6827, -125.6830),
      elevation = c(238.44, 240.0),
      x_meters = c(1157500, 1157480),
      y_meters = c(621200, 621220)
    ),
    coords = c("x_meters", "y_meters"),
    crs = 3005
  )

  # Find corresponding LAS files
  las_files <- list.files(las_dir, pattern = "\\.las$", full.names = TRUE)
  skip_if(length(las_files) < 2, "Not enough LAS files for test")

  test_points$las_files <- las_files[1:2]

  # Create temporary output directory
  tmp_dir <- withr::local_tempdir()

  # Setup parallel processing
  future::plan(future::multisession, workers = 2)
  on.exit(future::plan(future::sequential), add = TRUE)

  # Test with terra method and parallel = TRUE
  result_parallel <- gla_create_fisheye_photos(
    points = test_points,
    dem_path = dem_path,
    output_dir = tmp_dir,
    cam_ht = 1.37,
    min_dist = 1,
    max_dist = 220,
    img_res = 2800,
    max_cex = 0.2,
    min_cex = 0.05,
    pointsize = 10,
    dpi = 300,
    parallel = TRUE,
    resume = FALSE,
    horizon_estimation_method = "terra"
  )

  # Test with terra method and parallel = FALSE (sequential)
  tmp_dir_seq <- withr::local_tempdir()

  result_sequential <- gla_create_fisheye_photos(
    points = test_points,
    dem_path = dem_path,
    output_dir = tmp_dir_seq,
    cam_ht = 1.37,
    min_dist = 1,
    max_dist = 220,
    img_res = 2800,
    max_cex = 0.2,
    min_cex = 0.05,
    pointsize = 10,
    dpi = 300,
    parallel = FALSE,
    resume = FALSE,
    horizon_estimation_method = "terra"
  )

  # Both should succeed and return sf objects
  expect_s3_class(result_parallel, "sf")
  expect_s3_class(result_sequential, "sf")

  # Both should have fisheye_photo_path column
  expect_true("fisheye_photo_path" %in% names(result_parallel))
  expect_true("fisheye_photo_path" %in% names(result_sequential))

  # Both should create the same number of photos
  expect_equal(nrow(result_parallel), nrow(result_sequential))

  # Photos should exist
  expect_true(all(file.exists(result_parallel$fisheye_photo_path)))
  expect_true(all(file.exists(result_sequential$fisheye_photo_path)))
})
