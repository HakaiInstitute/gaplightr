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

  # DEM path
  dem_path <- test_path("testdata", "minimal_dem_3005.tif")
  skip_if(!file.exists(dem_path), "DEM file not found")

  dem_rast <- terra::rast(dem_path)

  # Test with custom parameters (R2D2 coordinates in BC Albers)
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

# test_that("terra horizon extraction works with parallel processing in gla_create_fisheye_photos", {
#   skip_on_ci()
#   skip_on_cran()

#   # Setup paths
#   dem_path <- test_path("testdata", "minimal_dem_3005.tif")
#   skip_if(!file.exists(dem_path), "DEM file not found")

#   las_dir <- here::here("data/TRIB08/virtual_plots")
#   skip_if(!dir.exists(las_dir), "Virtual plots directory not found")

#   # Create test points (use 2 points to test parallel processing)
#   test_points <- sf::st_as_sf(
#     data.frame(
#       lat = c(50.1876, 50.1880),
#       lon = c(-125.6827, -125.6830),
#       elevation = c(238.44, 240.0),
#       x_meters = c(1157500, 1157480),
#       y_meters = c(621200, 621220)
#     ),
#     coords = c("x_meters", "y_meters"),
#     crs = 3005
#   )

#   # Find corresponding LAS files
#   las_files <- list.files(las_dir, pattern = "\\.las$", full.names = TRUE)
#   skip_if(length(las_files) < 2, "Not enough LAS files for test")

#   test_points$las_files <- las_files[1:2]

#   # Create temporary output directory
#   tmp_dir <- withr::local_tempdir()

#   # Setup parallel processing
#   future::plan(future::multisession, workers = 2)
#   on.exit(future::plan(future::sequential), add = TRUE)

#   # Test with terra method and parallel = TRUE
#   result_parallel <- gla_create_fisheye_photos(
#     points = test_points,
#     dem_path = dem_path,
#     output_dir = tmp_dir,
#     cam_ht = 1.37,
#     min_dist = 1,
#     max_dist = 220,
#     img_res = 2800,
#     max_cex = 0.2,
#     min_cex = 0.05,
#     pointsize = 10,
#     dpi = 300,
#     parallel = TRUE,
#     resume = FALSE,
#     horizon_estimation_method = "terra"
#   )

#   # Test with terra method and parallel = FALSE (sequential)
#   tmp_dir_seq <- withr::local_tempdir()

#   result_sequential <- gla_create_fisheye_photos(
#     points = test_points,
#     dem_path = dem_path,
#     output_dir = tmp_dir_seq,
#     cam_ht = 1.37,
#     min_dist = 1,
#     max_dist = 220,
#     img_res = 2800,
#     max_cex = 0.2,
#     min_cex = 0.05,
#     pointsize = 10,
#     dpi = 300,
#     parallel = FALSE,
#     resume = FALSE,
#     horizon_estimation_method = "terra"
#   )

#   # Both should succeed and return sf objects
#   expect_s3_class(result_parallel, "sf")
#   expect_s3_class(result_sequential, "sf")

#   # Both should have fisheye_photo_path column
#   expect_true("fisheye_photo_path" %in% names(result_parallel))
#   expect_true("fisheye_photo_path" %in% names(result_sequential))

#   # Both should create the same number of photos
#   expect_equal(nrow(result_parallel), nrow(result_sequential))

#   # Photos should exist
#   expect_true(all(file.exists(result_parallel$fisheye_photo_path)))
#   expect_true(all(file.exists(result_sequential$fisheye_photo_path)))
# })
