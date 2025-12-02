test_that("End-to-end terra workflow completes successfully", {
  skip_on_macos_ci()

  # Create test fixtures on-demand
  dem_path <- withr::local_tempfile(fileext = ".tif")
  create_test_dem(crs = 3005, output_path = dem_path)

  stream_network_path <- withr::local_tempfile(fileext = ".gpkg")
  create_test_points(crs = 3005, n_points = 2, output_path = stream_network_path)

  las_dir <- withr::local_tempdir()
  las_path <- file.path(las_dir, "minimal_plot_3005.las")
  create_test_las(crs = 3005, n_points = 100, output_path = las_path)

  # Create temporary output directories
  output_dir_virtual_plots <- withr::local_tempdir()
  output_dir_fisheye <- withr::local_tempdir()

  # Step 1: Load stream network
  stream_points <- gla_load_points(
    stream_network_path,
    dem_path
  )

  expect_s3_class(stream_points, "sf")
  expect_equal(nrow(stream_points), 2)
  expect_true(all(
    c("x_meters", "y_meters", "elevation", "lat", "lon") %in%
      names(stream_points)
  ))

  # Step 2: Create virtual plots (reduced radius for speed)
  # gla_create_virtual_plots expects a directory path
  stream_points <- gla_create_virtual_plots(
    points = stream_points,
    folder = las_dir,
    output_dir = output_dir_virtual_plots,
    plot_radius = 5, # Small radius for fast test
    chunk_size = 0,
    resume = FALSE
  )

  expect_true("las_files" %in% names(stream_points))
  # At least one point should have a LAS file (might be fewer if points outside coverage)
  las_files_exist <- !is.na(stream_points$las_files) &
    file.exists(stream_points$las_files)
  expect_true(any(las_files_exist), "No virtual plot LAS files were created")

  # Filter to only points with valid LAS files for rest of test
  stream_points <- stream_points[las_files_exist, ]
  expect_true(nrow(stream_points) > 0, "Need at least one point with LAS file")

  # Step 3: Extract horizon masks
  stream_points <- gla_extract_horizons(
    points = stream_points,
    dem_path = dem_path,
    output_dir = withr::local_tempdir(),
    step = 30, # Coarser resolution for speed
    max_search_distance = 1000, # Limit distance for speed
    verbose = FALSE
  )

  expect_true("horizon_mask" %in% names(stream_points))
  expect_equal(length(stream_points$horizon_mask), nrow(stream_points))

  # Step 4: Create fisheye photos (reduced resolution for speed)
  stream_points <- gla_create_fisheye_photos(
    points = stream_points,
    output_dir = output_dir_fisheye,
    cam_ht = 1.37,
    min_dist = 1,
    max_dist = 50, # Shorter distance for fast test
    img_res = 100, # Much smaller resolution for fast test
    max_cex = 0.2,
    min_cex = 0.05,
    pointsize = 10,
    dpi = 100, # Lower DPI for fast test
    parallel = FALSE, # Sequential for simplicity in test
    resume = FALSE
  )

  expect_true("fisheye_photo_path" %in% names(stream_points))
  expect_true(all(file.exists(stream_points$fisheye_photo_path)))

  # Step 4: Process fisheye photos for solar radiation (reduced time period)
  stream_points <- gla_process_fisheye_photos(
    points = stream_points,
    clearsky_coef = 0.65,
    time_step_min = 10, # Coarser time step for speed
    day_start = 213,
    day_end = 215, # Just 3 days for speed
    day_res = 1,
    elev_res = 10, # Coarser resolution for speed
    azi_res = 10,
    Kt = 0.45,
    parallel = FALSE
  )

  # Verify results have expected columns
  expected_cols <- c(
    "canopy_openness_pct",
    "mean_daily_extraterrestrial_irradiance_Wm2",
    "mean_daily_direct_irradiation_MJm2d",
    "mean_daily_diffuse_irradiation_MJm2d",
    "mean_daily_global_irradiation_MJm2d",
    "transmitted_direct_irradiation_MJm2d",
    "transmitted_diffuse_irradiation_MJm2d",
    "transmitted_global_irradiation_MJm2d",
    "transmitted_direct_irradiation_pct",
    "transmitted_diffuse_irradiation_pct",
    "transmitted_global_irradiation_pct",
    "subcanopy_solar_radiation_MJm2d",
    "light_penetration_index"
  )

  expect_true(all(expected_cols %in% names(stream_points)))

  # Verify results are numeric and within reasonable ranges
  expect_true(all(
    stream_points$canopy_openness_pct >= 0 &
      stream_points$canopy_openness_pct <= 100
  ))
  expect_true(all(
    stream_points$light_penetration_index >= 0 &
      stream_points$light_penetration_index <= 1
  ))
  expect_true(all(stream_points$transmitted_global_irradiation_pct >= 0))

  # Snapshot test for result values
  # Extract key columns for snapshot (exclude geometry and file paths)
  results_snapshot <- sf::st_drop_geometry(stream_points)
  results_snapshot <- results_snapshot[, c(
    "canopy_openness_pct",
    "mean_daily_extraterrestrial_irradiance_Wm2",
    "mean_daily_direct_irradiation_MJm2d",
    "mean_daily_diffuse_irradiation_MJm2d",
    "mean_daily_global_irradiation_MJm2d",
    "transmitted_direct_irradiation_MJm2d",
    "transmitted_diffuse_irradiation_MJm2d",
    "transmitted_global_irradiation_MJm2d",
    "transmitted_direct_irradiation_pct",
    "transmitted_diffuse_irradiation_pct",
    "transmitted_global_irradiation_pct",
    "subcanopy_solar_radiation_MJm2d",
    "light_penetration_index"
  )]
  results_snapshot <- as.data.frame(results_snapshot)

  expect_snapshot(results_snapshot)
})

test_that("End-to-end workflow with resume=TRUE skips existing files", {
  skip_on_cran()
  skip_on_macos_ci()

  # Create test fixtures on-demand
  dem_path <- withr::local_tempfile(fileext = ".tif")
  create_test_dem(crs = 3005, output_path = dem_path)

  stream_network_path <- withr::local_tempfile(fileext = ".gpkg")
  create_test_points(crs = 3005, n_points = 2, output_path = stream_network_path)

  las_dir <- withr::local_tempdir()
  las_path <- file.path(las_dir, "minimal_plot_3005.las")
  create_test_las(crs = 3005, n_points = 100, output_path = las_path)

  output_dir_virtual_plots <- withr::local_tempdir()
  output_dir_fisheye <- withr::local_tempdir()

  stream_points <- gla_load_points(
    stream_network_path,
    dem_path
  )

  # First run - create files
  stream_points <- gla_create_virtual_plots(
    points = stream_points,
    folder = las_dir,
    output_dir = output_dir_virtual_plots,
    plot_radius = 5,
    chunk_size = 0,
    resume = FALSE
  )

  las_files_exist <- !is.na(stream_points$las_files) &
    file.exists(stream_points$las_files)
  stream_points <- stream_points[las_files_exist, ]
  expect_true(nrow(stream_points) > 0)

  # Extract horizons once
  stream_points <- gla_extract_horizons(
    points = stream_points,
    dem_path = dem_path,
    output_dir = withr::local_tempdir(),
    step = 30,
    max_search_distance = 1000,
    verbose = FALSE
  )

  stream_points_first <- gla_create_fisheye_photos(
    points = stream_points,
    output_dir = output_dir_fisheye,
    cam_ht = 1.37,
    min_dist = 1,
    max_dist = 50,
    img_res = 100,
    max_cex = 0.2,
    min_cex = 0.05,
    pointsize = 10,
    dpi = 100,
    parallel = FALSE,
    resume = FALSE
  )

  first_run_files <- stream_points_first$fisheye_photo_path
  expect_true(all(file.exists(first_run_files)))

  # Get file modification times
  first_mtimes <- file.info(first_run_files)$mtime

  # Wait a moment to ensure different timestamps
  Sys.sleep(0.5)

  # Second run with resume=TRUE should skip existing files
  stream_points_second <- gla_create_fisheye_photos(
    points = stream_points,
    output_dir = output_dir_fisheye,
    cam_ht = 1.37,
    min_dist = 1,
    max_dist = 50,
    img_res = 100,
    max_cex = 0.2,
    min_cex = 0.05,
    pointsize = 10,
    dpi = 100,
    parallel = FALSE,
    resume = TRUE # Should skip existing
  )

  second_run_files <- stream_points_second$fisheye_photo_path
  second_mtimes <- file.info(second_run_files)$mtime

  # Files should not have been modified
  expect_equal(first_mtimes, second_mtimes)
})

test_that("End-to-end workflow works with 26912", {
  skip_on_cran()

  # Create test fixtures on-demand (EPSG:26912 - Montana/Idaho)
  dem_path <- withr::local_tempfile(fileext = ".tif")
  create_test_dem(crs = 26912, output_path = dem_path)

  stream_network_path <- withr::local_tempfile(fileext = ".gpkg")
  create_test_points(crs = 26912, n_points = 2, output_path = stream_network_path)

  las_dir <- withr::local_tempdir()
  las_path <- file.path(las_dir, "minimal_plot_26912.las")
  create_test_las(crs = 26912, n_points = 100, output_path = las_path)

  # Verify fixtures are in UTM Zone 12N
  dem_crs <- sf::st_crs(terra::rast(dem_path))
  expect_equal(dem_crs$epsg, 26912)

  stream_crs <- sf::st_crs(sf::read_sf(stream_network_path))
  expect_equal(stream_crs$epsg, 26912)

  # Create temporary output directories
  output_dir_virtual_plots <- withr::local_tempdir()
  output_dir_fisheye <- withr::local_tempdir()

  # Step 1: Load stream network
  stream_points <- gla_load_points(
    stream_network_path,
    dem_path
  )

  expect_s3_class(stream_points, "sf")
  expect_equal(nrow(stream_points), 2)
  expect_true(all(
    c("x_meters", "y_meters", "elevation", "lat", "lon") %in%
      names(stream_points)
  ))

  # Verify CRS is UTM Zone 12N
  expect_equal(sf::st_crs(stream_points)$epsg, 26912)

  # Step 2: Create virtual plots (reduced radius for speed)
  stream_points <- gla_create_virtual_plots(
    points = stream_points,
    folder = las_dir,
    output_dir = output_dir_virtual_plots,
    plot_radius = 5,
    chunk_size = 0,
    resume = FALSE
  )

  expect_true("las_files" %in% names(stream_points))
  las_files_exist <- !is.na(stream_points$las_files) &
    file.exists(stream_points$las_files)
  expect_true(any(las_files_exist), "No virtual plot LAS files were created")

  stream_points <- stream_points[las_files_exist, ]
  expect_true(nrow(stream_points) > 0, "Need at least one point with LAS file")

  # Step 3: Extract horizon masks
  stream_points <- gla_extract_horizons(
    points = stream_points,
    dem_path = dem_path,
    output_dir = withr::local_tempdir(),
    step = 30,
    max_search_distance = 1000,
    verbose = FALSE
  )

  expect_true("horizon_mask" %in% names(stream_points))
  expect_equal(length(stream_points$horizon_mask), nrow(stream_points))

  # Step 4: Create fisheye photos (reduced resolution for speed)
  stream_points <- gla_create_fisheye_photos(
    points = stream_points,
    output_dir = output_dir_fisheye,
    cam_ht = 1.37,
    min_dist = 1,
    max_dist = 50,
    img_res = 100,
    max_cex = 0.2,
    min_cex = 0.05,
    pointsize = 10,
    dpi = 100,
    parallel = FALSE,
    resume = FALSE
  )

  expect_true("fisheye_photo_path" %in% names(stream_points))
  expect_true(all(file.exists(stream_points$fisheye_photo_path)))

  # Step 5: Process fisheye photos for solar radiation (reduced time period)
  stream_points <- gla_process_fisheye_photos(
    points = stream_points,
    clearsky_coef = 0.65,
    time_step_min = 10,
    day_start = 213,
    day_end = 215,
    day_res = 1,
    elev_res = 10,
    azi_res = 10,
    Kt = 0.45,
    parallel = FALSE
  )

  # Verify results have expected columns
  expected_cols <- c(
    "canopy_openness_pct",
    "mean_daily_extraterrestrial_irradiance_Wm2",
    "mean_daily_direct_irradiation_MJm2d",
    "mean_daily_diffuse_irradiation_MJm2d",
    "mean_daily_global_irradiation_MJm2d",
    "transmitted_direct_irradiation_MJm2d",
    "transmitted_diffuse_irradiation_MJm2d",
    "transmitted_global_irradiation_MJm2d",
    "transmitted_direct_irradiation_pct",
    "transmitted_diffuse_irradiation_pct",
    "transmitted_global_irradiation_pct",
    "subcanopy_solar_radiation_MJm2d",
    "light_penetration_index"
  )

  expect_true(all(expected_cols %in% names(stream_points)))

  # Verify results are numeric and within reasonable ranges
  expect_true(all(
    stream_points$canopy_openness_pct >= 0 &
      stream_points$canopy_openness_pct <= 100
  ))
  expect_true(all(
    stream_points$light_penetration_index >= 0 &
      stream_points$light_penetration_index <= 1
  ))
  expect_true(all(stream_points$transmitted_global_irradiation_pct >= 0))
})
